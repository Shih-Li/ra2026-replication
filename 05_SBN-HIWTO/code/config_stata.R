# ============================================================================
# R configuration and shared helpers
# Parallel to: code/config_stata.do
# Surviving Bad News: Health Information Without Treatment Options
# ============================================================================

REPLICATION_HELPER_VERSION <- "2026-08-22-mechanism-debug-v2"

required_packages <- c(
  "haven", "dplyr", "tidyr", "stringr", "ggplot2",
  "sandwich", "lmtest", "MASS", "ivDiag"
)

install_missing <- function(pkgs = required_packages) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  invisible(missing)
}

install_missing()

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

`%||%` <- function(x, y) if (is.null(x)) y else x

solve_diagnostic <- function(x) {
  rc <- tryCatch(rcond(x), error = function(e) NA_real_)
  rk <- tryCatch(qr(x)$rank, error = function(e) NA_integer_)
  val <- tryCatch(solve(x), error = function(e) NULL)
  method <- "solve"
  if (is.null(val) || any(!is.finite(val))) {
    val <- MASS::ginv(x)
    method <- "ginv"
  }
  list(value = val, method = method, rcond = rc, rank = rk, n = nrow(x))
}

safe_solve <- function(x) solve_diagnostic(x)$value

as_num <- function(x) {
  if (inherits(x, "haven_labelled")) x <- unclass(x)
  suppressWarnings(as.numeric(x))
}

destring_force <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", ".")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
}

idx_true <- function(x) which(!is.na(x) & x)

stata_bool <- function(x) {
  # Stata logical comparisons without an if qualifier evaluate to 0/1.
  # For R expressions that become NA because the source value is missing,
  # map NA to FALSE unless the Stata statement explicitly conditioned on nonmissing.
  as.numeric(replace(x, is.na(x), FALSE))
}

rowtotal_stata <- function(df) {
  m <- as.matrix(df)
  out <- rowSums(m, na.rm = TRUE)
  out[rowSums(!is.na(m)) == 0] <- NA_real_
  out
}

rownonmiss_stata <- function(df) rowSums(!is.na(as.matrix(df)))

stata_merge_1to1 <- function(master, using, key) {
  # Stata merge keeps master values for same-named variables unless update/replace
  add <- setdiff(names(using), names(master))
  dplyr::left_join(master, using[, c(key, add), drop = FALSE], by = key)
}

add_factor_dummies <- function(data, var, prefix = var) {
  vals <- sort(unique(data[[var]][!is.na(data[[var]])]))
  if (length(vals) <= 1L) return(list(data = data, names = character()))
  base <- vals[1]
  names_out <- character()
  for (v in vals[-1]) {
    nm <- paste0(prefix, "_", make.names(as.character(v)))
    data[[nm]] <- as.numeric(!is.na(data[[var]]) & data[[var]] == v)
    names_out <- c(names_out, nm)
  }
  list(data = data, names = names_out, base = base)
}

make_model <- function(coef, vcov, nobs, sample, type, extra = list()) {
  se <- sqrt(pmax(diag(vcov), 0))
  stat <- coef / se
  p <- 2 * pnorm(abs(stat), lower.tail = FALSE)
  structure(list(
    coefficients = coef,
    vcov = vcov,
    se = se,
    statistic = stat,
    p.value = p,
    nobs = nobs,
    sample = sample,
    type = type,
    extra = extra
  ), class = "rep_model")
}

fit_ols_cluster <- function(data, y, rhs, cluster, weights = NULL) {
  vars <- unique(c(y, rhs, cluster, weights))
  ok <- complete.cases(data[, vars, drop = FALSE])
  if (!is.null(weights)) ok <- ok & data[[weights]] > 0
  dd <- data[ok, , drop = FALSE]
  if (!nrow(dd)) stop("No complete observations for ", y)
  f <- reformulate(rhs, response = y)
  fit <- if (is.null(weights)) lm(f, data = dd) else lm(f, data = dd, weights = dd[[weights]])
  G <- length(unique(dd[[cluster]]))
  V <- sandwich::vcovCL(fit, cluster = dd[[cluster]], type = "HC1")
  b <- coef(fit)
  se <- sqrt(diag(V))
  stat <- b / se
  p <- 2 * pt(abs(stat), df = max(G - 1, 1), lower.tail = FALSE)
  out <- make_model(b, V, nobs(fit), which(ok), "OLS")
  out$statistic <- stat
  out$p.value <- p
  out$fit <- fit
  out
}

fit_ordered_probit_cluster <- function(data, y, rhs, cluster) {
  vars <- unique(c(y, rhs, cluster))
  ok <- complete.cases(data[, vars, drop = FALSE])
  dd <- data[ok, , drop = FALSE]
  dd[[y]] <- ordered(dd[[y]])
  f <- reformulate(rhs, response = y)
  fit <- MASS::polr(f, data = dd, method = "probit", Hess = TRUE)
  Vfull <- tryCatch(
    sandwich::vcovCL(fit, cluster = dd[[cluster]], type = "HC1"),
    error = function(e) vcov(fit)
  )
  bfull <- coef(fit)
  V <- Vfull[names(bfull), names(bfull), drop = FALSE]
  out <- make_model(bfull, V, nrow(dd), which(ok), "Ordered probit")
  out$fit <- fit
  out
}

fit_gmm_iv <- function(data, y, exog, endog = character(), instruments = character(),
                       cluster, weights = NULL, small_sample = FALSE,
                       scale_instruments = TRUE) {
  vars <- unique(c(y, exog, endog, instruments, cluster, weights))
  ok <- complete.cases(data[, vars, drop = FALSE])
  if (!is.null(weights)) ok <- ok & data[[weights]] > 0 & is.finite(data[[weights]])
  dd <- data[ok, , drop = FALSE]
  if (!nrow(dd)) stop("No complete observations for ", y)
  
  # Drop exogenous columns that are constant in the estimation sample.
  exog_keep <- exog[vapply(exog, function(v) {
    z <- dd[[v]]
    length(unique(z[!is.na(z)])) > 1L
  }, logical(1))]
  
  X <- cbind(`(Intercept)` = 1, as.matrix(dd[, c(exog_keep, endog), drop = FALSE]))
  storage.mode(X) <- "double"
  colnames(X) <- c("(Intercept)", exog_keep, endog)
  
  Zraw <- cbind(`(Intercept)` = 1, as.matrix(dd[, c(exog_keep, instruments), drop = FALSE]))
  storage.mode(Zraw) <- "double"
  colnames(Zraw) <- c("(Intercept)", exog_keep, instruments)
  Z <- Zraw[, !duplicated(colnames(Zraw)), drop = FALSE]
  
  # Remove zero-variance excluded instruments in this subsample.
  keep_z <- c(TRUE, vapply(seq_len(ncol(Z) - 1L) + 1L, function(j) {
    length(unique(Z[, j])) > 1L
  }, logical(1)))
  Z <- Z[, keep_z, drop = FALSE]
  
  # Stata's ivregress gmm is invariant to nonsingular rescaling of the
  # instrument matrix.  Rescale columns before the matrix algebra to avoid
  # numerical problems when instruments have very different units (for
  # example incentive, incentive^2, distance, and distance^2).  This changes
  # only the numerical conditioning, not the instrument span or the GMM
  # estimand.  The intercept is intentionally left unchanged.
  if (scale_instruments && ncol(Z) > 1L) {
    z_scale <- vapply(seq_len(ncol(Z)), function(j) {
      if (colnames(Z)[j] == "(Intercept)") return(1)
      s <- sqrt(mean(Z[, j]^2))
      if (!is.finite(s) || s <= .Machine$double.eps) 1 else s
    }, numeric(1))
    Z <- sweep(Z, 2, z_scale, "/")
  }
  
  yv <- as.numeric(dd[[y]])
  w <- if (is.null(weights)) rep(1, nrow(dd)) else as.numeric(dd[[weights]])
  w <- w / mean(w)
  
  zw <- Z * w
  xw <- X * w
  Zy <- crossprod(Z, w * yv)
  ZX <- crossprod(Z, xw)
  ZZ <- crossprod(Z, zw)
  
  # Initial 2SLS estimator used to construct the cluster-robust optimal GMM
  # weight matrix. Keep this stage in the returned model as a diagnostic.
  inv_ZZ <- solve_diagnostic(ZZ)
  Pz <- inv_ZZ$value
  A0 <- t(ZX) %*% Pz %*% ZX
  inv_A0 <- solve_diagnostic(A0)
  A0_inv <- inv_A0$value
  b0 <- as.vector(A0_inv %*% (t(ZX) %*% Pz %*% Zy))
  names(b0) <- colnames(X)
  u0 <- yv - as.vector(X %*% b0)
  
  cl <- dd[[cluster]]
  groups <- split(seq_len(nrow(dd)), cl)
  qg <- lapply(groups, function(ii) colSums(Z[ii, , drop = FALSE] * (w[ii] * u0[ii])))
  Q <- do.call(rbind, qg)
  S <- crossprod(Q)
  
  # Cluster-robust covariance for the initial 2SLS estimator. Diagnostic only.
  middle0 <- t(ZX) %*% Pz %*% S %*% Pz %*% ZX
  V0 <- A0_inv %*% middle0 %*% A0_inv
  dimnames(V0) <- list(names(b0), names(b0))
  
  inv_S <- solve_diagnostic(S)
  W <- inv_S$value
  
  A <- t(ZX) %*% W %*% ZX
  inv_A <- solve_diagnostic(A)
  b <- as.vector(inv_A$value %*% (t(ZX) %*% W %*% Zy))
  names(b) <- colnames(X)
  
  # Recompute the clustered moment covariance at the efficient GMM estimate.
  u <- yv - as.vector(X %*% b)
  qg2 <- lapply(groups, function(ii) colSums(Z[ii, , drop = FALSE] * (w[ii] * u[ii])))
  Q2 <- do.call(rbind, qg2)
  S2 <- crossprod(Q2)
  bread <- inv_A$value
  middle <- t(ZX) %*% W %*% S2 %*% W %*% ZX
  V <- bread %*% middle %*% bread
  dimnames(V) <- list(names(b), names(b))
  
  # Stata ivregress gmm reports large-sample z statistics by default.
  # Its finite-sample VCE adjustment is applied only when the Stata option
  # `small` is explicitly requested.  None of the authors' ivregress gmm
  # calls in this replication use `small`, so the default here is FALSE.
  if (small_sample) {
    G <- length(groups)
    n <- nrow(dd)
    k <- ncol(X)
    if (G > 1 && n > k) V <- V * (n * G) / ((n - k) * (G - 1))
  }
  
  out <- make_model(b, V, nrow(dd), which(ok), "Clustered linear GMM")
  out$residuals <- u
  out$cluster_count <- length(groups)
  out$exog <- exog_keep
  out$endog <- endog
  out$instruments <- colnames(Z)
  
  # Initial 2SLS stage retained for replication diagnostics.
  out$initial_coefficients <- b0
  out$initial_vcov <- V0
  out$initial_se <- setNames(sqrt(pmax(diag(V0), 0)), names(b0))
  out$diagnostics <- list(
    helper_version = REPLICATION_HELPER_VERSION,
    n = nrow(dd),
    clusters = length(groups),
    k_x = ncol(X),
    k_z = ncol(Z),
    rank_x = qr(X)$rank,
    rank_z = qr(Z)$rank,
    inv_ZZ = inv_ZZ[c("method", "rcond", "rank", "n")],
    inv_A0 = inv_A0[c("method", "rcond", "rank", "n")],
    inv_S = inv_S[c("method", "rcond", "rank", "n")],
    inv_A = inv_A[c("method", "rcond", "rank", "n")]
  )
  
  out
}

coef_of <- function(model, term) unname(model$coefficients[term])
se_of <- function(model, term) unname(model$se[term])
p_of <- function(model, term) unname(model$p.value[term])

attach_stats <- function(model, ...) {
  model$extra <- modifyList(model$extra %||% list(), list(...))
  model
}

stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))
}

tex_escape <- function(x) {
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

write_models_tex <- function(models, file, keep = NULL, title = NULL,
                             digits = 3, stars_on = TRUE, extra_stats = NULL) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (is.null(keep)) keep <- unique(unlist(lapply(models, function(m) names(m$coefficients))))
  con <- file(file, open = "wt")
  on.exit(close(con), add = TRUE)
  nmod <- length(models)
  cat("% R replication output\n", file = con)
  if (!is.null(title)) cat("% ", title, "\n", sep = "", file = con)
  cat("\\begin{tabular}{l", paste(rep("c", nmod), collapse = ""), "}\n", sep = "", file = con)
  cat("\\hline\n", file = con)
  for (term in keep) {
    bline <- character(nmod)
    seline <- character(nmod)
    for (j in seq_along(models)) {
      m <- models[[j]]
      if (term %in% names(m$coefficients)) {
        b <- m$coefficients[[term]]
        p <- m$p.value[[term]]
        ss <- if (stars_on) stars(p) else ""
        bline[j] <- paste0(formatC(b, format = "f", digits = digits), ss)
        seline[j] <- paste0("(", formatC(m$se[[term]], format = "f", digits = digits), ")")
      } else {
        bline[j] <- ""
        seline[j] <- ""
      }
    }
    cat(tex_escape(term), " & ", paste(bline, collapse = " & "), " \\\\\n", sep = "", file = con)
    cat(" & ", paste(seline, collapse = " & "), " \\\\\n", sep = "", file = con)
  }
  cat("\\hline\n", file = con)
  cat("N & ", paste(vapply(models, function(m) as.character(m$nobs), character(1)), collapse = " & "), " \\\\\n", sep = "", file = con)
  if (!is.null(extra_stats)) {
    for (st in extra_stats) {
      vals <- vapply(models, function(m) {
        v <- m$extra[[st]]
        if (is.null(v) || is.na(v)) "" else formatC(v, format = "f", digits = digits)
      }, character(1))
      cat(tex_escape(st), " & ", paste(vals, collapse = " & "), " \\\\\n", sep = "", file = con)
    }
  }
  cat("\\hline\n\\end{tabular}\n", file = con)
  invisible(file)
}

write_summary_tex <- function(data, vars, file, groups = list(All = rep(TRUE, nrow(data))),
                              include_sd_n = FALSE, digits = 3) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  con <- file(file, "wt")
  on.exit(close(con), add = TRUE)
  cat("% R replication summary statistics\n", file = con)
  cat("\\begin{tabular}{l", paste(rep("c", length(groups)), collapse = ""), "}\n\\hline\n", sep = "", file = con)
  cat("Variable & ", paste(names(groups), collapse = " & "), " \\\\\n", sep = "", file = con)
  for (v in vars) {
    vals <- vapply(groups, function(idx) {
      x <- data[[v]][idx]
      if (include_sd_n) {
        paste0(formatC(mean(x, na.rm = TRUE), format = "f", digits = digits),
               " [", formatC(sd(x, na.rm = TRUE), format = "f", digits = digits),
               "; ", sum(!is.na(x)), "]")
      } else formatC(mean(x, na.rm = TRUE), format = "f", digits = digits)
    }, character(1))
    cat(tex_escape(v), " & ", paste(vals, collapse = " & "), " \\\\\n", sep = "", file = con)
  }
  cat("\\hline\n\\end{tabular}\n", file = con)
}

write_matrix_tex <- function(mat, file, row_names = NULL, digits = 3) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  mat <- as.matrix(mat)
  con <- file(file, "wt")
  on.exit(close(con), add = TRUE)
  cat("% R replication matrix output\n", file = con)
  cat("\\begin{tabular}{l", paste(rep("c", ncol(mat)), collapse = ""), "}\n\\hline\n", sep = "", file = con)
  for (i in seq_len(nrow(mat))) {
    rn <- if (is.null(row_names)) as.character(i) else row_names[i]
    vals <- vapply(mat[i, ], function(z) if (is.na(z)) "" else formatC(z, format = "f", digits = digits), character(1))
    cat(tex_escape(rn), " & ", paste(vals, collapse = " & "), " \\\\\n", sep = "", file = con)
  }
  cat("\\hline\n\\end{tabular}\n", file = con)
}

effective_f <- function(data, y, endog, instruments, controls, cluster) {
  # Match Stata/ivreg2 behavior: silently omit regressors/instruments that are
  # constant in the estimation sample (e.g. `hiv` after restricting to hiv==0
  # or hiv==1 for Table A.7). Passing such columns to ivDiag can make its
  # Cholesky step rank-deficient even though Stata simply drops them.
  vars <- unique(c(y, endog, instruments, controls, cluster))
  dd <- data[complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  
  varies <- function(v) {
    x <- dd[[v]]
    length(unique(x[!is.na(x)])) > 1L
  }
  controls_keep <- controls[vapply(controls, varies, logical(1))]
  instruments_keep <- instruments[vapply(instruments, varies, logical(1))]
  
  if (!nrow(dd) || !length(instruments_keep)) return(NA_real_)
  
  tryCatch(
    as.numeric(ivDiag::eff_F(
      data = dd, Y = y, D = endog, Z = instruments_keep,
      controls = controls_keep, cl = cluster, prec = 8
    )),
    error = function(e) {
      warning("ivDiag effective F failed for ", y, "/", endog, ": ", conditionMessage(e))
      NA_real_
    }
  )
}

simes_qvalue <- function(p) {
  # qqvalue, method(simes) is analogous to the Simes/BH step-up adjustment.
  p.adjust(p, method = "BH")
}

run_selection_robustness <- function(data, year, j_max, l_max, figure_file,
                                     benchmark_neg, benchmark_pos) {
  y <- paste0("alive", year)
  demog <- c("hiv", "male", "age", "age2", "south", "north")
  instrument2 <- c("anyincentive", "incentive", "incentive2", "distvct", "distvct2",
                   "anyincentivehiv", "incentivehiv", "incentive2hiv", "distvcthiv", "distvct2hiv")
  
  data$missing_living <- as.numeric(is.na(data[[y]]))
  observed_neg <- data$hiv == 0 & !is.na(data[[y]])
  mortality_rate <- mean(data[[y]][observed_neg] == 0)
  n_missing_neg <- sum(data$missing_living == 1 & data$hiv == 0, na.rm = TRUE)
  nb_dead <- round(n_missing_neg * mortality_rate)
  
  result <- expand.grid(j = 0:j_max, l = 0:l_max)
  result$pos <- NA_real_
  result$neg <- NA_real_
  
  for (r in seq_len(nrow(result))) {
    j <- result$j[r]
    l <- result$l[r]
    bpos <- bneg <- rep(NA_real_, 100)
    for (i in 1:100) {
      # Stata concatenates i, j and l in `set seed`; R uses the same numeric seed
      # but a different RNG, so simulation draws are distributionally equivalent,
      # not bit-for-bit identical to Stata.
      set.seed(as.integer(paste0(i, j, l)))
      yy <- data[[y]]
      
      idx_neg <- which(data$missing_living == 1 & data$hiv == 0)
      if (length(idx_neg)) {
        dead_idx <- if (nb_dead > 0) sample(idx_neg, min(nb_dead, length(idx_neg))) else integer()
        yy[idx_neg] <- 1
        yy[dead_idx] <- 0
      }
      
      idx_pos_no <- which(data$missing_living == 1 & data$hiv == 1 & data$posttest == 0)
      if (length(idx_pos_no)) {
        dead_idx <- if (j > 0) sample(idx_pos_no, min(j, length(idx_pos_no))) else integer()
        yy[idx_pos_no] <- 1
        yy[dead_idx] <- 0
      }
      
      idx_pos_yes <- which(data$missing_living == 1 & data$hiv == 1 & data$posttest == 1)
      if (length(idx_pos_yes)) {
        dead_idx <- if (l > 0) sample(idx_pos_yes, min(l, length(idx_pos_yes))) else integer()
        yy[idx_pos_yes] <- 1
        yy[dead_idx] <- 0
      }
      
      tmp <- data
      tmp$.sim_alive <- yy
      mod <- fit_gmm_iv(tmp, ".sim_alive", demog,
                        c("learnhivneg", "learnhivpos"), instrument2,
                        "DK_village_number")
      bpos[i] <- coef_of(mod, "learnhivpos")
      bneg[i] <- coef_of(mod, "learnhivneg")
    }
    result$pos[r] <- mean(bpos, na.rm = TRUE)
    result$neg[r] <- mean(bneg, na.rm = TRUE)
  }
  
  plotdat <- tidyr::pivot_longer(result, c(pos, neg), names_to = "status", values_to = "estimate")
  p <- ggplot(plotdat, aes(x = j, y = estimate, shape = factor(l), group = interaction(status, l))) +
    geom_point(aes(alpha = status), size = 1.8) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_hline(yintercept = benchmark_neg) +
    geom_hline(yintercept = benchmark_pos) +
    scale_alpha_manual(values = c(neg = .45, pos = 1), guide = "none") +
    # ggplot2's default discrete shape scale supports only six levels. Figure
    # A.4 has l = 0,...,9 (10 levels), so specify valid point shapes manually;
    # otherwise levels 6-9 receive NA shapes and 104 points are dropped.
    scale_shape_manual(values = seq.int(0, l_max)) +
    labs(
      x = paste0("Number of HIV+ persons who did not learn about their status and are dead (out of ", j_max, ")"),
      y = NULL,
      shape = paste0("Deaths among HIV+ persons who learned status (out of ", l_max, ")")
    ) +
    theme_bw()
  dir.create(dirname(figure_file), recursive = TRUE, showWarnings = FALSE)
  ggsave(figure_file, p, width = 7.5, height = 5.5)
  invisible(result)
}
