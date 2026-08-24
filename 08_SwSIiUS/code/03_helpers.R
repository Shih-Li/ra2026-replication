# 03_helpers.R
# Shared utilities, Stata dsregress translation, table writers, and conditional-logit helper.
#
# IMPORTANT IMPLEMENTATION NOTE
# -----------------------------
# Stata 17 dsregress uses post-double-selection lasso. The helper below reproduces that
# architecture (partial out always-included controls; plugin-lambda lasso of the outcome
# and each variable of interest; union selected controls; final OLS with clustered SEs).
# The exact internal cluster-aware plugin loadings used by Stata are not public API, so
# selected controls can differ slightly from Stata. This is a documented implementation
# difference; final estimation always uses the original cluster variable.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

result_path <- function(name, type = c("table", "figure")) {
  type <- match.arg(type)
  dir <- if (type == "table") paths$tables else paths$figures
  file.path(dir, paste0(paths$result_prefix, name))
}

# Resolve the cluster identifier used throughout the original Stata analysis.
# The official do-file uses `s0d_dclusterid`. Some redistributed/converted .dta files
# can arrive with a case/punctuation variant, so normalize only when the match is unique.
canonicalize_cluster_name <- function(data, source_file = NULL) {
  canonical <- "s0d_dclusterid"
  nms <- names(data)
  if (canonical %in% nms) return(data)
  
  rename_unique <- function(idx, reason) {
    if (length(idx) != 1L) return(NULL)
    old <- nms[[idx]]
    names(data)[idx] <- canonical
    message("[data] Using `", old, "` as `", canonical, "` (", reason, ").")
    data
  }
  
  # 1) Exact name ignoring case.
  hit <- which(tolower(nms) == canonical)
  out <- rename_unique(hit, "case-insensitive match")
  if (!is.null(out)) return(out)
  
  # 2) Exact normalized spelling (ignore punctuation/underscores/case).
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  hit <- which(norm(nms) == norm(canonical))
  out <- rename_unique(hit, "normalized-name match")
  if (!is.null(out)) return(out)
  
  # 3) Conservative known aliases seen in Stata/R conversions.
  aliases <- c(
    "s0d_clusterid", "s0d_cluster_id", "s0d_dcluster_id",
    "dclusterid", "dcluster_id", "dcluster"
  )
  hit <- which(tolower(nms) %in% aliases)
  out <- rename_unique(hit, "known cluster-ID alias")
  if (!is.null(out)) return(out)
  
  # 4) Variable label can identify the field even if its imported name changed.
  labs <- vapply(data, function(x) {
    z <- attr(x, "label", exact = TRUE)
    if (is.null(z) || length(z) == 0L || is.na(z[[1]])) "" else as.character(z[[1]])
  }, character(1))
  label_hit <- which(grepl("cluster", labs, ignore.case = TRUE) & grepl("id|ident", labs, ignore.case = TRUE))
  out <- rename_unique(label_hit, "unique variable-label match")
  if (!is.null(out)) return(out)
  
  # 5) Last safe automatic option: exactly one cluster-ID-looking variable name.
  cand <- which(grepl("dcluster|cluster.*id|cluster_id", nms, ignore.case = TRUE))
  out <- rename_unique(cand, "unique cluster-ID-looking name")
  if (!is.null(out)) return(out)
  
  src <- if (is.null(source_file)) "this dataset" else basename(source_file)
  candidate_names <- nms[grepl("cluster|arrond", nms, ignore.case = TRUE)]
  candidate_text <- if (length(candidate_names)) {
    paste(candidate_names, collapse = ", ")
  } else {
    "<none>"
  }
  stop(
    src, " does not contain the original cluster variable `", canonical, "`.
",
    "Cluster/arrondissement-looking columns found: ", candidate_text, "
",
"This variable is required because the authors use it for both absorption and clustered SEs. ",
"Do not substitute Arrondissement_categorical. Check that the file is the official ",
"FinalData_* dataset from 04_CleanedData."
  )
}

read_source <- function(key) {
  f <- paths$data[[key]]
  if (is.null(f)) stop("Unknown source-data key: ", key)
  if (!file.exists(f)) stop("Missing required input: ", f)
  x <- haven::read_dta(f)
  canonicalize_cluster_name(x, f)
}

save_state <- function(x) saveRDS(x, paths$state)
load_state <- function() if (file.exists(paths$state)) readRDS(paths$state) else list()
state_put <- function(key, value) {
  s <- load_state(); s[[key]] <- value; save_state(s); invisible(value)
}
state_get <- function(key, required = TRUE) {
  s <- load_state()
  if (!key %in% names(s)) {
    if (required) stop("Missing analysis state '", key, "'. Run earlier numbered scripts first.")
    return(NULL)
  }
  s[[key]]
}

existing_vars <- function(data, vars) unique(vars[vars %in% names(data)])
require_vars <- function(data, vars, context = "analysis") {
  missing <- setdiff(vars, names(data))
  if (length(missing)) stop(context, " is missing variable(s): ", paste(missing, collapse = ", "))
  invisible(TRUE)
}
miss_vars <- function(data) grep("^miss_", names(data), value = TRUE)
fe_arrondissement_vars <- function(data) grep("^fe_arrondissement", names(data), value = TRUE)

bt <- function(x) paste0("`", gsub("`", "", x), "`")
terms_rhs <- function(x) if (length(x)) paste(bt(x), collapse = " + ") else "1"

model_sample <- function(fit) fit$data
coef_se <- function(model, term) {
  b <- stats::coef(model)
  if (!term %in% names(b)) return(c(estimate = NA_real_, se = NA_real_))
  V <- stats::vcov(model)
  c(estimate = unname(b[[term]]), se = sqrt(unname(V[term, term])))
}

# Stata's test command after vce(cluster ...) reports an F statistic with
# denominator df equal to (# clusters - 1).  Use that form when df2 is supplied;
# otherwise retain the large-sample chi-square form for nonclustered uses.
wald_p <- function(model, terms, df2 = NULL) {
  terms <- intersect(terms, names(stats::coef(model)))
  if (!length(terms)) return(NA_real_)
  V <- stats::vcov(model)[terms, terms, drop = FALSE]
  b <- stats::coef(model)[terms]
  keep <- is.finite(b) & is.finite(diag(V))
  b <- b[keep]; V <- V[keep, keep, drop = FALSE]
  q <- length(b)
  if (!q) return(NA_real_)
  
  chi2 <- as.numeric(t(b) %*% qr.solve(V, b))
  if (!is.null(df2) && is.finite(df2) && df2 > 0) {
    return(stats::pf(chi2 / q, df1 = q, df2 = df2, lower.tail = FALSE))
  }
  stats::pchisq(chi2, df = q, lower.tail = FALSE)
}

cluster_df2 <- function(data, cluster = "s0d_dclusterid") {
  require_vars(data, cluster, "cluster df")
  length(unique(data[[cluster]][!is.na(data[[cluster]])])) - 1L
}

# Match Stata's finite-sample clustered VCE more closely.  For areg with the
# absorbed effect equal to the clustering variable, Stata counts the absorbed
# group effects in K. fixest's default K.fixef='nonnested' does not, so Table 1
# and G1 need K.fixef='full'. Other specifications keep the nonnested rule.
stata_cluster_ssc <- function(count_absorbed_fe = FALSE) {
  fixest::ssc(
    K.adj = TRUE,
    K.fixef = if (count_absorbed_fe) "full" else "nonnested",
    K.exact = FALSE,
    G.adj = TRUE,
    G.df = "min",
    t.df = "min"
  )
}

# ---------- Stata-style plugin lasso approximation ----------
normalize_lasso_weights <- function(w) {
  w <- as.numeric(w)
  if (any(!is.finite(w)) || any(w <= 0)) stop("Invalid lasso observation weights.")
  w * length(w) / sum(w)
}

cluster_objective_weights <- function(cluster) {
  # Stata's clustered lasso objective gives each cluster equal total weight:
  # (1/G) sum_g (1/T_g) sum_t loss_gt.
  z <- as.character(cluster)
  if (anyNA(z)) stop("Cluster variable contains missing values in lasso sample.")
  sz <- table(z)
  normalize_lasso_weights(1 / as.numeric(sz[z]))
}

weighted_center_scale <- function(X, weights = NULL) {
  X <- as.matrix(X)
  if (is.null(weights)) weights <- rep(1, nrow(X))
  w <- normalize_lasso_weights(weights)
  sw <- sum(w)
  mu <- colSums(X * w) / sw
  Xc <- sweep(X, 2, mu, "-")
  sdv <- sqrt(colSums((Xc^2) * w) / sw)
  keep <- is.finite(sdv) & sdv > 1e-10
  list(
    X = sweep(Xc[, keep, drop = FALSE], 2, sdv[keep], "/"),
    keep = keep,
    center = mu[keep],
    scale = sdv[keep],
    weights = w
  )
}

weighted_residualize_on <- function(Y, A, weights = NULL) {
  Y <- as.matrix(Y)
  if (is.null(A) || ncol(A) == 0L) return(Y)
  A <- as.matrix(A)
  if (is.null(weights)) weights <- rep(1, nrow(A))
  w <- normalize_lasso_weights(weights)
  
  out <- matrix(NA_real_, nrow(Y), ncol(Y))
  for (j in seq_len(ncol(Y))) {
    out[, j] <- stats::lm.wfit(x = A, y = Y[, j], w = w)$residuals
  }
  colnames(out) <- colnames(Y)
  out
}

cluster_penalty_loadings <- function(X, eps, cluster, selected_count = 0L) {
  # Cluster-score loading: each row below is the mean score contribution for
  # one cluster, consistent with Stata's equal-cluster lasso objective.
  g <- match(as.character(cluster), unique(as.character(cluster)))
  G <- max(g)
  sizes <- tabulate(g, nbins = G)
  score_sums <- rowsum(X * as.numeric(eps), group = g, reorder = FALSE)
  score_means <- sweep(score_sums, 1, sizes, "/")
  denom <- max(G - selected_count, 1L)
  sqrt(colSums(score_means^2) / denom)
}

observation_penalty_loadings <- function(X, eps, selected_count = 0L) {
  denom <- max(nrow(X) - selected_count, 1L)
  sqrt(colSums((X * as.numeric(eps))^2) / denom)
}

post_lasso_residuals <- function(y, X, selected, weights) {
  if (!length(selected)) return(y)
  stats::lm.wfit(
    x = cbind(1, X[, selected, drop = FALSE]),
    y = y,
    w = weights
  )$residuals
}

plugin_lasso_select <- function(y, X, cluster = NULL, max_iter = 15L, tol = 1e-8) {
  if (!ncol(X)) return(character())
  ok <- is.finite(y) & apply(X, 1, function(z) all(is.finite(z)))
  if (!is.null(cluster)) ok <- ok & !is.na(cluster)
  y <- as.numeric(y[ok]); X <- as.matrix(X[ok, , drop = FALSE])
  if (!is.null(cluster)) cluster <- cluster[ok]
  if (nrow(X) < 5L || ncol(X) == 0L) return(character())
  
  clustered <- !is.null(cluster)
  obs_w <- if (clustered) cluster_objective_weights(cluster) else rep(1, nrow(X))
  sc <- weighted_center_scale(X, obs_w)
  Xs <- sc$X
  if (!ncol(Xs)) return(character())
  xnames <- colnames(X)[sc$keep]
  obs_w <- sc$weights
  
  ybar <- sum(obs_w * y) / sum(obs_w)
  y <- y - ybar
  
  # Stata plugin initialization: residuals from the five covariates with the
  # highest (weighted) univariate correlations with y.
  corv <- abs(colSums((Xs * y) * obs_w) / sum(obs_w))
  init <- order(corv, decreasing = TRUE)[seq_len(min(5L, length(corv)))]
  eps <- post_lasso_residuals(y, Xs, init, obs_w)
  
  p <- ncol(Xs)
  effective_n <- if (clustered) length(unique(as.character(cluster))) else nrow(Xs)
  gamma <- 0.1 / log(max(p, effective_n, 3L))
  lambda_base <- 1.1 / sqrt(effective_n) * stats::qnorm(1 - gamma / (2 * p))
  
  load_fun <- function(e, s = 0L) {
    if (clustered) {
      cluster_penalty_loadings(Xs, e, cluster, selected_count = s)
    } else {
      observation_penalty_loadings(Xs, e, selected_count = s)
    }
  }
  
  load <- load_fun(eps, 0L)
  load[!is.finite(load) | load < 1e-8] <- 1e-8
  last_load <- rep(Inf, p)
  selected <- integer()
  
  for (iter in seq_len(max_iter)) {
    pf <- load / mean(load)
    lambda_glmnet <- lambda_base * mean(load)
    
    gfit <- glmnet::glmnet(
      x = Xs,
      y = y,
      weights = obs_w,
      alpha = 1,
      lambda = lambda_glmnet,
      standardize = FALSE,
      intercept = FALSE,
      penalty.factor = pf,
      thresh = 1e-10
    )
    b <- as.matrix(stats::coef(gfit))[-1, 1]
    selected <- which(abs(b) > 1e-10)
    
    eps <- post_lasso_residuals(y, Xs, selected, obs_w)
    new_load <- load_fun(eps, length(selected))
    new_load[!is.finite(new_load) | new_load < 1e-8] <- 1e-8
    
    if (max(abs(new_load - load), na.rm = TRUE) < tol) {
      load <- new_load
      break
    }
    last_load <- load
    load <- new_load
  }
  
  xnames[selected]
}

always_matrix <- function(data, always = character(), fe = NULL) {
  always <- existing_vars(data, always)
  parts <- character()
  if (!is.null(fe)) parts <- c(parts, paste0("factor(", bt(fe), ")"))
  if (length(always)) parts <- c(parts, bt(always))
  if (!length(parts)) return(matrix(numeric(0), nrow(data), 0))
  f <- stats::as.formula(paste("~", paste(parts, collapse = " + ")))
  stats::model.matrix(f, data = data)
}

# Translation of Stata: dsregress y interest..., controls((always...) candidates...) vce(cluster cl)
#
# Important: Stata 17 makes *each lasso* cluster-aware when vce(cluster ...) is
# requested.  We therefore use equal-cluster objective weights and cluster-score
# plugin loadings in the selection stage, then estimate the final selected model
# by ordinary FE-OLS with cluster-robust VCE, as dsregress does.
dsregress_r <- function(data, y, interest, always = character(), candidates = character(),
                        fe = "Arrondissement_categorical", cluster = "s0d_dclusterid",
                        subset = NULL) {
  require_vars(data, c(y, interest, fe, cluster), paste0("dsregress ", y))
  always <- existing_vars(data, always)
  candidates <- existing_vars(data, candidates)
  candidates <- setdiff(candidates, c(y, interest, always, fe, cluster))
  
  if (is.null(subset)) subset <- rep(TRUE, nrow(data))
  subset[is.na(subset)] <- FALSE
  vars_for_cc <- unique(c(y, interest, always, candidates, fe, cluster))
  cc <- stats::complete.cases(data[, vars_for_cc, drop = FALSE])
  idx <- which(subset & cc)
  if (!length(idx)) stop("No estimation sample for ", y)
  d <- data[idx, , drop = FALSE]
  
  candidates <- candidates[vapply(candidates, function(v) {
    z <- d[[v]]
    is.numeric(z) && is.finite(stats::sd(z)) && stats::sd(z) > 1e-10
  }, logical(1))]
  
  selected <- character()
  if (length(candidates)) {
    lasso_w <- cluster_objective_weights(d[[cluster]])
    A <- always_matrix(d, always, fe)
    X <- as.matrix(d[, candidates, drop = FALSE])
    Xr <- weighted_residualize_on(X, A, lasso_w)
    colnames(Xr) <- candidates
    
    targets <- c(y, interest)
    for (target in targets) {
      tr <- weighted_residualize_on(matrix(d[[target]], ncol = 1), A, lasso_w)[, 1]
      selected <- union(
        selected,
        plugin_lasso_select(tr, Xr, cluster = d[[cluster]])
      )
    }
  }
  
  rhs <- unique(c(interest, always, selected))
  fml <- stats::as.formula(
    paste0(bt(y), " ~ ", terms_rhs(rhs), " | ", bt(fe))
  )
  cl_fml <- stats::as.formula(paste0("~", bt(cluster)))
  model <- fixest::feols(
    fml,
    data = d,
    cluster = cl_fml,
    ssc = stata_cluster_ssc(FALSE),
    warn = FALSE,
    notes = FALSE
  )
  
  list(model = model, selected = selected, data = d, original_rows = idx)
}

# ---------- Ordinary/fixed-effect regression helpers ----------
fit_feols <- function(data, y, rhs, fe = NULL, cluster = "s0d_dclusterid", subset = NULL,
                      count_absorbed_fe = FALSE) {
  require_vars(data, c(y, rhs, cluster, fe %||% character()), paste0("regression ", y))
  if (is.null(subset)) subset <- rep(TRUE, nrow(data))
  subset[is.na(subset)] <- FALSE
  vars <- unique(c(y, rhs, fe %||% character(), cluster))
  d <- data[subset & stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  ftxt <- paste0(bt(y), " ~ ", terms_rhs(rhs))
  if (!is.null(fe)) ftxt <- paste0(ftxt, " | ", bt(fe))
  model <- fixest::feols(
    stats::as.formula(ftxt), data = d,
    cluster = stats::as.formula(paste0("~", bt(cluster))),
    ssc = stata_cluster_ssc(count_absorbed_fe),
    warn = FALSE, notes = FALSE
  )
  list(model = model, data = d)
}

# ---------- Table output ----------
fmt_num <- function(x, digits = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
fmt_mean_sd <- function(mean, sd, digits = 3) {
  if (is.na(mean) && is.na(sd)) return("")
  paste0(fmt_num(mean, digits), " (", fmt_num(sd, digits), ")")
}
fmt_est_se <- function(est, se, digits = 3) {
  if (is.na(est) && is.na(se)) return("")
  if (is.na(se)) return(fmt_num(est, digits))
  paste0(fmt_num(est, digits), " (", fmt_num(se, digits), ")")
}
tex_escape_label <- function(x) x

write_models_tex <- function(models, file, terms, labels = terms, titles = names(models),
                             means = NULL, digits = 3, n_label = "$N$",
                             mean_label = "Outcome mean, comparison group") {
  if (is.null(titles) || any(!nzchar(titles))) titles <- paste0("(", seq_along(models), ")")
  k <- length(models)
  lines <- c(
    paste0("\\begin{tabular}{l", paste(rep("c", k), collapse = ""), "}"),
    "\\hline\\hline",
    paste(c("", titles), collapse = " & ") |> paste0(" \\\\"),
    "\\hline"
  )
  for (i in seq_along(terms)) {
    vals <- lapply(models, function(m) coef_se(m, terms[[i]]))
    est <- vapply(vals, `[[`, numeric(1), "estimate")
    se <- vapply(vals, `[[`, numeric(1), "se")
    lines <- c(
      lines,
      paste(c(labels[[i]], fmt_num(est, digits)), collapse = " & ") |> paste0(" \\\\"),
      paste(c("", paste0("(", fmt_num(se, digits), ")")), collapse = " & ") |> paste0(" \\\\")
    )
  }
  Ns <- vapply(models, stats::nobs, numeric(1))
  lines <- c(lines, "\\hline", paste(c(n_label, Ns), collapse = " & ") |> paste0(" \\\\"))
  if (!is.null(means)) {
    lines <- c(lines, paste(c(mean_label, fmt_num(means, digits)), collapse = " & ") |> paste0(" \\\\"))
  }
  lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(lines, file)
}

write_balance_tex <- function(left, right, file, panel_title = NULL) {
  # left/right: data frames with columns label, mean, sd, b1,se1,b2,se2,pjoint,N
  stopifnot(nrow(left) == nrow(right))
  lines <- c(
    "\\begin{tabular}{lccccccccc}", "\\hline\\hline",
    " & \\multicolumn{4}{c}{Treatments} & \\multicolumn{4}{c}{High-subsidy households in cluster} & Obs. \\\\",
    "Variable & Mean (SD) & T1 & T2 & joint $p$ & Mean (SD) & T1 & T2 & joint $p$ & N \\\\",
    "\\hline"
  )
  if (!is.null(panel_title)) lines <- c(lines, paste0("\\multicolumn{10}{l}{\\textit{", panel_title, "}} \\\\"))
  for (i in seq_len(nrow(left))) {
    L <- left[i, ]; R <- right[i, ]
    row <- c(
      L$label,
      fmt_mean_sd(L$mean, L$sd),
      fmt_est_se(L$b1, L$se1),
      fmt_est_se(L$b2, L$se2), fmt_num(L$pjoint),
      fmt_mean_sd(R$mean, R$sd),
      fmt_est_se(R$b1, R$se1),
      fmt_est_se(R$b2, R$se2), fmt_num(R$pjoint), ifelse(is.na(L$N), "", as.character(L$N))
    )
    lines <- c(lines, paste(row, collapse = " & ") |> paste0(" \\\\"))
  }
  lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
  writeLines(lines, file)
}

balance_panel <- function(data, vars, labels, T0, T1, T2, absorb_cluster = FALSE) {
  require_vars(data, c(vars, T0, T1, T2, "s0d_dclusterid"), "balance panel")
  out <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    y <- vars[[i]]
    z <- data[[y]][data[[T0]] == 1]
    fit <- fit_feols(
      data, y, c(T1, T2),
      fe = if (absorb_cluster) "s0d_dclusterid" else NULL,
      cluster = "s0d_dclusterid",
      count_absorbed_fe = absorb_cluster
    )
    c1 <- coef_se(fit$model, T1); c2 <- coef_se(fit$model, T2)
    out[[i]] <- data.frame(
      label = labels[[i]], mean = mean(z, na.rm = TRUE), sd = stats::sd(z, na.rm = TRUE),
      b1 = c1[["estimate"]], se1 = c1[["se"]], b2 = c2[["estimate"]], se2 = c2[["se"]],
      pjoint = wald_p(
        fit$model,
        c(T1, T2),
        df2 = cluster_df2(fit$data, "s0d_dclusterid")
      ),
      N = stats::nobs(fit$model), stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(out)
}

# ---------- Conditional logit corresponding to Stata asclogit ----------
fit_asclogit_r <- function(data, choice = "choice", case = "hhid", alt = "desludge", base = 1,
                           alt_specific = "past", casevars, cluster_col = "s0d_dclusterid") {
  require_vars(data, c(choice, case, alt, alt_specific, casevars, cluster_col), "conditional logit")
  vars <- unique(c(choice, case, alt, alt_specific, casevars, cluster_col))
  
  # Use an ordinary data.frame for survival's model-frame machinery.
  # This also avoids tibble subsetting/NSE surprises inside coxph().
  d <- as.data.frame(
    data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  )
  
  if (!nrow(d)) stop("conditional logit has no complete observations")
  
  alts <- sort(unique(d[[alt]]))
  if (!base %in% alts) {
    stop("conditional logit base alternative ", base, " is not present in `", alt, "`")
  }
  nonbase <- setdiff(alts, base)
  
  # asclogit requires one chosen alternative per case. Check this explicitly
  # so malformed long-form data fail with an informative message.
  chosen_by_case <- tapply(d[[choice]], d[[case]], sum, na.rm = TRUE)
  if (any(chosen_by_case != 1)) {
    stop(
      "conditional logit requires exactly one chosen alternative per case; ",
      sum(chosen_by_case != 1), " case(s) violate this condition"
    )
  }
  
  rhs <- alt_specific
  map <- list()
  for (a in nonbase) {
    ad <- paste0("alt_", a)
    d[[ad]] <- as.integer(d[[alt]] == a)
    rhs <- c(rhs, ad)
    for (v in casevars) {
      nm <- paste0(v, "__alt_", a)
      d[[nm]] <- d[[v]] * d[[ad]]
      rhs <- c(rhs, nm)
      map[[paste(v, a, sep = "::")]] <- nm
    }
  }
  
  # Stata asclogit is a conditional-logit model. With exactly one chosen
  # alternative per case, its conditional likelihood is equivalent to a
  # stratified Cox partial likelihood with one event per stratum.
  #
  # Important NSE detail: coxph() captures its `cluster=` expression and
  # re-evaluates it inside model.frame(). Passing d[[cluster_col]] therefore
  # leaves the symbol `cluster_col` to be re-evaluated and can collide with
  # survival::cluster(). Put the cluster IDs in a dedicated model-data column
  # and pass that bare column name instead.
  d$.asclogit_time <- 1
  d$.asclogit_cluster_id <- d[[cluster_col]]
  
  f <- stats::as.formula(paste0(
    "survival::Surv(.asclogit_time, ", bt(choice), ") ~ ",
    paste(bt(rhs), collapse = " + "),
    " + strata(", bt(case), ")"
  ))
  environment(f) <- asNamespace("survival")
  
  model <- survival::coxph(
    formula = f,
    data = d,
    ties = "efron",
    cluster = .asclogit_cluster_id,
    robust = TRUE,
    model = TRUE,
    x = TRUE
  )
  
  list(
    model = model,
    data = d,
    alternatives = alts,
    nonbase = nonbase,
    map = map,
    cluster_col = cluster_col
  )
}

write_asclogit_tex <- function(fit, file, alt_labels, show_casevars = c("subsidy_high", "neighbor_high"), digits = 3) {
  b <- stats::coef(fit$model); V <- stats::vcov(fit$model)
  orse <- function(term) {
    if (!term %in% names(b)) return(c(or = NA_real_, se = NA_real_))
    or <- exp(b[[term]])
    c(or = or, se = or * sqrt(V[term, term]))
  }
  lines <- c("\\begin{tabular}{lc}", "\\hline\\hline", " & Odds ratio \\\\", "\\hline")
  if ("past" %in% names(b)) {
    z <- orse("past")
    lines <- c(lines, paste0("Past & ", fmt_num(z[["or"]], digits), " \\\\"),
               paste0(" & (", fmt_num(z[["se"]], digits), ") \\\\"))
  }
  for (a in fit$nonbase) {
    lab <- alt_labels[[as.character(a)]] %||% paste0("Alternative ", a)
    lines <- c(lines, paste0("\\multicolumn{2}{l}{\\textit{", lab, "}} \\\\"))
    for (v in show_casevars) {
      term <- fit$map[[paste(v, a, sep = "::")]]
      z <- orse(term)
      lines <- c(lines, paste0(v, " & ", fmt_num(z[["or"]], digits), " \\\\"),
                 paste0(" & (", fmt_num(z[["se"]], digits), ") \\\\"))
    }
  }
  lines <- c(
    lines, "\\hline",
    paste0("$N$ of Obs. & ", nrow(fit$data), " \\\\"),
    paste0("$N$ of Cases & ", length(unique(fit$data$hhid)), " \\\\"),
    "\\hline\\hline", "\\end{tabular}"
  )
  writeLines(lines, file)
}

reverse_balance_p <- function(data, outcome_treat, comparison_treat, balance_vars, absorb_cluster = FALSE) {
  sub <- data[[comparison_treat]] == 1 | data[[outcome_treat]] == 1
  fit <- fit_feols(
    data, outcome_treat, balance_vars,
    fe = if (absorb_cluster) "s0d_dclusterid" else NULL,
    cluster = "s0d_dclusterid", subset = sub,
    count_absorbed_fe = absorb_cluster
  )
  wald_p(
    fit$model,
    balance_vars,
    df2 = cluster_df2(fit$data, "s0d_dclusterid")
  )
}