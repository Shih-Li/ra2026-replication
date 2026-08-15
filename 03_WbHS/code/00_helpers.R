# Shared helpers for the WBHS replication.

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "sandwich", "lmtest",
  "car", "ivreg", "ggplot2", "knitr"
)

check_packages <- function() {
  missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      "\nRun source('00_setup.R') once, then rerun 00_main.R.",
      call. = FALSE
    )
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

require_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "Missing required input file(s):\n- ", paste(missing, collapse = "\n- "),
      "\n\nSee README_MANUAL.md for the required folder layout.",
      call. = FALSE
    )
  }
}

require_cols <- function(data, cols, context = "data") {
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop("Missing column(s) in ", context, ": ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

zap <- function(x) haven::zap_labels(x)

read_stata <- function(path) {
  out <- haven::read_dta(path)
  out[] <- lapply(out, zap)
  out
}

stata_merge_1to1 <- function(master, using, id = "ID_Participante") {
  require_cols(master, id, "master dataset")
  require_cols(using, id, "using dataset")
  if (anyDuplicated(master[[id]])) stop("Duplicate IDs in master dataset: ", id, call. = FALSE)
  if (anyDuplicated(using[[id]])) stop("Duplicate IDs in using dataset: ", id, call. = FALSE)
  master_ids <- master[[id]]
  using_ids <- using[[id]]
  keep_using <- c(id, setdiff(names(using), names(master)))
  merged <- dplyr::full_join(master, using[, keep_using, drop = FALSE], by = id)
  flag <- ifelse(merged[[id]] %in% master_ids & merged[[id]] %in% using_ids, 3L,
                 ifelse(merged[[id]] %in% master_ids, 1L, 2L))
  list(data = merged, merge = flag)
}

stata_row_sum <- function(data, cols) {
  require_cols(data, cols)
  rowSums(as.data.frame(data[, cols, drop = FALSE]), na.rm = TRUE)
}

stata_row_mean <- function(data, cols) {
  require_cols(data, cols)
  z <- rowMeans(as.data.frame(data[, cols, drop = FALSE]), na.rm = TRUE)
  z[is.nan(z)] <- NA_real_
  z
}

additive_sum <- function(data, cols) {
  require_cols(data, cols)
  Reduce(`+`, lapply(cols, function(v) as.numeric(data[[v]])))
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

stata_year <- function(x) {
  if (inherits(x, "Date")) return(as.integer(format(x, "%Y")))
  if (inherits(x, "POSIXt")) return(as.integer(format(x, "%Y")))
  if (is.numeric(x)) {
    d <- as.Date(x, origin = "1960-01-01")
    return(as.integer(format(d, "%Y")))
  }
  suppressWarnings(as.integer(substr(as.character(x), 1L, 4L)))
}

not_blank <- function(x) !is.na(x) & trimws(as.character(x)) != ""

fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

stars <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))
}

# Stata's main specifications use all strata dummies except the first level-4 dummy.
strata_vars <- function(data, include_base = FALSE) {
  out <- grep("^lb_estrato_level", names(data), value = TRUE)
  if (!include_base) out <- setdiff(out, "lb_estrato_level4_1")
  out
}

controls_unb <- function() c("lb_years_education", "lb_jefe_hogar")
controls_all <- function() c("new_sd_ohip0", "new_sd_rosen0", "lb_employ", "lb_years_education", "lb_jefe_hogar")

controls_unb_gender <- function() c(
  "lb_years_education", "lb_genero", "lb_years_education:lb_genero",
  "lb_jefe_hogar", "lb_jefe_hogar:lb_genero"
)

controls_all_gender <- function() c(
  "new_sd_ohip0", "lb_genero", "new_sd_ohip0:lb_genero",
  "new_sd_rosen0", "new_sd_rosen0:lb_genero",
  "lb_employ", "lb_employ:lb_genero",
  "lb_years_education", "lb_years_education:lb_genero",
  "lb_jefe_hogar", "lb_jefe_hogar:lb_genero"
)

rhs_string <- function(terms) {
  terms <- unique(terms[nzchar(terms)])
  if (!length(terms)) "1" else paste(terms, collapse = " + ")
}

make_formula <- function(outcome, terms) stats::as.formula(paste(outcome, "~", rhs_string(terms)))

robust_lm <- function(data, outcome, terms, subset = NULL) {
  if (!is.null(subset)) data <- data[which(subset %in% TRUE), , drop = FALSE]
  f <- make_formula(outcome, terms)
  fit <- stats::lm(f, data = data, na.action = stats::na.omit)
  vc <- sandwich::vcovHC(fit, type = "HC1")
  list(model = fit, vcov = vc, data = data, formula = f)
}

coef_stats <- function(fit, term) {
  ct <- lmtest::coeftest(fit$model, vcov. = fit$vcov)
  if (!term %in% rownames(ct)) {
    # Try reverse interaction ordering used by R formulas.
    if (grepl(":", term, fixed = TRUE)) {
      rev_term <- paste(rev(strsplit(term, ":", fixed = TRUE)[[1]]), collapse = ":")
      if (rev_term %in% rownames(ct)) term <- rev_term
    }
  }
  if (!term %in% rownames(ct)) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = unname(ct[term, 1]), se = unname(ct[term, 2]), p = unname(ct[term, 4]))
}

wald_p <- function(fit, terms) {
  terms <- terms[terms %in% names(stats::coef(fit$model))]
  if (!length(terms)) return(NA_real_)
  ans <- try(car::linearHypothesis(fit$model, terms, vcov. = fit$vcov, test = "F"), silent = TRUE)
  if (inherits(ans, "try-error")) return(NA_real_)
  pcol <- grep("Pr\\(>F\\)", names(ans), value = TRUE)
  if (!length(pcol)) return(NA_real_)
  as.numeric(ans[nrow(ans), pcol[1]])
}

extract_terms <- function(fit, terms) {
  out <- lapply(terms, function(term) {
    st <- coef_stats(fit, term)
    data.frame(term = term, estimate = st[["estimate"]], se = st[["se"]], p = st[["p"]])
  })
  dplyr::bind_rows(out)
}

subgroup_mask <- function(data, group) {
  switch(group,
    All = rep(TRUE, nrow(data)),
    Men = data$lb_genero == 0,
    Women = data$lb_genero == 1,
    stop("Unknown subgroup: ", group)
  )
}

outcome_result <- function(data, outcome, baseline = NULL, group = "All",
                           controls = controls_unb(), add_strata = TRUE,
                           extra_terms = character(), sample_mask = NULL,
                           treatment_term = "treatment") {
  mask <- subgroup_mask(data, group)
  if (!is.null(sample_mask)) mask <- mask & sample_mask
  terms <- c(treatment_term, baseline, controls, extra_terms)
  if (add_strata) terms <- c(terms, strata_vars(data))
  fit <- robust_lm(data, outcome, terms, subset = mask)
  st <- coef_stats(fit, treatment_term)
  used <- as.integer(rownames(stats::model.frame(fit$model)))
  used_data <- data[used, , drop = FALSE]
  ctrl_mean <- safe_mean(used_data[[outcome]][used_data$treatment == 0])
  data.frame(
    outcome = outcome, group = group, control_mean = ctrl_mean,
    estimate = st[["estimate"]], se = st[["se"]], p = st[["p"]],
    stars = stars(st[["p"]]), n = stats::nobs(fit$model), stringsAsFactors = FALSE
  )
}

gender_diff_p <- function(data, outcome, baseline = NULL, controls = controls_unb(),
                          sample_mask = NULL, extra_terms = character()) {
  mask <- !is.na(data$lb_genero)
  if (!is.null(sample_mask)) mask <- mask & sample_mask
  terms <- c("treatment", "lb_genero", "treatment:lb_genero", baseline,
             controls, extra_terms, strata_vars(data))
  # Interact strata with gender as in the Stata specifications.
  s <- strata_vars(data)
  terms <- c(terms, paste0(s, ":lb_genero"))
  fit <- robust_lm(data, outcome, unique(terms), subset = mask)
  coef_stats(fit, "treatment:lb_genero")[["p"]]
}

write_table_outputs <- function(df, stem, caption, out_dir) {
  ensure_dir(out_dir)
  utils::write.csv(df, file.path(out_dir, paste0(stem, ".csv")), row.names = FALSE, na = "")
  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, caption = caption, digits = 3)
  writeLines(tex, file.path(out_dir, paste0(stem, ".tex")), useBytes = TRUE)
  invisible(df)
}

panel_treatment_diff <- function(data, y1, y2, group = "All", baseline_work_filter = FALSE) {
  d <- data
  d$.id <- seq_len(nrow(d))
  mask <- subgroup_mask(d, group)
  if (baseline_work_filter) mask <- mask & !is.na(d$ltotal_bl)
  d <- d[mask, , drop = FALSE]
  long <- dplyr::bind_rows(
    dplyr::transmute(d, .id, treatment, time = 1L, y = .data[[y1]]),
    dplyr::transmute(d, .id, treatment, time = 2L, y = .data[[y2]])
  )
  long$d1 <- as.numeric(long$time == 1L)
  long$d2 <- as.numeric(long$time == 2L)
  long$t1 <- long$treatment * long$d1
  long$t2 <- long$treatment * long$d2
  fit <- stats::lm(y ~ t1 + t2 + d1 + d2 + factor(.id), data = long, na.action = stats::na.omit)
  vc <- sandwich::vcovCL(fit, cluster = long$.id[as.integer(rownames(stats::model.frame(fit)))], type = "HC1")
  obj <- list(model = fit, vcov = vc)
  ans <- try(car::linearHypothesis(fit, "t1 = t2", vcov. = vc, test = "F"), silent = TRUE)
  if (inherits(ans, "try-error")) return(NA_real_)
  pcol <- grep("Pr\\(>F\\)", names(ans), value = TRUE)
  as.numeric(ans[nrow(ans), pcol[1]])
}

# Construct the single Stata-style stratum code used by rwolf/IV specifications.
make_n_estrato <- function(data) {
  out <- rep(NA_real_, nrow(data))
  for (i in 1:44) {
    v <- paste0("lb_estrato_level4_", i)
    if (v %in% names(data)) out[!is.na(data[[v]]) & data[[v]] == 1] <- i
  }
  for (i in 1:2) {
    v <- paste0("lb_estrato_level3_", i)
    if (v %in% names(data)) out[!is.na(data[[v]]) & data[[v]] == 1] <- 100 * i
  }
  for (i in 1:2) {
    v <- paste0("lb_estrato_level1_", i)
    if (v %in% names(data)) out[!is.na(data[[v]]) & data[[v]] == 1] <- 1000 * i
  }
  out
}

permute_within_strata <- function(treat, strata) {
  out <- treat
  groups <- split(seq_along(treat), interaction(strata, drop = TRUE, lex.order = TRUE))
  for (idx in groups) out[idx] <- sample(treat[idx], length(idx), replace = FALSE)
  out
}

# Romano-Wolf max-t stepdown analogue using treatment-label permutations within randomization strata.
# The authors' Stata code uses rwolf with reps(200), seed(456), strata(n_estrato), controls(), bl(_bl).
romano_wolf <- function(data, outcomes, baseline_suffix = "_bl", controls = controls_unb(),
                        reps = 200L, seed = 456L, group = "All") {
  mask <- subgroup_mask(data, group)
  d <- data[mask, , drop = FALSE]
  if (!"n_estrato" %in% names(d)) d$n_estrato <- make_n_estrato(d)

  get_t <- function(dat, outcome) {
    bl <- paste0(outcome, baseline_suffix)
    terms <- c("treatment", if (bl %in% names(dat)) bl, controls, strata_vars(dat))
    fit <- try(robust_lm(dat, outcome, terms), silent = TRUE)
    if (inherits(fit, "try-error")) return(NA_real_)
    st <- coef_stats(fit, "treatment")
    if (is.na(st[["estimate"]]) || is.na(st[["se"]]) || st[["se"]] == 0) return(NA_real_)
    abs(st[["estimate"]] / st[["se"]])
  }

  obs <- vapply(outcomes, function(y) get_t(d, y), numeric(1))
  ok <- is.finite(obs)
  ans <- setNames(rep(NA_real_, length(outcomes)), outcomes)
  if (!any(ok)) return(ans)
  ys <- outcomes[ok]
  obs <- obs[ok]
  ord <- order(obs, decreasing = TRUE)
  ys_ord <- ys[ord]
  obs_ord <- obs[ord]

  set.seed(seed)
  boot <- matrix(NA_real_, nrow = reps, ncol = length(ys_ord), dimnames = list(NULL, ys_ord))
  for (b in seq_len(reps)) {
    dp <- d
    valid <- !is.na(dp$treatment) & !is.na(dp$n_estrato)
    dp$treatment[valid] <- permute_within_strata(dp$treatment[valid], dp$n_estrato[valid])
    boot[b, ] <- vapply(ys_ord, function(y) get_t(dp, y), numeric(1))
  }

  raw_step <- rep(NA_real_, length(ys_ord))
  for (j in seq_along(ys_ord)) {
    mx <- apply(boot[, j:length(ys_ord), drop = FALSE], 1L, max, na.rm = TRUE)
    raw_step[j] <- (1 + sum(mx >= obs_ord[j], na.rm = TRUE)) / (reps + 1)
  }
  adj_ord <- cummax(raw_step)
  mapped <- setNames(rep(NA_real_, length(ys)), ys)
  mapped[ys_ord] <- adj_ord
  ans[names(mapped)] <- mapped
  ans
}

# IPW ATE with probit propensity and a nonparametric bootstrap SE.
ipw_ate <- function(data, outcome, group = "All", controls = controls_all(), reps = 200L, seed = 456L) {
  mask <- subgroup_mask(data, group)
  if (group == "Men") controls <- setdiff(controls, "lb_jefe_hogar") # separation in original Stata code
  d <- data[mask, , drop = FALSE]
  base_needed <- unique(c(outcome, "treatment", controls))
  d <- d[stats::complete.cases(d[, base_needed, drop = FALSE]), , drop = FALSE]

  # Only retain strata indicators represented in both treatment arms, matching the Stata script.
  sv <- strata_vars(d, include_base = TRUE)
  sv <- sv[vapply(sv, function(v) {
    any(d[[v]] != 0 & d$treatment == 0, na.rm = TRUE) && any(d[[v]] != 0 & d$treatment == 1, na.rm = TRUE)
  }, logical(1))]
  pterms <- unique(c(controls, sv))

  one <- function(z) {
    f <- make_formula("treatment", pterms)
    psfit <- suppressWarnings(stats::glm(f, data = z, family = stats::binomial(link = "probit")))
    p <- as.numeric(stats::predict(psfit, type = "response"))
    keep <- is.finite(p) & p > 1e-6 & p < 1 - 1e-6
    z <- z[keep, , drop = FALSE]; p <- p[keep]
    if (!nrow(z) || !any(z$treatment == 1) || !any(z$treatment == 0)) return(NA_real_)
    w1 <- z$treatment / p
    w0 <- (1 - z$treatment) / (1 - p)
    mu1 <- sum(w1 * z[[outcome]], na.rm = TRUE) / sum(w1, na.rm = TRUE)
    mu0 <- sum(w0 * z[[outcome]], na.rm = TRUE) / sum(w0, na.rm = TRUE)
    mu1 - mu0
  }

  est <- one(d)
  set.seed(seed)
  boots <- replicate(reps, {
    idx <- sample.int(nrow(d), nrow(d), replace = TRUE)
    tryCatch(one(d[idx, , drop = FALSE]), error = function(e) NA_real_)
  })
  se <- stats::sd(boots, na.rm = TRUE)
  p <- if (is.finite(est) && is.finite(se) && se > 0) 2 * stats::pnorm(-abs(est / se)) else NA_real_
  data.frame(outcome = outcome, group = group, estimate = est, se = se, p = p,
             stars = stars(p), n = nrow(d), stringsAsFactors = FALSE)
}

robust_iv <- function(data, outcome, endogenous, instruments, exogenous,
                      subset = NULL, cluster = NULL) {
  if (!is.null(subset)) data <- data[which(subset %in% TRUE), , drop = FALSE]
  lhs <- paste(outcome, "~", rhs_string(c(endogenous, exogenous)))
  rhs <- rhs_string(c(instruments, exogenous))
  f <- stats::as.formula(paste(lhs, "|", rhs))
  fit <- ivreg::ivreg(f, data = data, na.action = stats::na.omit)
  if (is.null(cluster)) {
    vc <- sandwich::vcovHC(fit, type = "HC1")
  } else {
    mf_rows <- as.integer(rownames(stats::model.frame(fit)))
    vc <- sandwich::vcovCL(fit, cluster = data[[cluster]][mf_rows], type = "HC1")
  }
  list(model = fit, vcov = vc, formula = f)
}
