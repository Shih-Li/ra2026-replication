# Replication target 01: Resisting Social Pressure in the Household Using Mobile Money
# Common setup and helper functions.
# Run from the project root (the directory containing 01_RSPitHUMM.Rproj).

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(99999)

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "fixest", "knitr"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(ggplot2)
  library(fixest)
})

ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
DATA_DIR <- file.path(ROOT, "data", "source", "cleaned")
OUT_MAIN_TABLE <- file.path(ROOT, "output", "tables", "main")
OUT_APP_TABLE <- file.path(ROOT, "output", "tables", "appendix")
OUT_MAIN_FIG <- file.path(ROOT, "output", "figures", "main")
OUT_APP_FIG <- file.path(ROOT, "output", "figures", "appendix")

invisible(lapply(
  c(DATA_DIR, OUT_MAIN_TABLE, OUT_APP_TABLE, OUT_MAIN_FIG, OUT_APP_FIG),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

RUN_GRAPHS <- TRUE
RUN_PERMUTATION <- TRUE
RUN_CAUSAL_FOREST <- TRUE
PERM_REPS <- 1000L

# Stata's areg ..., vce(robust) uses an HC1-style small-sample correction.
# Counting the absorbed FE in K is the closest fixest analogue for this workflow.
STATA_SSC <- fixest::ssc(K.adj = TRUE, K.fixef = "full", K.exact = TRUE)

main_results <- c("earn_business", "much_saved", "capital")
profit_outcomes <- c("t_sales", "sales", "monthly_profit", "weekly_profit")
savings_outcomes <- c("saving", "net_saving", "use_saving_5", "saving_amount_5", "saving_goal_6")
asset_outcomes <- c("asset_ent_index", "ent_asset_value", "ent_asset_dummy", "ent_asset_count", "inventory_value", "hh_asset_value")
labour_outcomes <- c("total_hoursbusiness", "hours_week", "adult_hoursbusiness", "child_hoursbusiness", "non_hhemployee", "employee_hours")
empower_outcomes <- c("switch_m", "own_decision", "equal_decision", "control_money", "remittance_share", "womans_income_share", "empower_1", "empower_2")
happy_outcomes <- c("happiness", "life_satisfaction", "worry_money", "worry_money_dum")
household_outcomes <- c("hh_income", "consumption_total", "consump_food", "consump_nonfood_exsch", "consump_school")
earnings_outcomes <- c("other_work_earning", "spouse_earning", "otherhh_earning", "spouse_bus_earn", "otherhh_bus_earn", "spouse_allearning", "otherhh_allearning")
give_spouse_outcomes <- c("give_spouse", "gave_spouse", "receive_spouse")
records_outcomes <- c("records_1", "records_2", "records_3", "records_4")
loan_use_outcomes <- c("loan_use_business", "loan_use_family", "loan_use_school", "loan_use_home", "loan_use_exp", "loan_use_sav", "loan_use_loan")
group_outcomes <- c("group_talk", "group_receivehelp", "group_givehelp")
remittance_outcomes <- c("remittance_samount", "remittance_ramount", "net_remittancerec", "remittance_mm", "receive_remittance", "sent_remittance")
hetero_vars <- c(
  "high_profits_base", "above_m_median_base", "abovem_inventory_base", "current_loan_base",
  "hyperbolic_base", "impatient_base", "abovem_risk_base", "abovem_sav_base",
  "above_med_basset_base", "married_base", "above_med_emp_base", "sent_fam_dummy_base",
  "spouse_fam_takes_base", "saving_goal_6_base", "spouse_bus_base", "hh_bus_base", "mm_close"
)
hetero_index <- c("hetero_perf_median", "hetero_selfc_median", "hetero_family_median")
strata_var <- "strata_fixed_base"
days_vars <- c("days", "days2")

read_clean <- function(filename) {
  path <- file.path(DATA_DIR, filename)
  if (!file.exists(path)) stop("Missing source data: ", path)
  haven::read_dta(path)
}

has_vars <- function(data, vars) all(vars %in% names(data))
existing_vars <- function(data, vars) vars[vars %in% names(data)]

rhs_formula <- function(outcome, rhs, fe = NULL) {
  rhs <- rhs[nzchar(rhs)]
  base <- paste(outcome, "~", if (length(rhs)) paste(rhs, collapse = " + ") else "1")
  if (!is.null(fe)) base <- paste0(base, " | ", fe)
  stats::as.formula(base)
}

fit_fe <- function(
    data,
    outcome,
    rhs,
    fe = strata_var,
    subset = NULL
) {
  
  fml <- rhs_formula(outcome, rhs, fe)
  
  vars_needed <- unique(all.vars(fml))
  
  if (!has_vars(data, vars_needed)) {
    stop(
      "Missing variable(s) for model ",
      outcome,
      ": ",
      paste(
        setdiff(vars_needed, names(data)),
        collapse = ", "
      )
    )
  }
  
  if (!is.null(subset)) {
    data <- data[subset, , drop = FALSE]
  }
  
  fixest::feols(
    fml,
    data = data,
    vcov = "hetero",
    ssc = STATA_SSC,
    
    # Important for matching Stata areg
    fixef.rm = "none",
    
    notes = FALSE,
    warn = TRUE
  )
}

fit_ols <- function(data, outcome, rhs, robust = FALSE, subset = NULL, intercept = TRUE) {
  vars_needed <- unique(c(outcome, rhs))
  if (!has_vars(data, vars_needed)) {
    stop("Missing variable(s) for model ", outcome, ": ", paste(setdiff(vars_needed, names(data)), collapse = ", "))
  }
  if (!is.null(subset)) data <- data[subset, , drop = FALSE]
  f <- stats::as.formula(paste(outcome, "~", if (!intercept) "0 +" else "", paste(rhs, collapse = " + ")))
  fixest::feols(
    f, data = data, vcov = if (robust) "hetero" else "iid",
    ssc = STATA_SSC, notes = FALSE, warn = TRUE
  )
}

coef_table <- function(model) {
  as.data.frame(fixest::coeftable(model)) |>
    tibble::rownames_to_column("term") |>
    dplyr::rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`
    )
}

coef_stat <- function(model, term, field = c("estimate", "std_error", "p_value")) {
  field <- match.arg(field)
  tab <- coef_table(model)
  i <- match(term, tab$term)
  if (is.na(i)) return(NA_real_)
  tab[[field]][i]
}

wald_linear <- function(model, weights) {
  b <- stats::coef(model)
  if (!all(names(weights) %in% names(b))) return(NA_real_)
  V <- stats::vcov(model, vcov = "hetero", ssc = STATA_SSC)
  L <- setNames(rep(0, length(b)), names(b))
  L[names(weights)] <- weights
  est <- sum(L * b)
  se2 <- as.numeric(t(L) %*% V %*% L)
  if (!is.finite(se2) || se2 <= 0) return(NA_real_)
  fstat <- (est^2) / se2
  df2 <- fixest::degrees_freedom(model, type = "t", vcov = "hetero", ssc = STATA_SSC)
  stats::pf(fstat, df1 = 1, df2 = df2, lower.tail = FALSE)
}

p_equal <- function(model, a, b) wald_linear(model, setNames(c(1, -1), c(a, b)))

p_sum_equal <- function(model, lhs_terms, rhs_terms) {
  w <- c(setNames(rep(1, length(lhs_terms)), lhs_terms), setNames(rep(-1, length(rhs_terms)), rhs_terms))
  w <- tapply(w, names(w), sum)
  wald_linear(model, w)
}

model_r2 <- function(model) {
  out <- suppressWarnings(fixest::r2(model, "r2"))
  as.numeric(out[1])
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)

standard_extras <- function(model, data, outcome, baseline = NULL, control_filter = NULL, test_terms = c("treatment2", "treatment3")) {
  if (is.null(control_filter)) control_filter <- data$treatment1 == 1
  ans <- c(
    Observations = stats::nobs(model),
    `R-squared` = model_r2(model),
    `Control mean` = safe_mean(data[[outcome]][control_filter])
  )
  if (!is.null(baseline) && baseline %in% names(data)) {
    ans <- c(ans, `Control mean baseline` = safe_mean(data[[baseline]][control_filter]))
  }
  if (all(test_terms %in% names(stats::coef(model)))) {
    ans <- c(ans, `p-value MA=MD` = p_equal(model, test_terms[1], test_terms[2]))
  }
  ans
}

by_qvalues <- function(models, terms, cap_one = FALSE) {
  ans <- setNames(vector("list", length(models)), names(models))
  for (nm in names(models)) ans[[nm]] <- setNames(rep(NA_real_, length(terms)), terms)
  for (term in terms) {
    p <- vapply(models, coef_stat, numeric(1), term = term, field = "p_value")
    ok <- is.finite(p)
    q <- rep(NA_real_, length(p))
    if (any(ok)) q[ok] <- stats::p.adjust(p[ok], method = "BY")
    if (cap_one) q[q == 1] <- 0.99
    for (i in seq_along(models)) ans[[i]][term] <- q[i]
  }
  ans
}

make_regression_table <- function(models, terms, model_names = names(models), term_labels = NULL,
                                  extras = NULL, q_values = NULL, permutation_p = NULL) {
  if (is.null(term_labels)) term_labels <- setNames(terms, terms)
  rows <- list()
  for (term in terms) {
    label <- unname(term_labels[term])
    if (is.na(label) || !nzchar(label)) label <- term
    for (stat in c("Estimate", "Robust SE", "Robust p-value")) {
      vals <- vapply(models, function(m) {
        switch(stat,
          Estimate = coef_stat(m, term, "estimate"),
          `Robust SE` = coef_stat(m, term, "std_error"),
          `Robust p-value` = coef_stat(m, term, "p_value")
        )
      }, numeric(1))
      rows[[length(rows) + 1L]] <- cbind(Row = paste(label, stat, sep = " — "), as.data.frame(as.list(vals), check.names = FALSE))
    }
    if (!is.null(q_values)) {
      vals <- vapply(seq_along(models), function(i) q_values[[i]][term], numeric(1))
      rows[[length(rows) + 1L]] <- cbind(Row = paste(label, "FDR q-value", sep = " — "), as.data.frame(as.list(vals), check.names = FALSE))
    }
    if (!is.null(permutation_p)) {
      vals <- vapply(seq_along(models), function(i) permutation_p[[i]][term], numeric(1))
      rows[[length(rows) + 1L]] <- cbind(Row = paste(label, "Permutation p-value", sep = " — "), as.data.frame(as.list(vals), check.names = FALSE))
    }
  }
  tab <- dplyr::bind_rows(rows)
  names(tab)[-1] <- model_names

  if (!is.null(extras)) {
    extra_names <- unique(unlist(lapply(extras, names)))
    for (stat in extra_names) {
      vals <- vapply(extras, function(x) if (stat %in% names(x)) as.numeric(x[[stat]]) else NA_real_, numeric(1))
      erow <- cbind(Row = stat, as.data.frame(as.list(vals), check.names = FALSE))
      names(erow)[-1] <- model_names
      tab <- dplyr::bind_rows(tab, erow)
    }
  }
  tab
}

write_table <- function(tab, stem, digits = 3) {
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tab, paste0(stem, ".csv"), row.names = FALSE, na = "")
  tex <- knitr::kable(tab, format = "latex", booktabs = TRUE, digits = digits, escape = TRUE)
  writeLines(tex, paste0(stem, ".tex"), useBytes = TRUE)
  invisible(tab)
}

write_regression_table <- function(models, terms, stem, model_names = names(models), term_labels = NULL,
                                   extras = NULL, q_values = NULL, permutation_p = NULL, digits = 3) {
  tab <- make_regression_table(models, terms, model_names, term_labels, extras, q_values, permutation_p)
  write_table(tab, stem, digits = digits)
}

fit_outcome_models <- function(data, outcomes, add_baseline = TRUE, extra_rhs = character(), fe = strata_var) {
  setNames(lapply(outcomes, function(y) {
    rhs <- c(if (add_baseline && paste0(y, "_base") %in% names(data)) paste0(y, "_base"), extra_rhs)
    fit_fe(data, y, rhs, fe = fe)
  }), outcomes)
}

extras_for_outcomes <- function(models, data, outcomes, control_filter = data$treatment1 == 1, include_baseline = TRUE) {
  Map(function(m, y) standard_extras(
    m, data, y,
    baseline = if (include_baseline && paste0(y, "_base") %in% names(data)) paste0(y, "_base") else NULL,
    control_filter = control_filter
  ), models, outcomes)
}

permute_within_group <- function(x, g) {
  ave(seq_along(x), g, FUN = function(idx) sample(idx, length(idx))) |>
    as.integer() |>
    (function(idx) x[idx])()
}

permutation_pvalue <- function(data, outcome, target, other, strata = strata_var, reps = PERM_REPS, seed = 9999L) {
  d <- data[, c(outcome, target, other, strata), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!nrow(d)) return(NA_real_)
  f <- stats::as.formula(paste(outcome, "~", target, "+", other))
  obs_fit <- stats::lm(f, data = d)
  obs <- stats::coef(obs_fit)[target]
  if (!is.finite(obs)) return(NA_real_)
  set.seed(seed)
  perm_stats <- numeric(reps)
  original <- d[[target]]
  for (r in seq_len(reps)) {
    d[[target]] <- permute_within_group(original, d[[strata]])
    perm_stats[r] <- stats::coef(stats::lm(f, data = d))[target]
  }
  (sum(abs(perm_stats) >= abs(obs), na.rm = TRUE) + 1) / (sum(is.finite(perm_stats)) + 1)
}

save_plot <- function(plot, path, width = 8, height = 5, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
}

cdf_frame <- function(data, value, treatment = "treatment") {
  d <- data[, c(value, treatment)]
  names(d) <- c("value", "treatment")
  d <- d[is.finite(d$value) & !is.na(d$treatment), , drop = FALSE]
  d |>
    dplyr::group_by(treatment) |>
    dplyr::arrange(value, .by_group = TRUE) |>
    dplyr::mutate(percentile = rank(value, ties.method = "max") / dplyr::n()) |>
    dplyr::ungroup()
}
