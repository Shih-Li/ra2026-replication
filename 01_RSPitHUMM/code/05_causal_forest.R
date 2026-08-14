# Table A21: causal-forest heterogeneity.
# Translation of causal_forest.do directly into R/grf.
# The original Stata package itself notes that this table is machine-sensitive.

if (!RUN_CAUSAL_FOREST) {
  message("Skipping causal forest (RUN_CAUSAL_FOREST = FALSE).")
} else {
  if (!requireNamespace("grf", quietly = TRUE)) {
    stop("Table A21 requires package 'grf'. Install with install.packages('grf').")
  }

  cf <- read_clean("survey_data.dta") |>
    dplyr::filter(consent == 1)
  if ("above_m_median_base" %in% names(cf)) cf$above_m_median_base[is.na(cf$above_m_median_base)] <- 0

  cf_vars <- c(
    "earn_business_base", "capital_base", "much_saved_base", "respondent_age_base",
    "married_base", "education_level_base", "work_occupation_base", "hh_size_base",
    "year_business_base", "own_business_1_base", "own_business_2_base", "sales_base",
    "expenditure_base", "non_hhemployee_base", "employee_hours_base", "current_loan_base",
    "consumption_total_base", "have_save_base", "control_money_base", "spouse_fam_takes_base",
    "hh_bus_base", "hyperbolic_base", "impatient_base", "saving_goal_6_base",
    "womans_income_share_base", "spouse_allearning_base", "own_decision_base",
    "above_m_median_base", "hetero_family_base", "hetero_selfc_base", "hetero_perf_base"
  )
  cf$current_loan <- cf$current_loan_base
  missing_cf <- setdiff(cf_vars, names(cf))
  if (length(missing_cf)) stop("Missing causal-forest covariates: ", paste(missing_cf, collapse = ", "))

  set.seed(99999)
  cf$split <- stats::runif(nrow(cf)) > 0.5
  cf$row_id <- seq_len(nrow(cf))

  result_blocks <- list()
  n_threads <- parallel::detectCores(logical = FALSE)
  if (!is.finite(n_threads) || n_threads < 1) n_threads <- 1L

  for (y in main_results) {
    vars_needed <- c(y, cf_vars, "treatment3", "split", "row_id")
    d <- cf[, vars_needed, drop = FALSE]
    cc <- stats::complete.cases(d[, c(y, cf_vars, "treatment3"), drop = FALSE])
    d <- d[cc, , drop = FALSE]

    X <- as.matrix(d[, cf_vars, drop = FALSE])
    Y <- as.numeric(d[[y]])
    W <- as.numeric(d$treatment3)
    split <- d$split
    pred <- rep(NA_real_, nrow(d))
    se <- rep(NA_real_, nrow(d))

    # First fold: train where split == FALSE, predict the held-out TRUE observations.
    train1 <- !split
    hold1 <- split
    set.seed(99999)
    f1 <- grf::causal_forest(
      X[train1, , drop = FALSE], Y[train1], W[train1],
      seed = 99999, num.threads = n_threads
    )
    pr1 <- predict(f1, X[hold1, , drop = FALSE], estimate.variance = TRUE)
    pred[hold1] <- pr1$predictions
    if (!is.null(pr1$variance.estimates)) se[hold1] <- sqrt(pmax(pr1$variance.estimates, 0))

    # Second fold: reverse holdout, matching `replace split = 1-split` in Stata.
    train2 <- split
    hold2 <- !split
    set.seed(99999)
    f2 <- grf::causal_forest(
      X[train2, , drop = FALSE], Y[train2], W[train2],
      seed = 99999, num.threads = n_threads
    )
    pr2 <- predict(f2, X[hold2, , drop = FALSE], estimate.variance = TRUE)
    pred[hold2] <- pr2$predictions
    if (!is.null(pr2$variance.estimates)) se[hold2] <- sqrt(pmax(pr2$variance.estimates, 0))

    d$estimated_effect <- pred
    d$SE_effect <- se
    d$quart <- dplyr::ntile(d$estimated_effect, 4)

    summarize_vars <- c("estimated_effect", cf_vars)
    block <- lapply(summarize_vars, function(v) {
      q1 <- d[[v]][d$quart == 1]
      q4 <- d[[v]][d$quart == 4]
      tt <- tryCatch(stats::t.test(q4, q1, var.equal = FALSE), error = function(e) NULL)
      tibble::tibble(
        variable = v,
        least_quartile_mean = safe_mean(q1),
        most_quartile_mean = safe_mean(q4),
        difference_most_minus_least = safe_mean(q4) - safe_mean(q1),
        p_value_unequal_variance_ttest = if (is.null(tt)) NA_real_ else tt$p.value
      )
    }) |>
      dplyr::bind_rows()
    names(block)[-1] <- paste0(y, c("_least", "_most", "_diff", "_p"))
    result_blocks[[y]] <- block

    utils::write.csv(
      d[, c("row_id", "estimated_effect", "SE_effect", "quart")],
      file.path(OUT_APP_TABLE, paste0("causal_forest_predictions_", y, ".csv")),
      row.names = FALSE
    )
  }

  table_cf <- Reduce(function(a, b) dplyr::full_join(a, b, by = "variable"), result_blocks)
  write_table(table_cf, file.path(OUT_APP_TABLE, "table_causal_forest"), digits = 2)
}
