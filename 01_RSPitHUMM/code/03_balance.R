# Table A2: baseline balance.
# Translation of analysis_balancecheck2.do.

balance <- read_clean("survey_data.dta")

balance_families <- list(
  strata = c("branch1_base", "branch2_base", "branch3_base", "branch4_base", "branch5_base", "branch6_base", "high_profits_base", "current_loan_base", "hyperbolic_base", "above_m_median_base"),
  hh_dem = c("respondent_age_base", "married_base", "hh_size_base"),
  hh_educ_work = c("completed_primary_base", "completed_secondary_base", "work_occupation_base"),
  branch = c("loan_amount_dis_base", "loan_40_base", "group_size"),
  business = c("monthly_profit_base", "earn_business_base", "ent_asset_value_base", "inventory_value_base", "total_hoursbusiness_base", "age_business_base", "non_hhemployee_base", "employee_hours_base"),
  empower = c("empower_1_base", "control_money_base", "spouse_fam_takes_base", "hh_bus_base", "spouse_bus_base"),
  save = c("impatient_base", "have_save_base", "much_saved_base", "mobile_account_base", "mm_distance", "saving_goal_6"),
  household = c("hh_income_base", "total_asset_value_base", "consumption_total_base")
)

balance_vars <- existing_vars(balance, unlist(balance_families, use.names = FALSE))
for (v in c("spouse_bus_base", "above_m_median_base", "hh_bus_base")) {
  if (v %in% names(balance)) balance[[v]][is.na(balance[[v]])] <- 0
}

balance <- balance |>
  dplyr::mutate(
    treat_disburse = as.numeric(treatment == 2),
    control = as.numeric(treatment == 0),
    treat_account = as.numeric(treatment == 1),
    treatment_combined = as.numeric(treatment %in% c(1, 2))
  )

# Orthogonality models reported as diagnostic tests in the source.
orthog_rhs <- balance_vars
orthog <- list(
  mobile_account = fit_fe(balance, "treat_account", orthog_rhs),
  mobile_disburse = fit_fe(balance, "treat_disburse", orthog_rhs),
  pooled_treatment = fit_fe(balance, "treatment_combined", orthog_rhs)
)
orthog_stats <- tibble::tibble(
  model = names(orthog),
  n = vapply(orthog, stats::nobs, numeric(1)),
  r2 = vapply(orthog, model_r2, numeric(1))
)
utils::write.csv(orthog_stats, file.path(OUT_APP_TABLE, "balance_orthogonality_diagnostics.csv"), row.names = FALSE)

balance_rows <- lapply(balance_vars, function(v) {
  d_c <- balance[[v]][balance$control == 1]
  d_a <- balance[[v]][balance$treat_account == 1]
  d_d <- balance[[v]][balance$treat_disburse == 1]

  fit3 <- stats::lm(stats::as.formula(paste(v, "~ treat_disburse + treat_account")), data = balance)
  ctab <- summary(fit3)$coefficients
  p_disburse <- if ("treat_disburse" %in% rownames(ctab)) ctab["treat_disburse", 4] else NA_real_
  p_account <- if ("treat_account" %in% rownames(ctab)) ctab["treat_account", 4] else NA_real_
  V <- stats::vcov(fit3)
  b <- stats::coef(fit3)
  if (all(c("treat_disburse", "treat_account") %in% names(b))) {
    diff <- b["treat_disburse"] - b["treat_account"]
    se <- sqrt(V["treat_disburse", "treat_disburse"] + V["treat_account", "treat_account"] - 2 * V["treat_disburse", "treat_account"])
    p_equal_arms <- 2 * stats::pt(abs(diff / se), df = stats::df.residual(fit3), lower.tail = FALSE)
  } else p_equal_arms <- NA_real_
  fs <- summary(fit3)$fstatistic
  joint_p <- if (is.null(fs)) NA_real_ else stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

  fit2 <- stats::lm(stats::as.formula(paste(v, "~ treatment_combined")), data = balance)
  fs2 <- summary(fit2)$fstatistic
  pooled_p <- if (is.null(fs2)) NA_real_ else stats::pf(fs2[1], fs2[2], fs2[3], lower.tail = FALSE)

  tibble::tibble(
    variable = v,
    control_mean = safe_mean(d_c), control_sd = safe_sd(d_c),
    account_mean = safe_mean(d_a), account_sd = safe_sd(d_a),
    disburse_mean = safe_mean(d_d), disburse_sd = safe_sd(d_d),
    p_control_vs_disburse = p_disburse,
    p_control_vs_account = p_account,
    p_disburse_vs_account = p_equal_arms,
    joint_p_value = joint_p,
    pooled_p_value = pooled_p
  )
}) |>
  dplyr::bind_rows()

label_map <- c(
  branch1_base = "branch1", branch2_base = "branch2", branch3_base = "branch3", branch4_base = "branch4", branch5_base = "branch5", branch6_base = "branch6",
  high_profits_base = "high profits", above_m_median_base = "hides money", current_loan_base = "repeat borrower", hyperbolic_base = "hyperbolic",
  respondent_age_base = "respondent age (yrs)", married_base = "married", hh_size_base = "household size",
  completed_primary_base = "completed primary", completed_secondary_base = "completed secondary", work_occupation_base = "other job",
  loan_amount_dis_base = "loan amount (USD)", loan_40_base = "loan term 40 weeks", group_size = "group size",
  monthly_profit_base = "profit calculated (USD)", earn_business_base = "profit self-report (USD)", ent_asset_value_base = "business assets (USD)", inventory_value_base = "inventory (USD)",
  total_hoursbusiness_base = "weekly hours business", age_business_base = "business age (yrs)", non_hhemployee_base = "has employees", employee_hours_base = "employee hours",
  empower_1_base = "empowerment index", control_money_base = "controls own earnings", spouse_fam_takes_base = "reports sharing pressure", hh_bus_base = "household business", spouse_bus_base = "spouse business",
  impatient_base = "Impatient", have_save_base = "has savings", much_saved_base = "amount saved (USD)", mobile_account_base = "mobile money account", mm_distance = "MM agent distance (min)", saving_goal_6 = "saves business",
  hh_income_base = "household income (USD)", total_asset_value_base = "household assets (USD)", consumption_total_base = "household consumption (USD)"
)
balance_rows$variable <- ifelse(balance_rows$variable %in% names(label_map), label_map[balance_rows$variable], balance_rows$variable)

# The original table hard-codes these baseline arm counts in its final Obs row.
balance_rows <- dplyr::bind_rows(
  balance_rows,
  tibble::tibble(
    variable = "Obs", control_mean = 984, account_mean = 993, disburse_mean = 982,
    control_sd = NA_real_, account_sd = NA_real_, disburse_sd = NA_real_,
    p_control_vs_disburse = NA_real_, p_control_vs_account = NA_real_, p_disburse_vs_account = NA_real_,
    joint_p_value = NA_real_, pooled_p_value = NA_real_
  )
)

write_table(balance_rows, file.path(OUT_APP_TABLE, "table_balance"), digits = 3)
