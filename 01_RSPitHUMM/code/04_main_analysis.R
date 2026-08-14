# Main-paper tables and most appendix analyses.
# Translation of the analysis portion of master.do, beginning from survey_data.dta.

survey_all <- read_clean("survey_data.dta")

# -----------------------------------------------------------------------------
# Tables A4-A5: attrition (run before restricting to endline-consented sample)
# -----------------------------------------------------------------------------
survey_all <- survey_all |>
  dplyr::mutate(attrition = as.numeric(is.na(consent) | consent != 1))

m_attr <- fit_fe(survey_all, "attrition", c("treatment2", "treatment3"))
attr_extras <- list(c(
  Observations = stats::nobs(m_attr),
  `R-squared` = model_r2(m_attr),
  `Control mean` = safe_mean(survey_all$attrition[survey_all$treatment1 == 1]),
  `p-value MA=MD` = p_equal(m_attr, "treatment2", "treatment3")
))
write_regression_table(
  list(Attrition = m_attr), c("treatment2", "treatment3"),
  file.path(OUT_APP_TABLE, "table_attrition"),
  model_names = "Attrition",
  term_labels = c(treatment2 = "Mobile Account", treatment3 = "Mobile Disburse"),
  extras = attr_extras, digits = 3
)

attr_data <- survey_all
for (x in c("loan_amount_dis", "weekly_profit", "earn_business", "much_saved", "hh_income")) {
  v <- paste0(x, "_base")
  if (v %in% names(attr_data)) attr_data[[v]] <- attr_data[[v]] / 1000
}
attr_data$treatment2_base <- attr_data$treatment2
attr_data$treatment3_base <- attr_data$treatment3

attr_joint <- c(
  "respondent_age_base", "married_base", "hh_size_base", "completed_primary_base",
  "completed_secondary_base", "work_occupation_base", "loan_amount_dis_base",
  "weekly_profit_base", "high_profits_base", "current_loan_base", "much_saved_base",
  "mobile_account_base", "hyperbolic_base", "impatient_base", "womans_income_share_base",
  "spouse_fam_takes_base"
)
attr_joint <- existing_vars(attr_data, attr_joint)
attr_cc <- attr_data[, c("attrition", attr_joint), drop = FALSE]
attr_cc <- attr_cc[stats::complete.cases(attr_cc), , drop = FALSE]
attr_joint_fit <- stats::lm(
  stats::as.formula(paste("attrition ~", paste(attr_joint, collapse = " + "))),
  data = attr_cc
)
fs <- summary(attr_joint_fit)$fstatistic
attr_joint_p <- if (is.null(fs)) NA_real_ else stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

attr_uni <- paste0(c(
  "treatment2", "treatment3", "respondent_age", "married", "hh_size", "completed_primary",
  "completed_secondary", "work_occupation", "loan_amount_dis", "weekly_profit", "high_profits",
  "current_loan", "much_saved", "mobile_account", "hyperbolic", "impatient",
  "womans_income_share", "above_m_median", "spouse_fam_takes"
), "_base")
attr_uni <- existing_vars(attr_data, attr_uni)
attr_last_n <- NA_real_
attr_rows <- lapply(attr_uni, function(v) {
  m <- fit_ols(attr_data, "attrition", v, robust = TRUE)
  attr_last_n <<- stats::nobs(m)
  tibble::tibble(variable = v, estimate = coef_stat(m, v, "estimate"), robust_se = coef_stat(m, v, "std_error"))
}) |> dplyr::bind_rows()
attr_rows <- dplyr::bind_rows(
  attr_rows,
  tibble::tibble(variable = c("F-test p-value", "Observations"), estimate = c(attr_joint_p, attr_last_n), robust_se = NA_real_)
)
write_table(attr_rows, file.path(OUT_APP_TABLE, "table_attrition2"), digits = 3)

# -----------------------------------------------------------------------------
# Endline analysis sample
# -----------------------------------------------------------------------------
survey <- survey_all |> dplyr::filter(consent == 1)

# Resolve variable-name inconsistencies between the Stata code
# and the distributed survey_data.dta
if (
  !"loan_use_exp" %in% names(survey) &&
  "loan_use_expenditure" %in% names(survey)
) {
  survey$loan_use_exp <- survey$loan_use_expenditure
  message("  using loan_use_expenditure as loan_use_exp")
}

if (
  !"loan_use_sav" %in% names(survey) &&
  "loan_use_save" %in% names(survey)
) {
  survey$loan_use_sav <- survey$loan_use_save
  message("  using loan_use_save as loan_use_sav")
}

term_treat <- c(treatment2 = "Mobile Account", treatment3 = "Mobile Disburse")

run_standard_table <- function(data, outcomes, stem, add_baseline = TRUE, control_filter = NULL, digits = 2) {
  missing_outcomes <- setdiff(outcomes, names(data))
  
  if (length(missing_outcomes) > 0) {
    stop(
      "Required outcomes missing from data: ",
      paste(missing_outcomes, collapse = ", ")
    )
  }
  models <- fit_outcome_models(data, outcomes, add_baseline = add_baseline, extra_rhs = c("treatment2", "treatment3"))
  if (is.null(control_filter)) control_filter <- data$treatment1 == 1
  extras <- extras_for_outcomes(models, data, outcomes, control_filter, include_baseline = add_baseline)
  write_regression_table(
    models, c("treatment2", "treatment3"), stem,
    model_names = outcomes, term_labels = term_treat, extras = extras, digits = digits
  )
  invisible(models)
}

# -----------------------------------------------------------------------------
# Table 1: primary outcomes + Benjamini-Yekutieli FDR q-values
# -----------------------------------------------------------------------------
primary_models <- fit_outcome_models(survey, main_results, add_baseline = TRUE, extra_rhs = c("treatment2", "treatment3"))
primary_q <- by_qvalues(primary_models, c("treatment2", "treatment3"))
primary_extras <- extras_for_outcomes(primary_models, survey, main_results, survey$treatment1 == 1, TRUE)
write_regression_table(
  primary_models, c("treatment2", "treatment3"), file.path(OUT_MAIN_TABLE, "table_primary_results"),
  model_names = main_results, term_labels = term_treat, extras = primary_extras, q_values = primary_q, digits = 2
)

# -----------------------------------------------------------------------------
# Table 2: heterogeneity by self-control and family-pressure median indices
# -----------------------------------------------------------------------------
survey <- survey |>
  dplyr::mutate(
    treatment2_hetero_selfc_median = treatment2 * hetero_selfc_median,
    treatment3_hetero_selfc_median = treatment3 * hetero_selfc_median,
    treatment2_hetero_family_median = treatment2 * hetero_family_median,
    treatment3_hetero_family_median = treatment3 * hetero_family_median
  )

hetero_terms <- c(
  "treatment2", "treatment3",
  "treatment2_hetero_selfc_median", "treatment3_hetero_selfc_median",
  "treatment2_hetero_family_median", "treatment3_hetero_family_median",
  "hetero_family_median"
)
hetero_rhs <- c(
  "treatment2", "treatment3",
  "treatment2_hetero_selfc_median", "treatment3_hetero_selfc_median",
  "treatment2_hetero_family_median", "treatment3_hetero_family_median",
  "hetero_selfc_median", "hetero_family_median"
)
hetero_models <- setNames(lapply(main_results, function(y) {
  fit_fe(survey, y, c(paste0(y, "_base"), hetero_rhs))
}), main_results)
hetero_q <- by_qvalues(hetero_models, hetero_terms[1:6], cap_one = TRUE)
hetero_extras <- Map(function(m, y) c(
  Observations = stats::nobs(m),
  `R-squared` = model_r2(m),
  `Control mean self control` = safe_mean(survey[[y]][survey$treatment == 0 & survey$hetero_selfc_median > 0]),
  `Control mean family pressure` = safe_mean(survey[[y]][survey$treatment == 0 & survey$hetero_family_median > 0]),
  `Control mean baseline self-control` = safe_mean(survey[[paste0(y, "_base")]][survey$treatment == 0 & survey$hetero_selfc_median > 0]),
  `Control mean baseline family pressure` = safe_mean(survey[[paste0(y, "_base")]][survey$treatment == 0 & survey$hetero_family_median > 0]),
  `p-val MD self control=MD family pressure` = p_equal(m, "treatment3_hetero_family_median", "treatment3_hetero_selfc_median"),
  `p-val MA self control=MA family pressure` = p_equal(m, "treatment2_hetero_family_median", "treatment2_hetero_selfc_median")
), hetero_models, main_results)
write_regression_table(
  hetero_models, hetero_terms, file.path(OUT_MAIN_TABLE, "table_hetero_index_family"),
  model_names = main_results,
  term_labels = c(
    treatment2 = "Mobile Account", treatment3 = "Mobile Disburse",
    treatment2_hetero_selfc_median = "MA*self control",
    treatment3_hetero_selfc_median = "MD*self control",
    treatment2_hetero_family_median = "MA*family pressure",
    treatment3_hetero_family_median = "MD*family pressure",
    hetero_family_median = "Family pressure"
  ),
  extras = hetero_extras, q_values = hetero_q, digits = 2
)

# -----------------------------------------------------------------------------
# Tables 3, 4, and 6
# -----------------------------------------------------------------------------
run_standard_table(survey, loan_use_outcomes, file.path(OUT_MAIN_TABLE, "table_second_loan_use"), add_baseline = FALSE, control_filter = survey$treatment == 0)
run_standard_table(survey, give_spouse_outcomes, file.path(OUT_MAIN_TABLE, "table_second_give_spouse"), add_baseline = TRUE, control_filter = survey$treatment == 0)
run_standard_table(survey, household_outcomes, file.path(OUT_MAIN_TABLE, "table_second_household"), add_baseline = TRUE, control_filter = survey$treatment == 0)

# -----------------------------------------------------------------------------
# Table 5: stated preferences and subsequent-loan mobile-money deposits
# -----------------------------------------------------------------------------
subseq_models <- list()
subseq_extras <- list()

m <- fit_fe(survey, "mm_offered", c("treatment2", "treatment3"))
subseq_models[["Want mobile deposit"]] <- m
subseq_extras[[1]] <- c(
  Observations = stats::nobs(m), `R-squared` = model_r2(m),
  `Control mean` = safe_mean(survey$mm_offered[survey$treatment1 == 1]),
  `p-value MA=MD` = p_equal(m, "treatment2", "treatment3")
)
m <- fit_fe(survey, "mm_offered", c("treatment2", "treatment3", "treatment2_hetero_family_median", "treatment3_hetero_family_median", "hetero_family_median"))
subseq_models[["Want mobile deposit x family pressure"]] <- m
subseq_extras[[2]] <- c(
  Observations = stats::nobs(m), `R-squared` = model_r2(m),
  `Control mean` = safe_mean(survey$mm_offered[survey$treatment1 == 1 & survey$hetero_family_median == 1]),
  `p-value MA=MD` = p_equal(m, "treatment2", "treatment3")
)

brac_mm <- read_clean("BRAC_MM_merged.dta") |>
  dplyr::mutate(
    treatment2_hetero_family_median = treatment2 * hetero_family_median,
    treatment3_hetero_family_median = treatment3 * hetero_family_median
  )
for (y in c("deposit_any", "deposit_max", "deposit_share")) {
  m1 <- fit_fe(brac_mm, y, "treatment3")
  subseq_models[[paste(y, "main")]] <- m1
  subseq_extras[[length(subseq_extras) + 1L]] <- c(
    Observations = stats::nobs(m1), `R-squared` = model_r2(m1),
    `Control mean` = safe_mean(brac_mm[[y]][brac_mm$treatment2 == 1])
  )
  m2 <- fit_fe(brac_mm, y, c("treatment3", "treatment3_hetero_family_median", "hetero_family_median"))
  subseq_models[[paste(y, "family pressure")]] <- m2
  subseq_extras[[length(subseq_extras) + 1L]] <- c(
    Observations = stats::nobs(m2), `R-squared` = model_r2(m2),
    `Control mean` = safe_mean(brac_mm[[y]][brac_mm$treatment2 == 1 & brac_mm$hetero_family_median == 1])
  )
}
write_regression_table(
  subseq_models,
  c("treatment2", "treatment3", "treatment2_hetero_family_median", "treatment3_hetero_family_median"),
  file.path(OUT_MAIN_TABLE, "table_subsequent"),
  model_names = names(subseq_models),
  term_labels = c(
    treatment2 = "Mobile Account", treatment3 = "Mobile Disburse",
    treatment2_hetero_family_median = "MA*family pressure",
    treatment3_hetero_family_median = "MD*family pressure"
  ),
  extras = subseq_extras, digits = 2
)

# -----------------------------------------------------------------------------
# Appendix Figures A1-A4 and Table A1
# -----------------------------------------------------------------------------
if (RUN_GRAPHS) {
  p <- ggplot(survey, aes(x = loan_amount_dis_base)) +
    geom_histogram(bins = 30, boundary = 0) +
    labs(x = "Loan Size (USD)", y = "Count", title = "Loan Size Distribution") +
    theme_minimal(base_size = 11)
  save_plot(p, file.path(OUT_APP_FIG, "hist_disbursed.png"))

  business_plot <- survey |>
    dplyr::mutate(business_type = haven::as_factor(business_type_base)) |>
    dplyr::count(business_type, name = "frequency")
  p <- ggplot(business_plot, aes(x = reorder(business_type, frequency), y = frequency)) +
    geom_col() + coord_flip() +
    labs(x = NULL, y = "frequency") + theme_minimal(base_size = 10)
  save_plot(p, file.path(OUT_APP_FIG, "business_types.png"), width = 8, height = 6)

  for (y in main_results) {
    d <- cdf_frame(survey, y)
    d$treatment <- factor(d$treatment, levels = c(0, 1, 2), labels = c("Control", "Account", "Disburse"))
    p <- ggplot(d, aes(value, percentile, linetype = treatment)) + geom_line() +
      labs(x = y, y = "Percentile", linetype = NULL) + theme_minimal(base_size = 11)
    save_plot(p, file.path(OUT_APP_FIG, paste0("CDF_", y, ".png")))

    ln_name <- paste0("ln_", y)
    survey[[ln_name]] <- ifelse(survey[[y]] > 0, log(survey[[y]]), NA_real_)
    dln <- cdf_frame(survey, ln_name)
    dln$treatment <- factor(dln$treatment, levels = c(0, 1, 2), labels = c("Control", "Account", "Disburse"))
    p <- ggplot(dln, aes(value, percentile, linetype = treatment)) + geom_line() +
      labs(x = paste("log", y), y = "Percentile", linetype = NULL) + theme_minimal(base_size = 11)
    save_plot(p, file.path(OUT_APP_FIG, paste0("CDF_ln_", y, ".png")))
  }
}

# Kolmogorov-Smirnov tests printed by the original graph block.
ks_rows <- list()
for (y in main_results) {
  for (cmp in list(c(1, 2), c(0, 2), c(0, 1))) {
    a <- survey[[y]][survey$treatment == cmp[1]]
    b <- survey[[y]][survey$treatment == cmp[2]]
    a <- a[is.finite(a)]; b <- b[is.finite(b)]
    kt <- if (length(a) && length(b)) suppressWarnings(stats::ks.test(a, b)) else NULL
    ks_rows[[length(ks_rows) + 1L]] <- tibble::tibble(
      outcome = y, group_a = cmp[1], group_b = cmp[2],
      statistic = if (is.null(kt)) NA_real_ else unname(kt$statistic),
      p_value = if (is.null(kt)) NA_real_ else kt$p.value
    )
  }
}
write_table(dplyr::bind_rows(ks_rows), file.path(OUT_APP_TABLE, "ks_tests_primary_outcomes"), digits = 4)

switch_tab <- survey |> dplyr::count(switch_m, name = "n") |> dplyr::mutate(percent = 100 * n / sum(n))
write_table(switch_tab, file.path(OUT_APP_TABLE, "table_A1_switch_m_counts"), digits = 2)

# -----------------------------------------------------------------------------
# Tables A6-A10: secondary outcomes
# -----------------------------------------------------------------------------
run_standard_table(survey, profit_outcomes, file.path(OUT_APP_TABLE, "table_second_profit"), TRUE, survey$treatment == 0)
run_standard_table(survey, savings_outcomes, file.path(OUT_APP_TABLE, "table_second_savings"), TRUE, survey$treatment == 0)
run_standard_table(survey, asset_outcomes, file.path(OUT_APP_TABLE, "table_second_assets"), TRUE, survey$treatment == 0)
run_standard_table(survey, labour_outcomes, file.path(OUT_APP_TABLE, "table_second_labour"), TRUE, survey$treatment == 0)

bus_outcomes <- existing_vars(survey, paste0("bus", 1:17))
bus_models <- fit_outcome_models(survey, bus_outcomes, TRUE, c("treatment2", "treatment3"))
if ("change_business" %in% names(survey)) {
  bus_models[["change_business"]] <- fit_fe(survey, "change_business", c("treatment2", "treatment3"))
}
bus_extras <- lapply(names(bus_models), function(y) {
  m <- bus_models[[y]]
  if (y == "change_business") {
    c(Observations = stats::nobs(m), `R-squared` = model_r2(m),
      `Control mean` = safe_mean(survey[[y]][survey$treatment1 == 1]),
      `p-value MA=MD` = p_equal(m, "treatment2", "treatment3"))
  } else {
    standard_extras(m, survey, y, paste0(y, "_base"), survey$treatment1 == 1)
  }
})
if (length(bus_models)) write_regression_table(
  bus_models, c("treatment2", "treatment3"), file.path(OUT_APP_TABLE, "table_second_bus_type"),
  model_names = names(bus_models), term_labels = term_treat, extras = bus_extras, digits = 3
)

# -----------------------------------------------------------------------------
# Table A11: winsorization robustness
# -----------------------------------------------------------------------------
winsor_specs <- c(`No winsorizing` = "100", `Winsorizing 99.5%` = "995", `Winsorizing 98%` = "98", `Winsorizing 95%` = "95")
winsor_tabs <- list()
for (panel in names(winsor_specs)) {
  suf <- winsor_specs[[panel]]
  outcomes <- paste0(main_results, "_", suf)
  models <- setNames(lapply(seq_along(main_results), function(i) {
    fit_fe(survey, outcomes[i], c(paste0(outcomes[i], "_base"), "treatment2", "treatment3"))
  }), main_results)
  extras <- Map(function(m, y, oy) standard_extras(m, survey, oy, paste0(oy, "_base"), survey$treatment1 == 1), models, main_results, outcomes)
  tab <- make_regression_table(models, c("treatment2", "treatment3"), main_results, term_treat, extras)
  tab <- dplyr::mutate(tab, Panel = panel, .before = 1)
  winsor_tabs[[panel]] <- tab
}
write_table(dplyr::bind_rows(winsor_tabs), file.path(OUT_APP_TABLE, "table_robust_winsor"), digits = 2)

# -----------------------------------------------------------------------------
# Table A12: robustness controls
# -----------------------------------------------------------------------------
robust_specs <- list(
  `Controlling for imbalanced variables at baseline` = c("mobile_account_base", "completed_secondary_base", "hyperbolic_base"),
  `Controlling for linear and quadratic time trend` = existing_vars(survey, days_vars),
  `Controlling for correlates of takeup` = c("married_base", "own_decision_base")
)
robust_tabs <- list()
for (panel in names(robust_specs)) {
  extra <- existing_vars(survey, robust_specs[[panel]])
  models <- setNames(lapply(main_results, function(y) {
    fit_fe(survey, y, c(paste0(y, "_base"), "treatment2", "treatment3", extra))
  }), main_results)
  extras <- extras_for_outcomes(models, survey, main_results, survey$treatment1 == 1, TRUE)
  tab <- make_regression_table(models, c("treatment2", "treatment3"), main_results, term_treat, extras)
  robust_tabs[[panel]] <- dplyr::mutate(tab, Panel = panel, .before = 1)
}
write_table(dplyr::bind_rows(robust_tabs), file.path(OUT_APP_TABLE, "table_robust"), digits = 2)

# -----------------------------------------------------------------------------
# Table A13: log primary outcomes
# -----------------------------------------------------------------------------
for (y in main_results) {
  if (!paste0("ln_", y) %in% names(survey)) survey[[paste0("ln_", y)]] <- ifelse(survey[[y]] > 0, log(survey[[y]]), NA_real_)
  if (!paste0("ln_", y, "_base") %in% names(survey)) survey[[paste0("ln_", y, "_base")]] <- ifelse(survey[[paste0(y, "_base")]] > 0, log(survey[[paste0(y, "_base")]]), NA_real_)
}
log_models <- setNames(lapply(main_results, function(y) {
  fit_fe(survey, paste0("ln_", y), c(paste0("ln_", y, "_base"), "treatment2", "treatment3"))
}), main_results)
log_extras <- Map(function(m, y) c(
  Observations = stats::nobs(m), `R-squared` = model_r2(m),
  `Control mean` = safe_mean(survey[[y]][survey$treatment1 == 1]),
  `Control mean baseline` = safe_mean(survey[[paste0(y, "_base")]][survey$treatment1 == 1]),
  `p-value MA=MD` = p_equal(m, "treatment2", "treatment3")
), log_models, main_results)
write_regression_table(log_models, c("treatment2", "treatment3"), file.path(OUT_APP_TABLE, "table_primary_logs"), main_results, term_treat, log_extras, digits = 2)

# -----------------------------------------------------------------------------
# Table A14: permutation p-values. The Stata permute commands intentionally run
# y ~ treatment2 + treatment3 without baseline controls or strata FE, while
# permuting one treatment dummy at a time within strata.
# -----------------------------------------------------------------------------
if (RUN_PERMUTATION) {
  perm_list <- setNames(vector("list", length(primary_models)), names(primary_models))
  for (i in seq_along(main_results)) {
    y <- main_results[i]
    perm_list[[i]] <- c(
      treatment2 = permutation_pvalue(survey, y, "treatment2", "treatment3", reps = PERM_REPS, seed = 9999L),
      treatment3 = permutation_pvalue(survey, y, "treatment3", "treatment2", reps = PERM_REPS, seed = 9999L)
    )
  }
  write_regression_table(
    primary_models, c("treatment2", "treatment3"), file.path(OUT_APP_TABLE, "table_primary_perm"),
    main_results, term_treat, primary_extras, permutation_p = perm_list, digits = 3
  )
}

# The two `multe ... est(OWN) diff` commands in master.do are not translated here.
# They depend on an external Stata package whose estimation algorithm was not part
# of the supplied replication files. This omission affects text-only contamination
# diagnostics, not the named paper tables/figures generated below.
writeLines(
  c(
    "Not reproduced: Stata multe contamination-bias diagnostics for earn_business and capital.",
    "Reason: the substantive multe implementation was not included among the supplied source files."
  ),
  file.path(OUT_APP_TABLE, "contamination_bias_NOT_REPLICATED.txt")
)

# -----------------------------------------------------------------------------
# Table A22: heterogeneity by spouse presence
# -----------------------------------------------------------------------------
survey <- survey |>
  dplyr::mutate(
    treatment2_no_spouse_home = treatment2 * no_spouse_home_base,
    treatment3_no_spouse_home = treatment3 * no_spouse_home_base
  )
spouse_models <- setNames(lapply(c("earn_business", "capital"), function(y) {
  fit_fe(survey, y, c(paste0(y, "_base"), "treatment2", "treatment3", "treatment2_no_spouse_home", "treatment3_no_spouse_home", "no_spouse_home_base"))
}), c("earn_business", "capital"))
spouse_extras <- Map(function(m, y) c(
  Observations = stats::nobs(m), `R-squared` = model_r2(m),
  `Control mean` = safe_mean(survey[[y]][survey$treatment1 == 1]),
  `Control mean baseline` = safe_mean(survey[[paste0(y, "_base")]][survey$treatment1 == 1]),
  `p-value MA=MD` = p_equal(m, "treatment2", "treatment3"),
  `p-value MA=MD interaction` = p_equal(m, "treatment2_no_spouse_home", "treatment3_no_spouse_home")
), spouse_models, names(spouse_models))
write_regression_table(
  spouse_models,
  c("treatment2", "treatment3", "treatment2_no_spouse_home", "treatment3_no_spouse_home", "no_spouse_home_base"),
  file.path(OUT_APP_TABLE, "table_spouse_present"),
  names(spouse_models),
  c(
    treatment2 = "Mobile Account", treatment3 = "Mobile Disburse",
    treatment2_no_spouse_home = "Mobile Account * no spouse at home",
    treatment3_no_spouse_home = "Mobile Disburse * no spouse at home",
    no_spouse_home_base = "No spouse at home"
  ), spouse_extras, digits = 2
)

# -----------------------------------------------------------------------------
# Tables A25-A30: additional secondary outcomes
# -----------------------------------------------------------------------------
additional_groups <- list(
  earnings = earnings_outcomes,
  happy = happy_outcomes,
  empower_all = empower_outcomes,
  records = records_outcomes,
  remittance = remittance_outcomes,
  group = group_outcomes
)
for (root in names(additional_groups)) {
  outcomes <- existing_vars(survey, additional_groups[[root]])
  models <- setNames(lapply(outcomes, function(y) {
    rhs <- c(if (paste0(y, "_base") %in% names(survey)) paste0(y, "_base"), "treatment2", "treatment3")
    fit_fe(survey, y, rhs)
  }), outcomes)
  extras <- Map(function(m, y) {
    ans <- c(
      Observations = stats::nobs(m), `R-squared` = model_r2(m),
      `Control mean endline` = safe_mean(survey[[y]][survey$treatment == 0]),
      `p-value MA=MD` = p_equal(m, "treatment2", "treatment3")
    )
    if (paste0(y, "_base") %in% names(survey)) ans <- append(ans, c(`Control mean baseline` = safe_mean(survey[[paste0(y, "_base")]][survey$treatment == 0])), after = 3)
    ans
  }, models, outcomes)
  if (length(models)) write_regression_table(
    models, c("treatment2", "treatment3"), file.path(OUT_APP_TABLE, paste0("table_second_", root)),
    outcomes, term_treat, extras, digits = 2
  )
}

# -----------------------------------------------------------------------------
# Table A31: BRAC administrative outcomes
# -----------------------------------------------------------------------------
brac <- read_clean("BRAC admin data.dta")
brac_outcomes <- existing_vars(brac, c("missed_payment", "missed_days_brac", "principal_outstanding_brac", "interest_outstanding_brac", "savings_amt_brac", "overdue_amount_brac"))
brac_models <- setNames(lapply(brac_outcomes, function(y) fit_fe(brac, y, c("treatment2", "treatment3"))), brac_outcomes)
brac_extras <- Map(function(m, y) c(
  Observations = stats::nobs(m), `R-squared` = model_r2(m),
  `Control mean` = safe_mean(brac[[y]][brac$treatment1 == 1]),
  `p-value MA=MD` = p_equal(m, "treatment2", "treatment3")
), brac_models, brac_outcomes)
write_regression_table(brac_models, c("treatment2", "treatment3"), file.path(OUT_APP_TABLE, "table_brac_admin"), brac_outcomes, term_treat, brac_extras, digits = 2)

# -----------------------------------------------------------------------------
# Tables A32-A34: heterogeneity across individual baseline dimensions
# -----------------------------------------------------------------------------
for (y in main_results) {
  cols <- list()
  for (h in existing_vars(survey, hetero_vars)) {
    d <- survey
    d$temp <- d[[h]]
    d$treatment2_temp <- d$treatment2 * d$temp
    d$treatment3_temp <- d$treatment3 * d$temp
    m <- fit_fe(d, y, c(paste0(y, "_base"), "treatment2_temp", "treatment3_temp", "temp", "treatment2", "treatment3"))
    extras <- c(
      Observations = stats::nobs(m), `R-squared` = model_r2(m),
      `Control mean` = safe_mean(d[[y]][d$treatment == 0 & d$temp == 1]),
      `Interaction mean` = safe_mean(d$temp),
      `MA=MD` = p_equal(m, "treatment2", "treatment3"),
      `MA=MD interaction` = p_sum_equal(m, c("treatment2", "treatment2_temp"), c("treatment3", "treatment3_temp"))
    )
    one <- make_regression_table(
      list(m), c("treatment2", "treatment3", "treatment2_temp", "treatment3_temp"),
      model_names = h,
      term_labels = c(
        treatment2 = "Mobile Account", treatment3 = "Mobile Disburse",
        treatment2_temp = "MA*interaction", treatment3_temp = "MD*interaction"
      ), extras = list(extras)
    )
    cols[[h]] <- one
  }
  # Merge each heterogeneity specification by row label to match outreg2's column layout.
  if (length(cols)) {
    merged <- Reduce(function(a, b) dplyr::full_join(a, b, by = "Row"), cols)
    write_table(merged, file.path(OUT_APP_TABLE, paste0("table_hetero_", y)), digits = 2)
  }
}

# -----------------------------------------------------------------------------
# Figure 2 and Tables A15-A20: mobile-money administrative usage
# -----------------------------------------------------------------------------
mm_bal <- read_clean("MM_balances.dta")
if (RUN_GRAPHS) {
  plot_bal <- mm_bal |> dplyr::filter(treatment %in% c(1, 2)) |>
    dplyr::mutate(treatment = factor(treatment, levels = c(1, 2), labels = c("Mobile account", "Mobile disbursement")))
  p <- ggplot(plot_bal, aes(days, balance_loan, shape = treatment)) +
    geom_point(alpha = 0.6, size = 1.2) +
    labs(x = "Days", y = "% of initial loan value", title = "Average mobile money account daily balance", shape = NULL) +
    theme_minimal(base_size = 11)
  save_plot(p, file.path(OUT_MAIN_FIG, "balances.png"), width = 8, height = 5)
}

mm_use <- read_clean("MM_useage.dta")
summary_vars <- existing_vars(mm_use, c("ever_deposit", "ever_withdrawal", "number_deposit", "number_withdrawal", "av_deposit", "av_withdrawal", "total_deposit", "total_withdrawal", "withdrew_perc", "withdrew_disburse"))
mm_summary <- mm_use |>
  dplyr::filter(treatment %in% c(1, 2)) |>
  dplyr::group_by(treatment) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(summary_vars), list(mean = safe_mean, sd = safe_sd, median = safe_median)), .groups = "drop") |>
  tidyr::pivot_longer(-treatment, names_to = c("variable", ".value"), names_pattern = "(.*)_(mean|sd|median)$") |>
  tidyr::pivot_wider(names_from = treatment, values_from = c(mean, sd, median), names_glue = "treatment{treatment}_{.value}")
write_table(mm_summary, file.path(OUT_APP_TABLE, "table_int_summary"), digits = 2)

if (RUN_GRAPHS && all(paste0("month", 2:6) %in% names(mm_use))) {
  d <- mm_use |> dplyr::filter(treatment == 1)
  m <- fit_ols(d, "total_deposit", paste0("month", 2:6), robust = FALSE, intercept = FALSE)
  ct <- coef_table(m) |> dplyr::filter(term %in% paste0("month", 2:6)) |>
    dplyr::mutate(month = as.integer(stringr::str_remove(term, "month")), lo = estimate - 1.96 * std_error, hi = estimate + 1.96 * std_error)
  p <- ggplot(ct, aes(month, estimate)) + geom_point() + geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
    scale_x_continuous(breaks = 2:6) +
    labs(x = "Month of loan disbursement", y = "Total deposits to account by month of opening USD") + theme_minimal(base_size = 11)
  save_plot(p, file.path(OUT_APP_FIG, "MA_time2.png"))
}

mm_use_outcomes <- existing_vars(mm_use, c("ever_deposit", "number_deposit", "av_deposit", "total_deposit", "ever_withdrawal", "number_withdrawal", "av_withdrawal", "total_withdrawal"))
int3_models <- setNames(lapply(mm_use_outcomes, function(y) fit_fe(mm_use, y, "treatment_2")), mm_use_outcomes)
int3_extras <- Map(function(m, y) c(Observations = stats::nobs(m), `R-squared` = model_r2(m), `Mobile account mean` = safe_mean(mm_use[[y]][mm_use$treatment == 1])), int3_models, mm_use_outcomes)
write_regression_table(int3_models, "treatment_2", file.path(OUT_APP_TABLE, "table_int3_results"), mm_use_outcomes, c(treatment_2 = "Mobile Disburse"), int3_extras, digits = 2)

balance_outcomes <- existing_vars(mm_use, c("av_balance7", "av_balance15", "av_balance30", "av_balance45", "av_balance60", "av_balance90", "av_balance180", "final_balance"))
int2_models <- setNames(lapply(balance_outcomes, function(y) fit_fe(mm_use, y, "treatment_2")), balance_outcomes)
int2_extras <- Map(function(m, y) c(Observations = stats::nobs(m), `R-squared` = model_r2(m), `Mobile account mean` = safe_mean(mm_use[[y]][mm_use$treatment == 1])), int2_models, balance_outcomes)
write_regression_table(int2_models, "treatment_2", file.path(OUT_APP_TABLE, "table_int2_results"), balance_outcomes, c(treatment_2 = "Mobile Disburse"), int2_extras, digits = 2)

mm_use$hetero_family_median_treat_2 <- mm_use$hetero_family_median * mm_use$treatment_2
inthet_models <- setNames(lapply(balance_outcomes, function(y) fit_fe(mm_use, y, c("treatment_2", "hetero_family_median_treat_2", "hetero_family_median"))), balance_outcomes)
inthet_extras <- Map(function(m, y) c(Observations = stats::nobs(m), `R-squared` = model_r2(m), `Mobile account mean` = safe_mean(mm_use[[y]][mm_use$treatment == 1])), inthet_models, balance_outcomes)
write_regression_table(
  inthet_models, c("treatment_2", "hetero_family_median_treat_2", "hetero_family_median"),
  file.path(OUT_APP_TABLE, "table_int_hetero"), balance_outcomes,
  c(treatment_2 = "Mobile Disburse", hetero_family_median_treat_2 = "MD*family pressure", hetero_family_median = "Family pressure"),
  inthet_extras, digits = 2
)

# Table A18 and Figure A5: transaction types.
mm_trans <- read_clean("MM_transactions.dta")
transfer_labels <- c(`1` = "Cash in", `2` = "Cash out", `3` = "Debit", `4` = "Payment", `5` = "Reversal", `6` = "Transfer In", `7` = "Transfer Out")
if (RUN_GRAPHS) {
  trans_fig <- mm_trans |> dplyr::filter(treatment %in% c(1, 2), !is.na(transfer_type)) |>
    dplyr::count(treatment, transfer_type, name = "n") |>
    dplyr::group_by(treatment) |>
    dplyr::mutate(percent = 100 * n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      treatment = factor(treatment, levels = c(1, 2), labels = c("Mobile account", "Mobile disbursement")),
      transfer = factor(transfer_type, levels = 1:7, labels = unname(transfer_labels))
    )
  p <- ggplot(trans_fig, aes(transfer, percent, fill = treatment)) +
    geom_col(position = "identity", alpha = 0.55) +
    labs(x = "Transfer type", y = "Percent", fill = NULL) +
    theme_minimal(base_size = 10) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
  save_plot(p, file.path(OUT_APP_FIG, "trans_type.png"), width = 8.5, height = 5.5)
}

trans_table <- mm_trans |>
  dplyr::filter(treatment %in% c(1, 2)) |>
  dplyr::group_by(treatment, transfer_type) |>
  dplyr::summarise(mean = safe_mean(AMOUNT), sd = safe_sd(AMOUNT), count = sum(!is.na(AMOUNT)), .groups = "drop") |>
  dplyr::mutate(transfer_type = dplyr::recode(as.character(transfer_type), !!!transfer_labels)) |>
  tidyr::pivot_wider(names_from = treatment, values_from = c(mean, sd, count), names_glue = "treatment{treatment}_{.value}")
write_table(trans_table, file.path(OUT_APP_TABLE, "table_trans_type"), digits = 2)
