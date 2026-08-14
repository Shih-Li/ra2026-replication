# Tables A23-A24: k-means heterogeneity.
# Translation of k-means.do.

km <- read_clean("survey_data.dta") |>
  dplyr::filter(consent == 1)

for (v in c("spouse_allearning_base", "switch_8_base")) {
  if (v %in% names(km)) km[[v]][is.na(km[[v]])] <- 0
}

km_vars <- c(
  "respondent_age_base", "married_base", "education_level_base", "work_occupation_base",
  "earn_business_base", "hh_size_base", "year_business_base", "own_business_1_base",
  "own_business_2_base", "sales_base", "expenditure_base", "non_hhemployee_base",
  "employee_hours_base", "capital_base", "current_loan_base", "consumption_total_base",
  "have_save_base", "much_saved_base", "control_money_base", "spouse_fam_takes_base",
  "womans_income_share_base", "hh_bus_base", "hyperbolic_base", "impatient_base",
  "saving_goal_6_base", "spouse_allearning_base", "own_decision_base", "switch_8_base"
)
missing_km <- setdiff(km_vars, names(km))
if (length(missing_km)) stop("Missing k-means variables: ", paste(missing_km, collapse = ", "))

km_complete <- stats::complete.cases(km[, km_vars, drop = FALSE])
X_raw <- as.matrix(km[km_complete, km_vars, drop = FALSE])
X <- scale(X_raw)
X[!is.finite(X)] <- 0

km_fits <- vector("list", 10)
wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  # Lloyd is used to stay close to the classic k-means implementation underlying
  # Stata's cluster kmeans. Cluster numbering can still differ across software.
  fit <- stats::kmeans(X, centers = k, iter.max = 100, nstart = 1, algorithm = "Lloyd")
  km_fits[[k]] <- fit
  wss[k] <- fit$tot.withinss
}

wss_table <- tibble::tibble(
  k = 1:10,
  WSS = wss,
  log_WSS = log(wss),
  eta_squared = 1 - wss / wss[1],
  PRE = c(NA_real_, (wss[-10] - wss[-1]) / wss[-10])
)
write_table(wss_table, file.path(OUT_APP_TABLE, "kmeans_WSS_diagnostics"), digits = 4)

# k = 4 is declared optimal in the authors' source.
km$cs4 <- NA_integer_
km$cs4[km_complete] <- km_fits[[4]]$cluster

# The source resets switch_8_base to missing for unmarried women only after clustering,
# before producing descriptive cluster summaries.
km_summary_data <- km
km_summary_data$switch_8_base[
  km_summary_data$married_base == 0
] <- NA_real_

cluster_summary_list <- lapply(1:4, function(g) {
  d <- km_summary_data[!is.na(km_summary_data$cs4) & km_summary_data$cs4 == g, km_vars, drop = FALSE]
  tibble::tibble(
    variable = km_vars,
    !!paste0("cluster", g, "_mean") := vapply(d, safe_mean, numeric(1)),
    !!paste0("cluster", g, "_sd") := vapply(d, safe_sd, numeric(1))
  )
})
cluster_summary <- Reduce(function(a, b) dplyr::full_join(a, b, by = "variable"), cluster_summary_list)
write_table(cluster_summary, file.path(OUT_APP_TABLE, "table_cluster_summary"), digits = 2)

# Generate cluster dummies and treatment interactions. Cluster 4 is the reference,
# matching the Stata regression that includes cluster 1-3 indicators/interactions.
for (g in 1:4) {
  km[[paste0("cs4_", g)]] <- as.numeric(km$cs4 == g)
  km[[paste0("treatment2_cs4_", g)]] <- km$treatment2 * km[[paste0("cs4_", g)]]
  km[[paste0("treatment3_cs4_", g)]] <- km$treatment3 * km[[paste0("cs4_", g)]]
}

cluster_rhs <- c(
  "treatment2",
  "treatment3",
  paste0("treatment2_cs4_", 1:3),
  paste0("treatment3_cs4_", 1:3),
  paste0("cs4_", 1:3),
  "days",
  "days2"
)
cluster_models <- setNames(lapply(main_results, function(y) fit_fe(km, y, cluster_rhs)), main_results)

cluster_extras <- Map(function(m, y) {
  means <- vapply(1:4, function(g) safe_mean(km[[paste0(y, "_base")]][km$treatment1 == 1 & km[[paste0("cs4_", g)]] == 1]), numeric(1))
  c(
    Observations = stats::nobs(m), `R-squared` = model_r2(m),
    `Mean group 1` = means[1], `Mean group 2` = means[2], `Mean group 3` = means[3], `Mean group 4` = means[4],
    `p-value MA=MD` = p_equal(m, "treatment2", "treatment3"),
    `p-value group 1*MA=group 2*MA` = p_equal(m, "treatment2_cs4_1", "treatment2_cs4_2"),
    `p-value group 1*MA=group 3*MA` = p_equal(m, "treatment2_cs4_1", "treatment2_cs4_3"),
    `p-value group 2*MA=group 3*MA` = p_equal(m, "treatment2_cs4_2", "treatment2_cs4_3"),
    `p-value group 1*MD=group 2*MD` = p_equal(m, "treatment3_cs4_1", "treatment3_cs4_2"),
    `p-value group 1*MD=group 3*MD` = p_equal(m, "treatment3_cs4_1", "treatment3_cs4_3"),
    `p-value group 2*MD=group 3*MD` = p_equal(m, "treatment3_cs4_2", "treatment3_cs4_3")
  )
}, cluster_models, main_results)

write_regression_table(
  cluster_models,
  c("treatment2", "treatment3", paste0("treatment2_cs4_", 1:3), paste0("treatment3_cs4_", 1:3), paste0("cs4_", 1:3)),
  file.path(OUT_APP_TABLE, "table_cluster_hetero"),
  model_names = main_results,
  term_labels = c(
    treatment2 = "Mobile Account", treatment3 = "Mobile Disburse",
    treatment2_cs4_1 = "group 1*MA", treatment2_cs4_2 = "group 2*MA", treatment2_cs4_3 = "group 3*MA",
    treatment3_cs4_1 = "group 1*MD", treatment3_cs4_2 = "group 2*MD", treatment3_cs4_3 = "group 3*MD",
    cs4_1 = "group 1", cs4_2 = "group 2", cs4_3 = "group 3"
  ),
  extras = cluster_extras, digits = 2
)

utils::write.csv(km[, c("id_key_new", "cs4")], file.path(OUT_APP_TABLE, "kmeans_cluster_assignments.csv"), row.names = FALSE)
