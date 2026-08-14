# ============================================================
# 04_tables.R
# Main and extended-data Tables 1-8 and 10
# ============================================================

individual <- read_clean("individual_level.dta")
community <- read_clean("community_data.dta")

fmt_cell <- function(model, term) {
  b <- coef_stat(model, term, "estimate")
  p <- coef_stat(model, term, "p_value")
  if (!is.finite(b)) return("")
  paste0(fmt_num(b), stars(p))
}

fmt_se_cell <- function(model, term) {
  se <- coef_stat(model, term, "std_error")
  if (!is.finite(se)) return("")
  paste0("(", fmt_num(se), ")")
}

first_nonmissing <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) NA else y[1]
}

# ------------------------------------------------------------
# Table 1: ITT effect of door-to-door and small-group treatments
# ------------------------------------------------------------
t1 <- merge_community_covariates(individual, community) |>
  dplyr::filter(above18 == 1, in_census == 1) |>
  dplyr::mutate(
    treat_dtd = as.numeric(treatment == 1),
    treat_small = as.numeric(treatment == 2)
  )

m1 <- fit_fe_model(t1, "vaccinated_endline", c("treat_dtd", "treat_small"), fe = "grpID", cluster = "community_code")
m2 <- fit_fe_model(
  t1,
  "vaccinated_endline",
  c("treat_dtd", "treat_small", COMMUNITY_COVARIATES),
  fe = "grpID",
  cluster = "community_code"
)

t1_dtd <- t1 |> dplyr::filter(treatment == 1, periphery == 0)
m3 <- fit_fe_model(t1_dtd, "vaccinated_endline", "dtd_treatment", fe = "community_code", cluster = "structure_id")

mask1 <- complete_mask(t1, c("vaccinated_endline", "treat_dtd", "treat_small", "grpID", "community_code"))
mask2 <- complete_mask(t1, c("vaccinated_endline", "treat_dtd", "treat_small", COMMUNITY_COVARIATES, "grpID", "community_code"))
mask3 <- complete_mask(t1_dtd, c("vaccinated_endline", "dtd_treatment", "community_code", "structure_id"))

p12_1 <- linear_combo(m1, c(treat_dtd = 1, treat_small = -1))["p_value"]
p12_2 <- linear_combo(m2, c(treat_dtd = 1, treat_small = -1))["p_value"]

model_names <- c("(1)", "(2)", "(3)")
table1 <- tibble::tribble(
  ~Row, ~`(1)`, ~`(2)`, ~`(3)`,
  "Door-to-Door", fmt_cell(m1, "treat_dtd"), fmt_cell(m2, "treat_dtd"), fmt_cell(m3, "dtd_treatment"),
  "", fmt_se_cell(m1, "treat_dtd"), fmt_se_cell(m2, "treat_dtd"), fmt_se_cell(m3, "dtd_treatment"),
  "Small-Group", fmt_cell(m1, "treat_small"), fmt_cell(m2, "treat_small"), "",
  "", fmt_se_cell(m1, "treat_small"), fmt_se_cell(m2, "treat_small"), "",
  "Proportion Vaccinated at Baseline", "", fmt_cell(m2, "vaccinated_baseline_18"), "",
  "", "", fmt_se_cell(m2, "vaccinated_baseline_18"), "",
  "Additional Covariates", "No", "Yes", "No",
  "Observations", as.character(stats::nobs(m1)), as.character(stats::nobs(m2)), as.character(stats::nobs(m3)),
  "Mean in Control",
    fmt_num(safe_mean(t1$vaccinated_endline[mask1 & t1$treatment == 0])),
    fmt_num(safe_mean(t1$vaccinated_endline[mask2 & t1$treatment == 0])),
    fmt_num(safe_mean(t1_dtd$vaccinated_endline[mask3 & t1_dtd$dtd_treatment == 0])),
  "No. of Villages",
    as.character(safe_n_unique(t1$community_code[mask1])),
    as.character(safe_n_unique(t1$community_code[mask2])),
    as.character(safe_n_unique(t1_dtd$community_code[mask3])),
  "No. of Structures",
    as.character(safe_n_unique(t1$structure_id[mask1])),
    as.character(safe_n_unique(t1$structure_id[mask2])),
    as.character(safe_n_unique(t1_dtd$structure_id[mask3])),
  "P(Door-to-Door = Small-Group)", fmt_num(p12_1), fmt_num(p12_2), "",
  "R-squared", fmt_num(model_r2(m1)), fmt_num(model_r2(m2)), fmt_num(model_r2(m3))
)
write_table(table1, file.path(OUT_MAIN_TABLE, "itt_over_treatments"))

# ------------------------------------------------------------
# Table 2: baseline descriptive statistics and statistical balance
# ------------------------------------------------------------
t2 <- community |>
  dplyr::select(
    community_code, treatment, grpID, any_treat,
    dplyr::all_of(CENSUS_2015_VARS), dplyr::all_of(VILLAGE_CENSUS_VARS)
  ) |>
  dplyr::rename_with(~ paste0("VC_", .x), dplyr::all_of(VILLAGE_CENSUS_VARS)) |>
  dplyr::mutate(
    treat_dtd = as.numeric(treatment == 1),
    treat_small = as.numeric(treatment == 2)
  )

VC_VARS <- paste0("VC_", VILLAGE_CENSUS_VARS)
BALANCE_VARS <- c(CENSUS_2015_VARS, VC_VARS)

balance_labels <- setNames(vapply(BALANCE_VARS, function(v) var_label(t2, v), character(1)), BALANCE_VARS)
balance_labels[c(
  "VC_villsize", "VC_vaccinated_baseline_18", "VC_anyschooling", "VC_BSL_owns_land",
  "VC_BSL_reduced_portions", "VC_age", "VC_hh_gender", "VC_breast", "VC_preg"
)] <- c(
  "Village population", "Proportion of adults vaccinated at baseline", "HH head has had any schooling",
  "Owns land", "Reduced portion sizes in last week", "Age", "HH head is female",
  "Is breastfeeding", "Is pregnant"
)

balance_rows <- list()
for (v in BALANCE_VARS) {
  m <- fit_fe_model(t2, v, c("treat_dtd", "treat_small"), fe = "grpID", robust = TRUE)
  d12 <- linear_combo(m, c(treat_dtd = 1, treat_small = -1))
  sample_mask <- complete_mask(t2, c(v, "treat_dtd", "treat_small", "grpID"))

  control <- t2[[v]][sample_mask & t2$treatment == 0]
  balance_rows[[length(balance_rows) + 1L]] <- tibble::tibble(
    Variable = balance_labels[[v]],
    `Control Mean (SD)` = fmt_num(safe_mean(control)),
    `Control-Door-to-Door Diff (SE)` = fmt_num(coef_stat(m, "treat_dtd", "estimate")),
    `Control-Small-Group Diff (SE)` = fmt_num(coef_stat(m, "treat_small", "estimate")),
    `Door-to-Door-Small-Group Diff (SE)` = fmt_num(d12["estimate"]),
    N = as.character(stats::nobs(m))
  )
  balance_rows[[length(balance_rows) + 1L]] <- tibble::tibble(
    Variable = "",
    `Control Mean (SD)` = paste0("(", fmt_num(safe_sd(control)), ")"),
    `Control-Door-to-Door Diff (SE)` = paste0("(", fmt_num(coef_stat(m, "treat_dtd", "std_error")), ")"),
    `Control-Small-Group Diff (SE)` = paste0("(", fmt_num(coef_stat(m, "treat_small", "std_error")), ")"),
    `Door-to-Door-Small-Group Diff (SE)` = paste0("(", fmt_num(d12["std_error"]), ")"),
    N = ""
  )
}

t2_numeric <- t2 |>
  dplyr::mutate(dplyr::across(dplyr::all_of(BALANCE_VARS), as_numeric_plain))

joint_dtd <- multinom_joint_p(t2_numeric, "treatment", BALANCE_VARS, ref = 0, target_class = 1)
joint_smg <- multinom_joint_p(t2_numeric, "treatment", BALANCE_VARS, ref = 0, target_class = 2)
joint_diff <- multinom_joint_p(t2_numeric, "treatment", BALANCE_VARS, ref = 1, target_class = 2)

balance_core <- dplyr::bind_rows(balance_rows)
balance_core <- dplyr::bind_rows(
  tibble::tibble(
    Variable = "Community Characteristics from 2015 Census",
    `Control Mean (SD)` = "", `Control-Door-to-Door Diff (SE)` = "",
    `Control-Small-Group Diff (SE)` = "", `Door-to-Door-Small-Group Diff (SE)` = "", N = ""
  ),
  balance_core[1:(2 * length(CENSUS_2015_VARS)), ],
  tibble::tibble(
    Variable = "Characteristics from Village Census",
    `Control Mean (SD)` = "", `Control-Door-to-Door Diff (SE)` = "",
    `Control-Small-Group Diff (SE)` = "", `Door-to-Door-Small-Group Diff (SE)` = "", N = ""
  ),
  balance_core[(2 * length(CENSUS_2015_VARS) + 1):nrow(balance_core), ],
  tibble::tibble(
    Variable = "Joint F-test p-value",
    `Control Mean (SD)` = "",
    `Control-Door-to-Door Diff (SE)` = fmt_num(joint_dtd),
    `Control-Small-Group Diff (SE)` = fmt_num(joint_smg),
    `Door-to-Door-Small-Group Diff (SE)` = fmt_num(joint_diff),
    N = ""
  )
)
write_table(balance_core, file.path(OUT_MAIN_TABLE, "census_bal"), excel = TRUE)

# ------------------------------------------------------------
# Table 3: ITT estimates of vaccination rate among census adults
# ------------------------------------------------------------
t3_base <- individual |> dplyr::filter(above18 == 1, in_census == 1)
t3_cov <- merge_community_covariates(t3_base, community)

r1 <- fit_fe_model(t3_base, "vaccinated_endline", "any_treat", fe = "grpID", cluster = "community_code")
r2 <- fit_fe_model(t3_cov, "vaccinated_endline", c("any_treat", COMMUNITY_COVARIATES), fe = "grpID", cluster = "community_code")

t3_comm <- t3_cov |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_baseline = safe_mean(vaccinated_baseline),
    vaccinated_endline = safe_mean(vaccinated_endline),
    any_treat = first_nonmissing(any_treat),
    treatment = first_nonmissing(treatment),
    grpID = first_nonmissing(grpID),
    .groups = "drop"
  )
r3 <- fit_fe_model(t3_comm, "vaccinated_endline", "any_treat", fe = "grpID")

mask_r1 <- complete_mask(t3_base, c("vaccinated_endline", "any_treat", "grpID", "community_code", "treatment"))
mask_r2 <- complete_mask(t3_cov, c("vaccinated_endline", "any_treat", COMMUNITY_COVARIATES, "grpID", "community_code", "treatment"))
mask_r3 <- complete_mask(t3_comm, c("vaccinated_endline", "any_treat", "grpID", "treatment"))

pboot3 <- c(
  wild_boot_p(r1, "any_treat", clustid = "community_code"),
  wild_boot_p(r2, "any_treat", clustid = "community_code"),
  wild_boot_p(r3, "any_treat", clustid = NULL)
)

table3 <- tibble::tribble(
  ~Row, ~`(1)`, ~`(2)`, ~`(3)`,
  "Pooled Treatment", fmt_cell(r1, "any_treat"), fmt_cell(r2, "any_treat"), fmt_cell(r3, "any_treat"),
  "", fmt_se_cell(r1, "any_treat"), fmt_se_cell(r2, "any_treat"), fmt_se_cell(r3, "any_treat"),
  "Proportion Vaccinated at Baseline", "", fmt_cell(r2, "vaccinated_baseline_18"), "",
  "", "", fmt_se_cell(r2, "vaccinated_baseline_18"), "",
  "Additional Covariates", "No", "Yes", "No",
  "Bootstrapped P-Value", fmt_num(pboot3[1]), fmt_num(pboot3[2]), fmt_num(pboot3[3]),
  "Mean in Control",
    fmt_num(safe_mean(t3_base$vaccinated_endline[mask_r1 & t3_base$treatment == 0])),
    fmt_num(safe_mean(t3_cov$vaccinated_endline[mask_r2 & t3_cov$treatment == 0])),
    fmt_num(safe_mean(t3_comm$vaccinated_endline[mask_r3 & t3_comm$treatment == 0])),
  "No. of Observations", as.character(stats::nobs(r1)), as.character(stats::nobs(r2)), as.character(stats::nobs(r3)),
  "No. of Villages", as.character(cluster_count(r1)), as.character(cluster_count(r2)), as.character(stats::nobs(r3)),
  "R-squared", fmt_num(model_r2(r1)), fmt_num(model_r2(r2)), fmt_num(model_r2(r3))
)
write_table(table3, file.path(OUT_MAIN_TABLE, "itt_rate"))

# ------------------------------------------------------------
# Table 4: ITT estimates of count vaccinated per site
# ------------------------------------------------------------
t4 <- merge_community_covariates(individual, community)
condition_filters <- list(
  "Census only" = function(d) d |> dplyr::filter(in_census == 1),
  "Census + other villages" = function(d) d |> dplyr::filter(diff_comm == 1 | in_census == 1),
  "Anyone" = function(d) d
)

collapse_t4 <- function(d) {
  d |>
    dplyr::group_by(community_code) |>
    dplyr::summarise(
      vaccinated_endline = sum(vaccinated_endline, na.rm = TRUE),
      treatment = first_nonmissing(treatment),
      grpID = first_nonmissing(grpID),
      any_treat = first_nonmissing(any_treat),
      dplyr::across(dplyr::all_of(COMMUNITY_COVARIATES), first_nonmissing),
      .groups = "drop"
    )
}

models4 <- list()
extras4 <- list()
for (with_cov in c(FALSE, TRUE)) {
  for (nm in names(condition_filters)) {
    d <- collapse_t4(condition_filters[[nm]](t4))
    rhs <- c("any_treat", if (with_cov) COMMUNITY_COVARIATES else character())
    m <- fit_fe_model(d, "vaccinated_endline", rhs, fe = "grpID", cluster = "community_code")
    key <- paste0(nm, if (with_cov) " + covariates" else "")
    models4[[key]] <- m
    mask <- complete_mask(d, c("vaccinated_endline", rhs, "grpID", "community_code", "treatment"))
    extras4[[key]] <- list(
      `Additional Covariates` = if (with_cov) "Yes" else "No",
      `Bootstrapped P-Value` = fmt_num(wild_boot_p(m, "any_treat", clustid = "community_code")),
      `Mean in Control` = fmt_num(safe_mean(d$vaccinated_endline[mask & d$treatment == 0])),
      `No. of Observations` = as.character(stats::nobs(m)),
      `R-squared` = fmt_num(model_r2(m))
    )
  }
}

table4 <- reg_table(
  models4,
  terms = c("any_treat", "vaccinated_baseline_18"),
  model_names = names(models4),
  term_labels = c(any_treat = "Pooled treatment", vaccinated_baseline_18 = "Proportion Vaccinated at Baseline"),
  extras = extras4
)
write_table(table4, file.path(OUT_MAIN_TABLE, "itt_count"))

# ------------------------------------------------------------
# Table 5: vaccination by baseline willingness and meeting attendance
# ------------------------------------------------------------
t5 <- individual |>
  dplyr::filter(treatment != 0, in_baseline == 1) |>
  dplyr::select(attended, vaccinated_endline, BSL_covid_wouldtake) |>
  dplyr::filter(!is.na(BSL_covid_wouldtake), !is.na(attended)) |>
  dplyr::group_by(BSL_covid_wouldtake, attended) |>
  dplyr::summarise(
    mean_vaccinated_endline = safe_mean(vaccinated_endline),
    n = sum(!is.na(vaccinated_endline)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    willingness = ifelse(BSL_covid_wouldtake == 1, "Yes", "No"),
    attended_meeting = ifelse(attended == 1, "Yes", "No")
  ) |>
  dplyr::select(willingness, attended_meeting, mean_vaccinated_endline, n)
write_table(t5, file.path(OUT_MAIN_TABLE, "att_willing"))

# ------------------------------------------------------------
# Tables 6 and 7: knowledge/attitudes and trust outcomes
# ------------------------------------------------------------
subsample <- individual |> dplyr::filter(incomplete_observations == 0)

make_multi_outcome_table <- function(data, outcomes, titles, stem) {
  models <- setNames(lapply(outcomes, function(y) {
    fit_fe_model(data, y, "any_treat", fe = "grpID", cluster = "community_code")
  }), titles)

  ordinary_p <- vapply(models, coef_stat, numeric(1), term = "any_treat", field = "p_value")
  qvals <- sharpened_qvalues(ordinary_p)

  extras <- Map(function(m, y, q) {
    mask <- complete_mask(data, c(y, "any_treat", "grpID", "community_code", "treatment"))
    list(
      `Bootstrapped P-Value` = fmt_num(wild_boot_p(m, "any_treat", clustid = "community_code")),
      `FDR Q-value` = fmt_num(q),
      `Mean in Control` = fmt_num(safe_mean(data[[y]][mask & data$treatment == 0])),
      `No. of Observations` = as.character(stats::nobs(m)),
      `No. of Villages` = as.character(cluster_count(m)),
      `R-squared` = fmt_num(model_r2(m))
    )
  }, models, outcomes, qvals)
  names(extras) <- titles

  tab <- reg_table(
    models,
    terms = "any_treat",
    model_names = titles,
    term_labels = c(any_treat = "Pooled Treatment"),
    extras = extras
  )
  write_table(tab, file.path(OUT_MAIN_TABLE, stem))
}

make_multi_outcome_table(
  subsample,
  KNOWLEDGE_OUTCOMES,
  c(
    "Believes COVID-19 is real", "Knows about the COVID-19 vaccine",
    "Believes vaccines are effective", "Believes vaccines are safe"
  ),
  "itt_knowledge"
)

# The Stata source relies on Table 6's in-memory incomplete_observations == 0 sample.
# We explicitly use the same subsample here so this table is independently reproducible.
make_multi_outcome_table(
  subsample,
  TRUST_OUTCOMES,
  c(
    "Community Health Clinic", "Ministry of Health and Sanitation", "Media",
    "Social media", "Family and friends"
  ),
  "itt_trust"
)

# ------------------------------------------------------------
# Table 8: ITT estimates for demographic subgroups
# ------------------------------------------------------------
t8 <- individual |> dplyr::filter(above18 == 1, in_census == 1)
subgroup_filters <- list(
  "Full sample" = rep(TRUE, nrow(t8)),
  "Female" = t8$female == 1,
  "Male" = t8$female == 0,
  "Aged 18-24" = t8$Age18_24 == 1,
  "Aged 25-54" = t8$Age25_54 == 1,
  "Aged 55+" = t8$Age55 == 1,
  "HH head any schooling" = t8$anyschooling == 1,
  "HH head no schooling" = t8$anyschooling == 0,
  "HH owns any land" = t8$BSL_owns_land == 1,
  "HH owns no land" = t8$BSL_owns_land == 0,
  "HH reduced food portions" = t8$BSL_reduced_portions == 1,
  "HH did not reduce food portions" = t8$BSL_reduced_portions == 0
)

models8 <- list()
extras8 <- list()
for (nm in names(subgroup_filters)) {
  idx <- subgroup_filters[[nm]]
  d <- t8[!is.na(idx) & idx, , drop = FALSE]
  m <- fit_fe_model(d, "vaccinated_endline", "any_treat", fe = "grpID", cluster = "community_code")
  mask <- complete_mask(d, c("vaccinated_endline", "any_treat", "grpID", "community_code", "treatment"))
  models8[[nm]] <- m
  extras8[[nm]] <- list(
    `Mean in Control` = fmt_num(safe_mean(d$vaccinated_endline[mask & d$treatment == 0])),
    `No. of Observations` = as.character(stats::nobs(m)),
    `No. of Villages` = as.character(cluster_count(m)),
    `R-squared` = fmt_num(model_r2(m))
  )
}

table8 <- reg_table(
  models8,
  terms = "any_treat",
  model_names = names(models8),
  term_labels = c(any_treat = "Pooled Treatment"),
  extras = extras8
)
write_table(table8, file.path(OUT_MAIN_TABLE, "itt_demographic"))

# ------------------------------------------------------------
# Table 10: comparison of full sample and restricted subsample
# ------------------------------------------------------------
t10 <- community |>
  dplyr::select(
    community_code, treatment, incomplete_observations, grpID, any_treat,
    dplyr::all_of(CENSUS_2015_VARS), dplyr::all_of(VILLAGE_CENSUS_VARS)
  ) |>
  dplyr::rename_with(~ paste0("VC_", .x), dplyr::all_of(VILLAGE_CENSUS_VARS))

sample_labels <- setNames(vapply(BALANCE_VARS, function(v) var_label(t10, v), character(1)), BALANCE_VARS)
sample_labels[names(balance_labels)] <- balance_labels

sample_rows <- list()
for (v in BALANCE_VARS) {
  x <- as_numeric_plain(t10[[v]])
  sub <- x[t10$incomplete_observations == 0]
  tt_data <- tibble::tibble(x = x, incomplete = as_numeric_plain(t10$incomplete_observations)) |>
    dplyr::filter(!is.na(x), !is.na(incomplete))
  p <- if (dplyr::n_distinct(tt_data$incomplete) == 2L) {
    stats::t.test(x ~ incomplete, data = tt_data, var.equal = TRUE)$p.value
  } else {
    NA_real_
  }

  sample_rows[[length(sample_rows) + 1L]] <- tibble::tibble(
    Variable = sample_labels[[v]],
    `Full Sample N` = as.character(sum(!is.na(x))),
    `Full Sample Mean (SD)` = fmt_num(safe_mean(x)),
    `Restricted Sample N` = as.character(sum(!is.na(sub))),
    `Restricted Sample Mean (SD)` = fmt_num(safe_mean(sub)),
    `Difference (p-value)` = fmt_num(safe_mean(x) - safe_mean(sub))
  )
  sample_rows[[length(sample_rows) + 1L]] <- tibble::tibble(
    Variable = "",
    `Full Sample N` = "",
    `Full Sample Mean (SD)` = paste0("(", fmt_num(safe_sd(x)), ")"),
    `Restricted Sample N` = "",
    `Restricted Sample Mean (SD)` = paste0("(", fmt_num(safe_sd(sub)), ")"),
    `Difference (p-value)` = paste0("(", fmt_num(p), ")")
  )
}

t10_numeric <- t10 |>
  dplyr::mutate(dplyr::across(dplyr::all_of(BALANCE_VARS), as_numeric_plain)) |>
  dplyr::mutate(incomplete_observations = as_numeric_plain(incomplete_observations))
joint_sample <- glm_joint_p(t10_numeric, "incomplete_observations", BALANCE_VARS)

sample_core <- dplyr::bind_rows(sample_rows)
sample_core <- dplyr::bind_rows(
  tibble::tibble(
    Variable = "Community Characteristics from 2015 Census",
    `Full Sample N` = "", `Full Sample Mean (SD)` = "", `Restricted Sample N` = "",
    `Restricted Sample Mean (SD)` = "", `Difference (p-value)` = ""
  ),
  sample_core[1:(2 * length(CENSUS_2015_VARS)), ],
  tibble::tibble(
    Variable = "Characteristics from Village Census",
    `Full Sample N` = "", `Full Sample Mean (SD)` = "", `Restricted Sample N` = "",
    `Restricted Sample Mean (SD)` = "", `Difference (p-value)` = ""
  ),
  sample_core[(2 * length(CENSUS_2015_VARS) + 1):nrow(sample_core), ],
  tibble::tibble(
    Variable = "Joint F-test p-value",
    `Full Sample N` = "",
    `Full Sample Mean (SD)` = fmt_num(joint_sample),
    `Restricted Sample N` = "",
    `Restricted Sample Mean (SD)` = "",
    `Difference (p-value)` = ""
  )
)
write_table(sample_core, file.path(OUT_MAIN_TABLE, "sample_diff"), excel = TRUE)

message("  Main and extended-data tables written to output/tables/main/")
