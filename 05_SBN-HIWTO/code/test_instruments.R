# ============================================================================
# Instrument exogeneity, first-stage, and weak-IV diagnostics
# Parallel to: code/test_instruments.do
# ============================================================================

if (!exists("fit_gmm_iv")) source(file.path("code", "config_stata.R"))

infile <- file.path("data", "processed", "hivtest_mortality.rds")
if (!file.exists(infile)) stop("Run code/test_mortality_prep.R first")
d <- readRDS(infile)

demog <- c("hiv", "male", "age", "age2", "south", "north")
Z <- c("anyincentive", "incentive", "incentive2", "distvct", "distvct2")

d$hiv_neg <- 1 - d$hiv
for (v in Z) d[[paste0(v, "_hivneg")]] <- d[[v]] * d$hiv_neg
instrument_hivneg <- paste0(Z, "_hivneg")
instrument_hivpos <- c("anyincentivehiv", "incentivehiv", "incentive2hiv", "distvcthiv", "distvct2hiv")

# Table A.2: exogeneity of basic instruments
models_a2 <- list(
  fit_ols_cluster(d, "male", c(Z, "south", "north"), "DK_village_number"),
  fit_ols_cluster(d, "age", c(Z, "south", "north"), "DK_village_number"),
  fit_ols_cluster(d, "hiv", c(Z, "south", "north"), "DK_village_number")
)
write_models_tex(models_a2, file.path("tables", "tableA2.tex"), keep = Z, title = "Exogeneity of instruments")

# Extended baseline controls
d$schooling_ext <- d$schooling
idx <- is.na(d$schooling_ext) & !is.na(d$DK_M2_level_education)
d$schooling_ext[idx] <- as.numeric(d$DK_M2_level_education[idx] > 0)
idx <- is.na(d$schooling_ext) & !is.na(d$DK_M1_level_education)
d$schooling_ext[idx] <- as.numeric(d$DK_M1_level_education[idx] > 0)

d$yrs_schooling <- as_num(d$DK_M3_years_level_education)
d$yrs_schooling[idx_true(d$yrs_schooling > 30)] <- NA_real_
d$yrs_schooling_ext <- as_num(d$DK_M3_years_level_education)
idx <- is.na(d$yrs_schooling_ext)
d$yrs_schooling_ext[idx] <- as_num(d$DK_M2_years_level_eductation[idx])
idx <- is.na(d$yrs_schooling_ext)
d$yrs_schooling_ext[idx] <- as_num(d$DK_M1_years_level_educat[idx])
d$yrs_schooling_ext[idx_true(d$yrs_schooling_ext > 30)] <- NA_real_
d$nb_children <- as_num(d$DK_M3_children)
d$nb_children[idx_true(d$nb_children > 50)] <- NA_real_

# Table A.5
rhs_exog <- c(Z, "south", "north")
models_a5 <- list(
  fit_ols_cluster(d, "married", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "schooling", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "schooling_ext", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "yrs_schooling", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "yrs_schooling_ext", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "nb_children", rhs_exog, "DK_village_number"),
  fit_ols_cluster(d, "DK_M3_wealth_score", rhs_exog, "DK_village_number"),
  fit_ordered_probit_cluster(d, "DK_M3_wealth_quantile", rhs_exog, "DK_village_number")
)
write_models_tex(models_a5, file.path("tables", "tableA5.tex"), keep = Z, title = "Exogeneity of instruments — extended controls")

# Table A.6: second stage with expanded controls
demog_exp <- c("hiv", "male", "age", "age2", "south", "north", "married", "nb_children", "schooling_ext", "DK_M3_wealth_score")
years <- c("2006", "2008", "2010", "2018")
models_a6 <- lapply(years, function(yr) {
  fit_gmm_iv(d, paste0("alive", yr), demog_exp, c("learnhivneg", "learnhivpos"),
             c(instrument_hivneg, instrument_hivpos), "DK_village_number")
})
write_models_tex(models_a6, file.path("tables", "tableA6.tex"),
                 keep = c("learnhivneg", "learnhivpos", "married", "nb_children", "schooling_ext", "DK_M3_wealth_score"),
                 title = "Effects on mortality including extended baseline characteristics")

# Table 2 bottom panel: first stages
first_stage_models <- list()
for (yr in years) {
  dd <- d[!is.na(d[[paste0("alive", yr)]]), , drop = FALSE]
  first_stage_models[[length(first_stage_models) + 1L]] <- fit_ols_cluster(
    dd, "learnhivneg", c(demog, instrument_hivneg, instrument_hivpos), "DK_village_number")
  first_stage_models[[length(first_stage_models) + 1L]] <- fit_ols_cluster(
    dd, "learnhivpos", c(demog, instrument_hivneg, instrument_hivpos), "DK_village_number")
}
write_models_tex(first_stage_models, file.path("tables", "table2bottom.tex"),
                 keep = c(instrument_hivneg, instrument_hivpos), title = "First stages", stars_on = FALSE)

# Table 2 test statistics
# Row 1: Montiel Olea-Pflueger effective F for each endogenous regressor.
# Rows 2-4 in Stata are e(cdf), weakivtest2 statistic, and weakivtest2 critical value.
# The latter two come from Lewis-Mertens weakivtest2; no like-for-like R package is
# currently used here, so those cells are intentionally NA rather than mislabeled.
test <- matrix(NA_real_, nrow = 4, ncol = 8)
for (i in seq_along(years)) {
  yr <- years[i]
  dd <- d[d[[paste0("obs", yr)]] == 1, , drop = FALSE]
  col <- 2 * i - 1
  test[1, col] <- effective_f(dd, paste0("alive", yr), "learnhivneg",
                              c(instrument_hivneg, instrument_hivpos), demog, "DK_village_number")
  test[1, col + 1] <- effective_f(dd, paste0("alive", yr), "learnhivpos",
                                  c(instrument_hivneg, instrument_hivpos), demog, "DK_village_number")
}
write_matrix_tex(test, file.path("tables", "table2_teststats.tex"),
                 row_names = c("Effective F", "Cragg-Donald F [validate]", "weakivtest2 statistic [Stata-only]", "weakivtest2 critical value [Stata-only]"))

# Table A.7 bottom panel: effective F by HIV status
f_sep <- matrix(NA_real_, nrow = 2, ncol = 4)
d0 <- d[d$hiv == 0, , drop = FALSE]
d1 <- d[d$hiv == 1, , drop = FALSE]
for (i in seq_along(years)) {
  yr <- years[i]
  f_sep[1, i] <- effective_f(d0, paste0("alive", yr), "learnhivneg", Z, demog, "DK_village_number")
  f_sep[2, i] <- effective_f(d1, paste0("alive", yr), "learnhivpos", Z, demog, "DK_village_number")
}
write_matrix_tex(f_sep, file.path("tables", "tableA7_Fstat.tex"), row_names = c("HIV-", "HIV+"))

# Table A.4 bottom: financial incentives only
instrument_hivneg_fin <- c("anyincentive_hivneg", "incentive_hivneg", "incentive2_hivneg")
instrument_hivpos_fin <- c("anyincentivehiv", "incentivehiv", "incentive2hiv")
first_stage_fin <- list()
for (yr in years) {
  dd <- d[!is.na(d[[paste0("alive", yr)]]), , drop = FALSE]
  first_stage_fin[[length(first_stage_fin) + 1L]] <- fit_ols_cluster(
    dd, "learnhivneg", c(demog, instrument_hivneg_fin, instrument_hivpos_fin), "DK_village_number")
  first_stage_fin[[length(first_stage_fin) + 1L]] <- fit_ols_cluster(
    dd, "learnhivpos", c(demog, instrument_hivneg_fin, instrument_hivpos_fin), "DK_village_number")
}
write_models_tex(first_stage_fin, file.path("tables", "tableA4bottom.tex"),
                 keep = c(instrument_hivneg_fin, instrument_hivpos_fin), title = "First stages — financial incentives only", stars_on = FALSE)

test2 <- matrix(NA_real_, nrow = 4, ncol = 8)
for (i in seq_along(years)) {
  yr <- years[i]
  dd <- d[d[[paste0("obs", yr)]] == 1, , drop = FALSE]
  col <- 2 * i - 1
  test2[1, col] <- effective_f(dd, paste0("alive", yr), "learnhivneg",
                               c(instrument_hivneg_fin, instrument_hivpos_fin), demog, "DK_village_number")
  test2[1, col + 1] <- effective_f(dd, paste0("alive", yr), "learnhivpos",
                                   c(instrument_hivneg_fin, instrument_hivpos_fin), demog, "DK_village_number")
}
write_matrix_tex(test2, file.path("tables", "tableA4_teststats.tex"),
                 row_names = c("Effective F", "Cragg-Donald F [validate]", "weakivtest2 statistic [Stata-only]", "weakivtest2 critical value [Stata-only]"))

# Table A.8: first stages excluding cross effects
models_a8 <- list()
for (yr in years) {
  dd <- d[d[[paste0("obs", yr)]] == 1, , drop = FALSE]
  models_a8[[length(models_a8) + 1L]] <- fit_ols_cluster(dd, "learnhivneg", c(instrument_hivneg, demog), "DK_village_number")
  models_a8[[length(models_a8) + 1L]] <- fit_ols_cluster(dd, "learnhivpos", c(instrument_hivpos, demog), "DK_village_number")
}
write_models_tex(models_a8, file.path("tables", "tableA8.tex"), keep = c(instrument_hivneg, instrument_hivpos), title = "First stage excluding cross effects")

message("Note: exact weakivtest2 rows remain intentionally unresolved in R; see comments in test_instruments.R.")
