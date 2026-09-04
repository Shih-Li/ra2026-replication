# 04_main_effects_robustness.R
# Translation of loan_main.do: Table 3, A3, A5, Figure-3 nonlinearity checks,
# Table A6, and Table A7.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("prepare_loanmain")) source("code/02_helpers.R")

d <- prepare_loanmain()

# ------------------------------------------------------------------
# Table 3: main firm-level effects
# ------------------------------------------------------------------

main_outcomes <- existing_vars(
  d, c("lnpart5revenue", "total_profit", "lnlabor", "lnwage_cost",
       "total_fixedassets", "lnmaterial_cost", "shutdown")
)

t3_specs <- lapply(main_outcomes, function(y) {
  list(
    id = paste0("Table3_", y),
    outcome = y,
    rhs = c("post", "interpost", "inter4post"),
    test_terms = c("interpost", "inter4post"),
    subset_fun = function(x) rep(TRUE, nrow(x)),
    cluster = "survey_town",
    fe = "firmid",
    panel = "firmid"
  )
})

t3_models <- lapply(t3_specs, function(s) fit_spec(d, s))
t3 <- dplyr::bind_rows(lapply(seq_along(t3_specs), function(i) {
  s <- t3_specs[[i]]
  model_rows(
    t3_models[[i]], s$test_terms, s$id, s$outcome,
    control = control_mean(d, s$outcome, d$round == 1 & d$survey_town_type == 0),
    fixed_effects = "Firm FE + Post"
  )
}))

rw3 <- romano_wolf_cluster(d, t3_specs, reps = getOption("ra2026.rwolf_reps", 1000L),
                           seed = 123L, cluster = "survey_town", panel = "firmid")
t3 <- dplyr::left_join(
  t3, rw3[, c("model", "outcome", "term", "rw_p.value")],
  by = c("model", "outcome", "term")
)
write_table(t3, "Table3", "Table 3. Main firm-level effects")

# ------------------------------------------------------------------
# Table A3: specification and sample checks
# ------------------------------------------------------------------

a3_outcomes <- existing_vars(
  d, c("lnpart5revenue", "total_profit", "lnlabor", "lnwage_cost",
       "total_fixedassets", "lnmaterial_cost")
)

a3_samples <- list(
  baseline_all = function(x) x$round == 1,
  baseline_no_shutdown_attrition = function(x) x$shutdownall == 0 & x$flagatt == 0 & x$round == 1,
  baseline_observed_endline = function(x) x$endatt == 0 & x$round == 1
)

a3 <- dplyr::bind_rows(lapply(names(a3_samples), function(sn) {
  dplyr::bind_rows(lapply(a3_outcomes, function(y) {
    rhs <- c("type", "treatratio_comp")
    if (sn == "baseline_all") rhs <- c(rhs, "townsize")
    rhs <- existing_vars(d, rhs)
    m <- fit_ols(d, y, rhs, subset = a3_samples[[sn]](d), cluster = "firmid")
    z <- model_rows(m, intersect(c("type", "treatratio_comp"), rhs),
                    paste0("A3_", sn, "_", y), y)
    z$sample <- sn
    z$joint_p.value <- joint_wald_p(m, intersect(c("type", "treatratio_comp"), rhs))
    z
  }))
}))
write_table(a3, "TableA3", "Table A3. Specification checks")

# ------------------------------------------------------------------
# Table A5: nonlinearity and follow-up specifications
# ------------------------------------------------------------------

a5_outcomes <- existing_vars(d, c("lnpart5revenue", "total_profit", "lnlabor"))
a5_rows <- list()

for (y in a5_outcomes) {
  rhs <- existing_vars(d, c("postmarket50T", "postmarket80T", "postmarket50", "postmarket80", "post"))
  m <- fit_ols(d, y, rhs, cluster = "survey_town", fe = "firmid")
  z <- model_rows(m, rhs, paste0("A5_market_saturation_", y), y, fixed_effects = "Firm FE")
  z$specification <- "50/80 market saturation"
  a5_rows[[length(a5_rows) + 1L]] <- z

  basevar <- switch(y, lnpart5revenue = "baselnsales", total_profit = "baseprofit", lnlabor = "baselnlabor")
  rhs2 <- existing_vars(d, c("type", "treatratio_comp", basevar))
  m2 <- fit_ols(d, y, rhs2, subset = d$round > 1, cluster = "survey_town")
  z2 <- model_rows(m2, intersect(c("type", "treatratio_comp"), rhs2),
                   paste0("A5_followup_", y), y)
  z2$specification <- "follow-up with baseline outcome"
  a5_rows[[length(a5_rows) + 1L]] <- z2

  rhs3 <- existing_vars(d, c("post", "interpost", "interpost_bin2", "interpost_bin3", "interpost_bin4"))
  m3 <- fit_ols(d, y, rhs3, cluster = "survey_town", fe = "firmid")
  z3 <- model_rows(m3, intersect(c("interpost", "interpost_bin2", "interpost_bin3", "interpost_bin4"), rhs3),
                   paste0("A5_bins_", y), y, fixed_effects = "Firm FE")
  z3$specification <- "competitor-treatment bins"
  a5_rows[[length(a5_rows) + 1L]] <- z3
}
a5 <- dplyr::bind_rows(a5_rows)
write_table(a5, "TableA5", "Table A5. Nonlinearity")

# Reconstruct the quantities the original loan_main.do checks before the separate
# Figure3.do reads the author-prepared Figure3_do.xlsx.
fig3_check <- dplyr::bind_rows(lapply(a5_outcomes, function(y) {
  bin_rhs <- existing_vars(d, c("post", "interpost", "interpost_bin2", "interpost_bin3", "interpost_bin4"))
  lin_rhs <- existing_vars(d, c("post", "interpost", "inter4post"))
  mb <- fit_ols(d, y, bin_rhs, cluster = "survey_town", fe = "firmid")
  ml <- fit_ols(d, y, lin_rhs, cluster = "survey_town", fe = "firmid")
  slope <- coef_stat(ml, "inter4post")["estimate"]

  dplyr::bind_rows(lapply(1:4, function(i) {
    bmask <- d[[paste0("bin", i)]] == 1
    xbar <- safe_mean(d$treatratio_comp[bmask])
    if (i == 1) {
      b <- 0; se <- NA_real_
    } else {
      st <- coef_stat(mb, paste0("interpost_bin", i))
      b <- st["estimate"]; se <- st["se"]
    }
    data.frame(
      outcome = y, bin = i, mean_share_competitors_treated = xbar,
      nonlinear_effect = b, nonlinear_se = se,
      nonlinear_ci_low = ifelse(is.na(se), NA_real_, b - 1.96 * se),
      nonlinear_ci_high = ifelse(is.na(se), NA_real_, b + 1.96 * se),
      linear_effect = slope * xbar
    )
  }))
}))
write_table(fig3_check, "Figure3-nonlinearity-check",
            "Quantities underlying the Figure 3 nonlinearity check")

# ------------------------------------------------------------------
# Table A6: shutdown and inverse-hyperbolic-sine robustness
# ------------------------------------------------------------------

a6_outcomes <- existing_vars(
  d, c(
    "part5revenue", "total_profit", "labor",
    "shut_part5revenue", "shut_total_profit", "shut_labor",
    "asinhpart5revenue", "asinhtotal_profit", "asinhlabor",
    "asinhshut_part5revenue", "asinhshut_total_profit", "asinhshut_labor"
  )
)

a6 <- dplyr::bind_rows(lapply(a6_outcomes, function(y) {
  rhs <- existing_vars(d, c("interpost", "inter4post", "post"))
  m <- fit_ols(d, y, rhs, cluster = "survey_town", fe = "firmid")
  model_rows(
    m, intersect(c("interpost", "inter4post"), rhs),
    paste0("A6_", y), y,
    control = control_mean(d, y, d$round == 1 & d$survey_town_type == 0),
    fixed_effects = "Firm FE"
  )
}))
write_table(a6, "TableA6", "Table A6. Shutdown and IHS robustness")

# ------------------------------------------------------------------
# Table A7: treated/untreated competitor exposure and mid/endline timing
# ------------------------------------------------------------------

a7_rows <- list()
for (y in a5_outcomes) {
  rhs1 <- existing_vars(d, c("interpost", "interpost_comp1", "interpost_comp2", "post"))
  m1 <- fit_ols(d, y, rhs1, cluster = "survey_town", fe = "firmid")
  z1 <- model_rows(m1, intersect(c("interpost", "interpost_comp1", "interpost_comp2"), rhs1),
                   paste0("A7_status_", y), y, fixed_effects = "Firm FE")
  z1$specification <- "competitor treatment by own treatment status"
  a7_rows[[length(a7_rows) + 1L]] <- z1

  rhs2 <- existing_vars(d, c("interpost_round11", "interpost_round12",
                             "interpost_round21", "interpost_round22",
                             "midline", "endline"))
  m2 <- fit_ols(d, y, rhs2, cluster = "survey_town", fe = "firmid")
  z2 <- model_rows(m2, intersect(c("interpost_round11", "interpost_round12",
                                   "interpost_round21", "interpost_round22"), rhs2),
                   paste0("A7_timing_", y), y, fixed_effects = "Firm FE")
  z2$specification <- "midline/endline interactions"
  a7_rows[[length(a7_rows) + 1L]] <- z2
}
write_table(dplyr::bind_rows(a7_rows), "TableA7", "Table A7. Status and timing robustness")
