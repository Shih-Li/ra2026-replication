# ============================================================================
# HIV test and mortality analysis
# Parallel to: code/test_mortality_analysis.do
# ============================================================================

if (!exists("fit_gmm_iv")) source(file.path("code", "config_stata.R"))

infile <- file.path("data", "processed", "hivtest_mortality.rds")
if (!file.exists(infile)) stop("Run code/test_mortality_prep.R first: ", infile)
d <- readRDS(infile)

demog <- c("hiv", "male", "age", "age2", "south", "north")
Z <- c("anyincentive", "incentive", "incentive2", "distvct", "distvct2")

d$hiv_neg <- 1 - d$hiv
for (v in Z) d[[paste0(v, "_hivneg")]] <- d[[v]] * d$hiv_neg
instrument_hivneg <- paste0(Z, "_hivneg")
instrument_hivpos <- c("anyincentivehiv", "incentivehiv", "incentive2hiv", "distvcthiv", "distvct2hiv")

# Table 1: summary statistics
sumvars <- c("male", "age", "south", "north", "anyincentive", "incentive", "distvct", "posttest",
             "obs2006", "obs2008", "obs2010", "obs2018",
             "alive2006", "alive2008", "alive2010", "alive2018")
write_summary_tex(
  d, sumvars, file.path("tables", "table1.tex"),
  groups = list(All = rep(TRUE, nrow(d)), `HIV-` = d$hiv == 0, `HIV+` = d$hiv == 1)
)

# Figure A.1
p1 <- ggplot(d, aes(x = incentive)) + geom_histogram() + theme_bw() + labs(x = "Financial incentive ($)", y = NULL)
ggsave(file.path("figures", "figureA1a.pdf"), p1, width = 6.5, height = 4.5)
p2 <- ggplot(d, aes(x = distvct)) + geom_histogram() + theme_bw() + labs(x = "Distance to health clinic (km)", y = NULL)
ggsave(file.path("figures", "figureA1b.pdf"), p2, width = 6.5, height = 4.5)

# Table 2 top panel: OLS / IV-GMM survival effects
years <- c("2006", "2008", "2010", "2018")
models_t2 <- list()
for (yr in years) {
  y <- paste0("alive", yr)
  mean_neg <- mean(d[[y]][d$learnhivneg == 0 & d$hiv == 0], na.rm = TRUE)
  mean_pos <- mean(d[[y]][d$learnhivpos == 0 & d$hiv == 1], na.rm = TRUE)

  ols <- fit_gmm_iv(d, y, c(demog, "learnhivneg", "learnhivpos"), character(), character(), "DK_village_number")
  ols <- attach_stats(ols, OutcomeMean_HIVneg = mean_neg, OutcomeMean_HIVpos = mean_pos)
  iv <- fit_gmm_iv(d, y, demog, c("learnhivneg", "learnhivpos"),
                   c(instrument_hivneg, instrument_hivpos), "DK_village_number")
  iv <- attach_stats(iv, OutcomeMean_HIVneg = mean_neg, OutcomeMean_HIVpos = mean_pos)
  models_t2 <- c(models_t2, list(ols, iv))
}
write_models_tex(models_t2, file.path("tables", "table2top.tex"),
                 keep = c("learnhivneg", "learnhivpos"), title = "Survival",
                 extra_stats = c("OutcomeMean_HIVneg", "OutcomeMean_HIVpos"))

# Table A.9: selection
models_a9 <- lapply(years, function(yr) {
  fit_gmm_iv(d, paste0("obs", yr), demog, c("learnhivneg", "learnhivpos"),
             c(instrument_hivneg, instrument_hivpos), "DK_village_number")
})
write_models_tex(models_a9, file.path("tables", "tableA9.tex"),
                 keep = c("learnhivneg", "learnhivpos"), title = "Selection")

# Table A.10: inverse-probability weighting based on 2010 observation probability
probit_vars <- c("obs2010", demog, instrument_hivneg, instrument_hivpos)
okp <- complete.cases(d[, probit_vars, drop = FALSE])
probit_fit <- glm(
  reformulate(c(demog, instrument_hivneg, instrument_hivpos), response = "obs2010"),
  data = d[okp, , drop = FALSE], family = binomial(link = "probit")
)
d$phat <- NA_real_
d$phat[okp] <- predict(probit_fit, type = "response")
d$ipw <- 1 / d$phat
models_a10 <- lapply(years, function(yr) {
  fit_gmm_iv(d, paste0("alive", yr), demog, c("learnhivneg", "learnhivpos"),
             c(instrument_hivneg, instrument_hivpos), "DK_village_number", weights = "ipw")
})
write_models_tex(models_a10, file.path("tables", "tableA10.tex"),
                 keep = c("learnhivneg", "learnhivpos"), title = "Mortality — IPW")

# Table A.4 top: financial incentives only
instrument_hivneg_fin <- c("anyincentive_hivneg", "incentive_hivneg", "incentive2_hivneg")
instrument_hivpos_fin <- c("anyincentivehiv", "incentivehiv", "incentive2hiv")
models_a4 <- lapply(years, function(yr) {
  fit_gmm_iv(d, paste0("alive", yr), demog, c("learnhivneg", "learnhivpos"),
             c(instrument_hivneg_fin, instrument_hivpos_fin), "DK_village_number")
})
write_models_tex(models_a4, file.path("tables", "tableA4top.tex"),
                 keep = c("learnhivneg", "learnhivpos"), title = "Financial incentives only", stars_on = FALSE)

# Table A.7: separate regressions by HIV status
models_a7 <- list()
d0 <- d[d$hiv == 0, , drop = FALSE]
for (yr in years) {
  models_a7[[length(models_a7) + 1L]] <- fit_gmm_iv(
    d0, paste0("alive", yr), demog, "learnhivneg", Z, "DK_village_number"
  )
}
d1 <- d[d$hiv == 1, , drop = FALSE]
for (yr in years) {
  models_a7[[length(models_a7) + 1L]] <- fit_gmm_iv(
    d1, paste0("alive", yr), demog, "learnhivpos", Z, "DK_village_number"
  )
}
write_models_tex(models_a7, file.path("tables", "tableA7.tex"),
                 keep = c("learnhivneg", "learnhivpos"), title = "Separate regressions by HIV status")

# Table A.3: reduced form
models_a3 <- lapply(years, function(yr) {
  fit_ols_cluster(d, paste0("alive", yr), c(demog, instrument_hivneg, instrument_hivpos), "DK_village_number")
})
write_models_tex(models_a3, file.path("tables", "tableA3.tex"),
                 keep = c(instrument_hivneg, instrument_hivpos), title = "Reduced form")
