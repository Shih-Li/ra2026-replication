# ============================================================================
# Mechanisms using the Thornton 2005 follow-up survey
# Parallel to: code/multiple_test.do
# ============================================================================

# Always reload the shared helper.  This avoids silently reusing an older
# fit_gmm_iv() left in the global workspace from a previous run.
source(file.path("code", "config_stata.R"))

expected_helper_version <- "2026-08-22-mechanism-debug-v2"
if (!exists("REPLICATION_HELPER_VERSION") ||
    !identical(REPLICATION_HELPER_VERSION, expected_helper_version)) {
  stop(
    "Wrong code/config_stata.R loaded. Expected helper version: ",
    expected_helper_version,
    ". Replace code/config_stata.R with the matching revised file."
  )
}

main_file <- file.path("data", "processed", "hivtest_mortality.rds")
follow_file <- file.path("data", "source", "raw", "rawdata_2005followup.dta")
if (!file.exists(main_file)) stop("Run code/test_mortality_prep.R first")
if (!file.exists(follow_file)) stop("Missing required source file: ", follow_file)

d <- readRDS(main_file)
follow <- haven::read_dta(follow_file)
d <- stata_merge_1to1(d, follow, "respondentid")

demog <- c("hiv", "male", "age", "age2", "south", "north")

# Exact excluded-instrument specification from the Stata multiple_test.do:
#   Z plus Z x HIV
# Keep this parameterization rather than changing the instrument basis.
z_base <- c("anyincentive", "incentive", "incentive2", "distvct", "distvct2")
instrument2 <- c(
  z_base,
  paste0(z_base, "hiv")
)
stopifnot(length(instrument2) == 10L, all(instrument2 %in% names(d)))

# Stata factor variables (i.var) choose their omitted/base category from the
# regression estimation sample. This matters here because when all mechanism
# components are missing, rowtotal(..., missing) is missing while rownonmiss
# equals zero. Thus category 0 is absent from the summary-index regression.
# Building dummies on the full merged data first leaves a full set of dummies
# plus an intercept in the actual regression sample, making the design matrix
# exactly collinear.
add_stata_factor_for_iv <- function(data, factor_var, y, controls,
                                    endog, instruments, cluster,
                                    prefix = factor_var) {
  sample_vars <- unique(c(y, factor_var, controls, endog, instruments, cluster))
  ok <- complete.cases(data[, sample_vars, drop = FALSE])
  vals <- sort(unique(data[[factor_var]][ok]))
  
  if (length(vals) <= 1L) {
    base <- if (length(vals)) vals[1] else NA_real_
    return(list(data = data, names = character(), base = base))
  }
  
  base <- vals[1]
  out_names <- character()
  for (v in vals[-1]) {
    nm <- paste0(prefix, "_", make.names(as.character(v)))
    data[[nm]] <- ifelse(
      is.na(data[[factor_var]]),
      NA_real_,
      as.numeric(data[[factor_var]] == v)
    )
    out_names <- c(out_names, nm)
  }
  
  list(data = data, names = out_names, base = base)
}

# ----------------------------
# Worries
# ----------------------------
d$problemsother <- as.character(d$problemsother)
other_yes <- c(
  "1 fertilizer", "SHE LOST HER SISTER AND HER MUM", "clinic is far from here",
  "clothes for her children", "farming activities", "farming everyday",
  "friends hate her reason not known", "funeral especially at relatives",
  "funeral of relatives", "funerals especially of relatives", "groceries",
  "proble of clean water", "she has 3 orphans which she take care of.",
  "she worries abuot farming", "sickness of relatives", "worried about cloths",
  "worried about field work",
  "worries about who brother who is sick and is admitted to mangohi hospital. This"
)
d$problemsother[d$problemsother %in% other_yes] <- "1"
d$problemsother[idx_true(d$problemsother == "none")] <- "0"

problem_vars <- c("problemshiv", "problemshealth", "problemsschoolfees", "problemsmoney", "problemsfood", "problemsother")
for (v in problem_vars) d[[v]] <- destring_force(d[[v]])

d$worried_hiv <- ifelse(d$problemshiv %in% c(0, 1), d$problemshiv, NA_real_)
d$worried_health <- ifelse(d$problemshealth %in% c(0, 1), d$problemshealth, NA_real_)
d$worried_school <- ifelse(d$problemsschoolfees %in% c(0, 1), d$problemsschoolfees, NA_real_)
d$worried_money <- ifelse(d$problemsmoney %in% c(0, 1), d$problemsmoney, NA_real_)
d$worried_food <- ifelse(d$problemsfood %in% c(0, 1), d$problemsfood, NA_real_)
d$worried_other <- ifelse(d$problemsother %in% c(0, 1), d$problemsother, NA_real_)

worry_vars <- c("worried_hiv", "worried_health", "worried_school", "worried_money", "worried_food", "worried_other")
d$sum_worried <- rowtotal_stata(d[, worry_vars])
d$non_miss_sum_worried <- rownonmiss_stata(d[, worry_vars])
d$sum_worried_nofood <- rowtotal_stata(d[, setdiff(worry_vars, "worried_food")])

nmw <- add_stata_factor_for_iv(
  d,
  factor_var = "non_miss_sum_worried",
  y = "sum_worried",
  controls = demog,
  endog = c("learnhivneg", "learnhivpos"),
  instruments = instrument2,
  cluster = "DK_village_number"
)
d <- nmw$data
nmw_names <- nmw$names

# Estimate the summary-index regression once. The same stored object is used
# in Table A.11 and Table 3.
worries_main <- fit_gmm_iv(
  d, "sum_worried", c(nmw_names, demog),
  c("learnhivneg", "learnhivpos"), instrument2,
  "DK_village_number"
)

models_a11 <- lapply(
  c("worried_hiv", "worried_health", "worried_school", "worried_food", "worried_money"),
  function(y) {
    fit_gmm_iv(
      d, y, demog,
      c("learnhivneg", "learnhivpos"), instrument2,
      "DK_village_number"
    )
  }
)
models_a11[[6]] <- worries_main
write_models_tex(
  models_a11,
  file.path("tables", "tableA11.tex"),
  keep = c("learnhivneg", "learnhivpos"),
  title = "Worries"
)

# ----------------------------
# Plans for future / uncertainty
# ----------------------------
future_vars <- c("futureuncertain", "familynotenoughfood", "notenoughmoneyforbasics", "goddetfuture",
                 "easy500kwa", "soncare", "daughtercare")
for (v in future_vars) d[[v]] <- destring_force(d[[v]])

for (v in c("futureuncertain", "familynotenoughfood", "notenoughmoneyforbasics", "goddetfuture")) {
  out <- rep(NA_real_, nrow(d))
  out[d[[v]] %in% c(4, 44, 444, 4444, 88, 8)] <- 1
  out[d[[v]] %in% c(5, 55, 555)] <- 0
  d[[paste0("new_", v)]] <- out
}
for (v in c("easy500kwa", "soncare", "daughtercare")) {
  out <- rep(NA_real_, nrow(d))
  out[d[[v]] %in% c(4, 44, 444, 4444)] <- 0
  out[d[[v]] %in% c(88, 8, 888, 5, 55, 555, 66)] <- 1
  d[[paste0("new_", v)]] <- out
}

d$any_children <- NA_real_
d$any_children[idx_true(d$new_soncare == 1 & d$new_daughtercare == 1)] <- 1
d$any_children[idx_true(d$new_soncare == 0 | d$new_daughtercare == 0)] <- 0
d$any_children[idx_true(d$new_daughtercare == 1 & is.na(d$new_soncare))] <- 1
d$any_children[idx_true(d$new_soncare == 1 & is.na(d$new_daughtercare))] <- 1

uncertain_vars <- c("new_futureuncertain", "new_familynotenoughfood", "new_notenoughmoneyforbasics",
                    "new_goddetfuture", "new_easy500kwa", "any_children")
d$sum_uncertain <- rowtotal_stata(d[, uncertain_vars])
d$non_miss_sum_uncertain <- rownonmiss_stata(d[, uncertain_vars])
d$sum_uncertain_nofood <- rowtotal_stata(d[, setdiff(uncertain_vars, "new_familynotenoughfood")])

nmu <- add_stata_factor_for_iv(
  d,
  factor_var = "non_miss_sum_uncertain",
  y = "sum_uncertain",
  controls = demog,
  endog = c("learnhivneg", "learnhivpos"),
  instruments = instrument2,
  cluster = "DK_village_number"
)
d <- nmu$data
nmu_names <- nmu$names

# Estimate the summary-index regression once and reuse it in A.12 and Table 3.
uncertainty_main <- fit_gmm_iv(
  d, "sum_uncertain", c(nmu_names, demog),
  c("learnhivneg", "learnhivpos"), instrument2,
  "DK_village_number"
)

models_a12 <- lapply(
  c("new_easy500kwa", "new_futureuncertain", "new_familynotenoughfood",
    "new_notenoughmoneyforbasics", "new_goddetfuture", "any_children"),
  function(y) {
    fit_gmm_iv(
      d, y, demog,
      c("learnhivneg", "learnhivpos"), instrument2,
      "DK_village_number"
    )
  }
)
models_a12[[7]] <- uncertainty_main
write_models_tex(
  models_a12,
  file.path("tables", "tableA12.tex"),
  keep = c("learnhivneg", "learnhivpos"),
  title = "Economic uncertainty"
)

# ----------------------------
# Discounting
# ----------------------------
d$discount500_600 <- destring_force(d$discount500_600)
d$discount500_750 <- destring_force(d$discount500_750)
d$discount500_600[idx_true(d$discount500_600 > 2)] <- NA_real_
d$discount500_750[idx_true(d$discount500_750 > 2)] <- NA_real_
d$discount_other <- destring_force(d$discount_other)

d$time_pref <- NA_real_
d$time_pref[idx_true(d$discount500_600 == 2)] <- 600
d$time_pref[idx_true(d$discount500_600 == 1 & d$discount500_750 == 2)] <- 750
d$time_pref[is.na(d$time_pref) & !is.na(d$discount_other) & d$discount_other > 750] <- d$discount_other[is.na(d$time_pref) & !is.na(d$discount_other) & d$discount_other > 750]
d$ln_time_pref <- log(d$time_pref)
d$time_pref2 <- d$time_pref
d$time_pref2[idx_true(d$time_pref == 600)] <- 550
d$time_pref2[idx_true(d$time_pref == 750)] <- 675
d$ln_time_pref2 <- log(d$time_pref2)
d$time_pref_usd <- d$time_pref * 0.009456
d$ln_time_pref_usd <- log(d$time_pref_usd)
d$time_pref_usd2 <- d$time_pref2 * 0.009456
d$ln_time_pref_usd2 <- log(d$time_pref_usd2)

# Main-paper discounting regression (used again in Table 3)
discount_main <- fit_gmm_iv(d, "time_pref_usd", demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number")

# Appendix robustness regressions are run for parity with Stata, though Stata does not save them to a file.
discount_robust <- list(
  fit_gmm_iv(d, "ln_time_pref_usd", demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number"),
  fit_gmm_iv(d, "time_pref_usd2", demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number")
)
d_discount_trim <- d[is.na(d$time_pref) | d$time_pref <= 2000, , drop = FALSE]
discount_robust[[3]] <- fit_gmm_iv(d_discount_trim, "time_pref_usd", demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number")

# ----------------------------
# Drinking
# ----------------------------
freq <- as.character(d$freqdrink)
ever <- as.character(d$everdrink)
d$drink_past12m <- NA_real_
d$drink_past12m[freq %in% c("1", "2", "3", "4")] <- 1
d$drink_past12m[idx_true(freq == "5")] <- 0
d$drink_past12m[idx_true(ever == "0" & is.na(d$drink_past12m))] <- 0

drunkvars <- c("drunkm", "drunkt", "drunkw", "drunkth", "drunkf", "drunksat", "drunksun")
for (v in drunkvars) {
  raw <- as.character(d[[v]])
  num <- suppressWarnings(as.numeric(raw))
  raw[!is.na(num) & num > 1] <- "1"
  raw[idx_true(raw == "NA")] <- NA_character_
  d[[v]] <- destring_force(raw)
}
d$drunk_nm <- rownonmiss_stata(d[, drunkvars])
d$nb_days_drunk <- NA_real_
all7 <- d$drunk_nm == 7
d$nb_days_drunk[all7] <- rowSums(d[all7, drunkvars, drop = FALSE], na.rm = TRUE)
d$nb_days_drunk[idx_true(d$drink_past12m == 0 & is.na(d$nb_days_drunk))] <- 0
d$drunk_atleast1 <- ifelse(is.na(d$nb_days_drunk), NA_real_, as.numeric(d$nb_days_drunk > 0))
d$drunk_atleast2 <- ifelse(is.na(d$nb_days_drunk), NA_real_, as.numeric(d$nb_days_drunk > 1))
d$offweek <- NA_real_
weekday <- c("drunkm", "drunkt", "drunkw", "drunkth")
d$offweek[all7] <- as.numeric(rowSums(d[all7, weekday, drop = FALSE] > 0, na.rm = TRUE) > 0)
d$offweek[idx_true(d$nb_days_drunk == 0)] <- 0
d$drink_past12m[!is.na(d$nb_days_drunk) & d$nb_days_drunk > 0] <- 1

drinking_outcomes <- c("drink_past12m", "nb_days_drunk", "drunk_atleast1", "drunk_atleast2", "offweek")
models_a13 <- lapply(drinking_outcomes, function(y) {
  fit_gmm_iv(d, y, demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number")
})
drinking_main <- models_a13[[1]]
write_models_tex(models_a13, file.path("tables", "tableA13.tex"), keep = c("learnhivneg", "learnhivpos"), title = "Drinking")

# ----------------------------
# Life expectancy
# ----------------------------
levars <- paste0("le", seq(40, 100, 5))
d$le_nm <- rowSums(vapply(levars, function(v) {
  x <- as.character(d[[v]])
  !is.na(x) & x != ""
}, logical(nrow(d))))
for (v in levars) {
  d[[v]] <- destring_force(d[[v]])
  d[[v]][!is.na(d[[v]]) & d[[v]] > 1] <- NA_real_
}
d$le <- NA_real_
d$le[idx_true(d$le40 == 1)] <- 40
d$le[idx_true(d$le40 == 0)] <- 35
for (j in seq(45, 100, 5)) d$le[idx_true(d[[paste0("le", j)]] == 1)] <- j
d$le_age <- d$le - d$age05
d$le_age[!is.na(d$le_age) & d$le_age < 0] <- 0
d$le_dk <- ifelse(d$le_nm > 0, as.numeric(is.na(d$le)), NA_real_)
d$le45l <- ifelse(!is.na(d$le), as.numeric(d$le > 44), NA_real_)
d$le60l <- ifelse(!is.na(d$le), as.numeric(d$le > 59), NA_real_)
d$le65l <- ifelse(!is.na(d$le), as.numeric(d$le > 64), NA_real_)
d$le55l <- ifelse(!is.na(d$le), as.numeric(d$le > 54), NA_real_)
d$le20 <- ifelse(!is.na(d$le_age), as.numeric(d$le_age > 19), NA_real_)
d$le18 <- ifelse(!is.na(d$le_age), as.numeric(d$le_age > 17), NA_real_)
d$le24 <- ifelse(!is.na(d$le_age), as.numeric(d$le_age > 23), NA_real_)
life_models <- lapply(c("le_age", "le60l", "le18"), function(y) {
  fit_gmm_iv(d, y, demog, c("learnhivneg", "learnhivpos"), instrument2, "DK_village_number")
})
life_main <- life_models[[1]]

# ----------------------------
# Table 3: main mechanisms with Simes q-values
# ----------------------------
# Reuse the exact models already estimated for the component/appendix tables.
# This guarantees:
#   Table 3 col 1 == Table A.11 col 6
#   Table 3 col 2 == Table A.12 col 7
#   Table 3 col 4 == Table A.13 col 1
main_outcomes <- c(
  "sum_worried", "sum_uncertain", "time_pref_usd",
  "drink_past12m", "le_age"
)
main_models <- list(
  worries_main,
  uncertainty_main,
  discount_main,
  drinking_main,
  life_main
)

if (is.null(worries_main$initial_coefficients) || is.null(worries_main$initial_se)) {
  stop("The fitted object does not contain the initial 2SLS stage; stale helper detected.")
}

message(sprintf(
  paste0(
    "Mechanism IV diagnostic (sum_worried, Learn HIV+): ",
    "initial 2SLS = %.6f (SE %.6f); final GMM = %.6f (SE %.6f); ",
    "factor base = %s; helper = %s"
  ),
  unname(worries_main$initial_coefficients["learnhivpos"]),
  unname(worries_main$initial_se["learnhivpos"]),
  coef_of(worries_main, "learnhivpos"),
  se_of(worries_main, "learnhivpos"),
  as.character(nmw$base),
  REPLICATION_HELPER_VERSION
))

message(sprintf(
  paste0(
    "GMM matrix diagnostic: N=%d; clusters=%d; rank(X)=%d/%d; rank(Z)=%d/%d; ",
    "ZZ=%s (rcond %.3e); A0=%s (rcond %.3e); S=%s (rcond %.3e); A=%s (rcond %.3e)"
  ),
  worries_main$diagnostics$n,
  worries_main$diagnostics$clusters,
  worries_main$diagnostics$rank_x, worries_main$diagnostics$k_x,
  worries_main$diagnostics$rank_z, worries_main$diagnostics$k_z,
  worries_main$diagnostics$inv_ZZ$method, worries_main$diagnostics$inv_ZZ$rcond,
  worries_main$diagnostics$inv_A0$method, worries_main$diagnostics$inv_A0$rcond,
  worries_main$diagnostics$inv_S$method, worries_main$diagnostics$inv_S$rcond,
  worries_main$diagnostics$inv_A$method, worries_main$diagnostics$inv_A$rcond
))

coef_neg <- vapply(main_models, coef_of, numeric(1), term = "learnhivneg")
coef_pos <- vapply(main_models, coef_of, numeric(1), term = "learnhivpos")
se_neg <- vapply(main_models, se_of, numeric(1), term = "learnhivneg")
se_pos <- vapply(main_models, se_of, numeric(1), term = "learnhivpos")
p_neg <- vapply(main_models, p_of, numeric(1), term = "learnhivneg")
p_pos <- vapply(main_models, p_of, numeric(1), term = "learnhivpos")
q_neg <- simes_qvalue(p_neg)
q_pos <- simes_qvalue(p_pos)
obs <- vapply(main_models, function(m) m$nobs, numeric(1))
means_neg <- means_pos <- numeric(5)
for (i in seq_along(main_models)) {
  m <- main_models[[i]]
  dd <- d[m$sample, , drop = FALSE]
  y <- main_outcomes[i]
  means_neg[i] <- mean(dd[[y]][dd$hiv == 0 & dd$learnhivneg == 0], na.rm = TRUE)
  means_pos[i] <- mean(dd[[y]][dd$hiv == 1 & dd$learnhivpos == 0], na.rm = TRUE)
}

total <- rbind(
  coef_neg, se_neg, p_neg, q_neg,
  coef_pos, se_pos, p_pos, q_pos,
  obs, means_neg, means_pos
)
rownames(total) <- c("Learn HIV-", "", "p-value", "Simes", "Learn HIV+", "", "p-value", "Simes", "Observations", "means_hiv-", "means_hiv+")
write_matrix_tex(total, file.path("tables", "table3.tex"), row_names = rownames(total))

# Table A.1: mechanism summary statistics
summary_vars <- c(
  "sum_worried", "worried_hiv", "worried_health", "worried_school", "worried_money", "worried_food",
  "sum_uncertain", "new_easy500kwa", "new_futureuncertain", "new_familynotenoughfood",
  "new_notenoughmoneyforbasics", "new_goddetfuture", "any_children", "time_pref_usd",
  "drink_past12m", "nb_days_drunk", "drunk_atleast1", "drunk_atleast2", "offweek", "le_age"
)
write_summary_tex(
  d, summary_vars, file.path("tables", "tableA1.tex"),
  groups = list(All = rep(TRUE, nrow(d)), `HIV-` = d$hiv == 0, `HIV+` = d$hiv == 1),
  include_sd_n = TRUE
)