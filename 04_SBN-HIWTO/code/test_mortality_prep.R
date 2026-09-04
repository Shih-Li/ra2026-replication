# ============================================================================
# Data preparation
# Parallel to: code/test_mortality_prep.do
# Input : data/source/raw/rawdata.dta
# Output: data/processed/hivtest_mortality.rds
# ============================================================================

if (!exists("fit_gmm_iv")) source(file.path("code", "config_stata.R"))

infile <- file.path("data", "source", "raw", "rawdata.dta")
outfile <- file.path("data", "processed", "hivtest_mortality.rds")
if (!file.exists(infile)) stop("Missing required source file: ", infile)

d <- haven::read_dta(infile)

# Sample selection
d <- d %>%
  filter(!is.na(M3_T_posttest_vct), !is.na(M3_T_finalhivres), !is.na(distvct))

# Vital status
d$dead2018 <- ifelse(
  !is.na(d$lastvitalyear) & d$lastvitalyear > 2017 &
    !is.na(d$lastvitalstatus) & d$lastvitalstatus < 3,
  as.numeric(d$lastvitalstatus == 0), NA_real_
)
d$dead2018[!is.na(d$lastvitalstatus) & d$lastvitalstatus == 0] <- 1

d$dead2010 <- NA_real_
d$dead2010[!is.na(d$lastvitalstatus) & d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr < 2011] <- 1
d$dead2010[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2009 & d$lastvitalstatus == 1 & !is.na(d$alivelastyr) & d$alivelastyr > 2009)] <- 0
d$dead2010[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2009 & d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr > 2010 & !is.na(d$alivelastyr) & d$alivelastyr > 2009)] <- 0

d$dead2008 <- NA_real_
d$dead2008[idx_true(d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr < 2009)] <- 1
d$dead2008[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2007 & d$lastvitalstatus == 1 & !is.na(d$alivelastyr) & d$alivelastyr > 2007)] <- 0
d$dead2008[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2007 & d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr > 2008 & !is.na(d$alivelastyr) & d$alivelastyr > 2007)] <- 0

d$dead2006 <- NA_real_
d$dead2006[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2005 & d$lastvitalstatus == 1 & !is.na(d$alivelastyr) & d$alivelastyr > 2005)] <- 0
d$dead2006[idx_true(!is.na(d$lastvitalyear) & d$lastvitalyear > 2005 & d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr > 2007 & !is.na(d$alivelastyr) & d$alivelastyr > 2005)] <- 0
d$dead2006[idx_true(d$lastvitalstatus == 0 & !is.na(d$deadyr) & d$deadyr < 2008)] <- 1

d$dead2010vital <- d$dead2010
d$dead2018vital <- d$dead2018

# Incorporate Phil information
d$dead2010[!is.na(d$dead2010_phil)] <- d$dead2010_phil[!is.na(d$dead2010_phil)]

# Incorporate 2022 vital-status information
d$dead2018new <- d$dead2018
d$dead2018new[idx_true(is.na(d$dead2018new) & d$m12_vital_status == 1)] <- 0
d$dead2018new[idx_true(is.na(d$dead2018new) & d$m12_vital_status == 0 & !is.na(d$death_year) & d$death_year > 2019)] <- 0
d$dead2018new[idx_true(is.na(d$dead2018new) & d$m12_vital_status == 0 & !is.na(d$death_year) & d$death_year < 2020)] <- 1

d$dead2010new <- d$dead2010
d$dead2010new[idx_true(is.na(d$dead2010new) & d$m12_vital_status == 1)] <- 0
d$dead2010new[idx_true(is.na(d$dead2010new) & d$m12_vital_status == 0 & !is.na(d$death_year) & d$death_year > 2010)] <- 0

d$dead2008new <- d$dead2008
d$dead2008new[idx_true(is.na(d$dead2008new) & d$m12_vital_status == 1)] <- 0
d$dead2008new[idx_true(is.na(d$dead2008new) & d$m12_vital_status == 0 & !is.na(d$death_year) & d$death_year > 2010)] <- 0

d$dead2006new <- d$dead2006
d$dead2006new[idx_true(is.na(d$dead2006new) & d$m12_vital_status == 1)] <- 0
d$dead2006new[idx_true(is.na(d$dead2006new) & d$m12_vital_status == 0 & !is.na(d$death_year) & d$death_year > 2010)] <- 0

for (yr in c("2006", "2008", "2010", "2018")) {
  d[[paste0("obs", yr)]] <- as.numeric(!is.na(d[[paste0("dead", yr)]]))
}
d$obs2010vital <- as.numeric(!is.na(d$dead2010vital))
d$obs2018vital <- as.numeric(!is.na(d$dead2018vital))

for (yr in c("2006", "2008", "2010", "2018")) {
  d[[paste0("obs", yr, "new")]] <- as.numeric(!is.na(d[[paste0("dead", yr, "new")]]))
  d[[paste0("dead", yr)]] <- d[[paste0("dead", yr, "new")]]
  d[[paste0("obs", yr)]] <- d[[paste0("obs", yr, "new")]]
}
d <- d[, !grepl("new$", names(d)), drop = FALSE]

# Duplicate observations have vital status set to missing, then are dropped later.
for (v in grep("^(obs|dead)", names(d), value = TRUE)) {
  d[[v]][!is.na(d$duplicate) & d$duplicate == 1] <- NA_real_
}

for (yr in c("2006", "2008", "2010", "2018")) {
  d[[paste0("alive", yr)]] <- 1 - d[[paste0("dead", yr)]]
}

# HIV definition
d$hiv <- ifelse(!is.na(d$M3_T_finalhivres) & d$M3_T_finalhivres < 2,
                as.numeric(d$M3_T_finalhivres == 1), NA_real_)
d$hiv2 <- as.numeric(!is.na(d$M3_T_finalhivres) & d$M3_T_finalhivres > 0)

# Demographics
d <- d %>% rename(male = DK_gender)
d$age <- d$DK_age_2010
d$age04 <- d$DK_age_2010 - 6
d$age2 <- d$age^2
d$schooling <- ifelse(!is.na(d$DK_M3_level_education), as.numeric(d$DK_M3_level_education > 0), NA_real_)
d$married <- ifelse(!is.na(d$DK_M3_marital_status), as.numeric(d$DK_M3_marital_status == 1), NA_real_)
d$south <- stata_bool(d$DK_region == 2)
d$north <- stata_bool(d$DK_region == 3)

# Instruments
d$anyincentive <- ifelse(!is.na(d$M3_T_incentive), as.numeric(d$M3_T_incentive > 0), NA_real_)
d$incentive <- d$M3_T_incentive * 0.009456
d$incentive2 <- d$incentive^2
d$under <- ifelse(!is.na(d$distvct), as.numeric(d$distvct < 1.5), NA_real_)
d$distvct2 <- d$distvct^2
d <- d %>% rename(posttest = M3_T_posttest_vct)

# Interactions
d$learnhivpos <- d$posttest * d$hiv
d$learnhivneg <- d$posttest * (1 - d$hiv)
for (x in c("anyincentive", "incentive", "incentive2", "distvct", "distvct2", "under")) {
  d[[paste0(x, "hiv")]] <- d[[x]] * d$hiv
  for (y in c("age", "male")) {
    d[[paste0(x, y)]] <- d[[x]] * d[[y]]
    d[[paste0(x, y, "hiv")]] <- d[[x]] * d[[y]] * d$hiv
  }
}

d$learnhivpos2 <- d$posttest * d$hiv2
d$learnhivneg2 <- d$posttest * (1 - d$hiv2)
for (x in c("anyincentive", "incentive", "incentive2", "distvct", "distvct2", "under")) {
  d[[paste0(x, "hiv2")]] <- d[[x]] * d$hiv2
  for (y in c("age", "male")) {
    d[[paste0(x, y, "hiv2")]] <- d[[x]] * d[[y]] * d$hiv2
  }
}

# HIV perceptions and testing before 2004
d$tested <- ifelse(!is.na(d$M3_a25) & d$M3_a25 < 2, d$M3_a25, NA_real_)
d$know_results <- stata_bool(d$M3_a26 == 1)
d$likelihood_hiv <- d$M3_a8
d$likelihood_hiv[is.na(d$likelihood_hiv)] <- 99

# Final sample selection
d <- d %>%
  filter(!is.na(hiv), !is.na(age), !is.na(north), is.na(duplicate) | duplicate != 1)

dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
saveRDS(d, outfile)
message("Saved ", outfile, " (", nrow(d), " rows)")
