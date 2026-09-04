# 00_run_all.R
# Master runner for Paper 11:
# Westberg, Skjeflo, and Kallbekken,
# "The Power of Information: A Survey Experiment on Public Support
# for Electricity Price Compensation Schemes"
#
# Run from the paper replication root with:
# source("code/00_run_all.R")

rm(list = ls())

source("code/01_setup.R")

steps <- c(
  "02_data_prep_quality.R",
  "03_descriptives.R",
  "04_primary_hypotheses.R",
  "05_prior_beliefs.R",
  "06_heterogeneity_beliefs.R",
  "07_financial_impact.R",
  "08_session_info.R"
)

for (s in steps) {
  message("[", sub("_.*$", "", s), "] ", s)
  source(file.path("code", s), local = .GlobalEnv)
}

message("Paper 11 replication scripts completed.")
