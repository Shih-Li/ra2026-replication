# 00_run_all.R
# Master runner for Paper 8: Spillovers without Social Interactions in Urban Sanitation.
# Run from the paper repository root with: source("code/00_run_all.R")

rm(list = ls())

source("code/01_setup.R")
source("code/02_variable_lists.R")
source("code/03_helpers.R")

# Start a fresh cross-script state. Table 2 populates selected controls used by Tables 3/G3;
# earlier analysis scripts also save standard errors used by Table G2.
if (file.exists(paths$state)) unlink(paths$state)
save_state(list())

steps <- c(
  "04_table1_balance.R",
  "05_tableG1_attrition.R",
  "06_figure1_decision_spillovers.R",
  "07_table2_decision_spillovers.R",
  "08_table4_health.R",
  "09_table5_social_networks.R",
  "10_table6_learning_coordination.R",
  "11_table7_learning_networks.R",
  "12_table8_social_pressure.R",
  "13_tableG5_price.R",
  "14_table3_multinomial_first.R",
  "15_tableG3_multinomial_how.R",
  "16_table9_reciprocity.R",
  "17_tableG4_remember_neighbors.R",
  "18_tableG2_power.R"
)

for (s in steps) {
  message("[", sub("_.*$", "", s), "] ", s)
  source(file.path("code", s), local = .GlobalEnv)
}

message("Paper 8 replication scripts completed.")
