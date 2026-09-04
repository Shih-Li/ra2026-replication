# 00_run_all.R
# Master runner for Cai and Szeidl, "Indirect Effects of Access to Finance".
# Run from the repository root: source("code/00_run_all.R")

rm(list = ls())

source("code/01_setup.R")
source("code/02_helpers.R")

message("[03] Baseline, borrowing, and Figure 2")
source("code/03_baseline_borrowing.R")

message("[04] Main effects and robustness")
source("code/04_main_effects_robustness.R")

message("[05] Business and consumer outcomes")
source("code/05_business_consumer.R")

message("[06] Market effects")
source("code/06_market_effects.R")

message("[07] Local spillovers and IV")
source("code/07_local_spillovers_iv.R")

message("[08] Welfare and returns")
source("code/08_welfare.R")

message("[09] Figure 3 (optional author workbook)")
source("code/09_figure3.R")

message("Replication scripts completed.")
