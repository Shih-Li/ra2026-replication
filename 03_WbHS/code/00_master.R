# ============================================================
# 00_master.R
# Master script for paper 3 replication
# Gallego, Larroulet & Repetto, "What's Behind Her Smile?"
# ============================================================

rm(list = ls())

this_file <- normalizePath(
  sys.frame(1)$ofile,
  winslash = "/",
  mustWork = TRUE
)
code_dir <- dirname(this_file)
project_dir <- dirname(code_dir)
setwd(project_dir)

message("Project directory: ", getwd())
source("code/01_setup.R")

required_inputs <- c(
  file.path(RAW_DATA_DIR, "random_anon.dta"),
  file.path(RAW_DATA_DIR, "BL_anon.dta"),
  file.path(RAW_DATA_DIR, "PICTURES_anon.dta"),
  file.path(RAW_DATA_DIR, "EXAM_anon.dta"),
  file.path(RAW_DATA_DIR, "FU1_anon.dta"),
  file.path(RAW_DATA_DIR, "FU2_anon.dta"),
  file.path(CLEAN_DATA_DIR, "Casen_employment_2009to2015.dta")
)
require_files(required_inputs)

message("[1/4] Building analysis dataset")
source("code/02_data_prep.R")

message("[2/4] Main Tables 1-8")
source("code/03_tables_1_to_8.R")

message("[3/4] Appendix Tables 1-5")
source("code/04_appendix.R")

message("[4/4] Figure 1")
source("code/05_figures.R")

message("Replication scripts finished. Outputs are under output/.")
