# ============================================================
# 00_master.R
# Master script for Meriggi et al. vaccine-delivery replication
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

if (RUN_IN_TEXT) {
  message("[1/4] In-text calculations")
  source("code/02_intext_calculations.R")
}

if (RUN_FIGURES) {
  message("[2/4] Main-text figures")
  source("code/03_figures.R")
}

if (RUN_TABLES) {
  message("[3/4] Main and extended-data tables")
  source("code/04_tables.R")
}

if (RUN_SUPPLEMENT) {
  message("[4/4] Supplementary information")
  source("code/05_supplementary.R")
}

message("Replication scripts finished. Outputs are under output/.")
