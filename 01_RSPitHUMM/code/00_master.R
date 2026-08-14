# ============================================================
# 00_master.R
# Master script for Riley (2023) replication
# ============================================================

rm(list=ls())

# ============================================================
# Automatically detect project directory
# ============================================================

# Full path of this 00_master.R file
this_file <- normalizePath(
  sys.frame(1)$ofile,
  winslash = "/",
  mustWork = TRUE
)

# Folder containing 00_master.R
code_dir <- dirname(this_file)

# Project root = one folder above /code
project_dir <- dirname(code_dir)

# Set project root as working directory
setwd(project_dir)

message("Project directory: ", getwd())

#=============================================================

source("code/01_setup.R")
source("code/functions/weightave.R")

message("[1/5] Take-up analysis")
source("code/02_takeup.R")

message("[2/5] Balance analysis")
source("code/03_balance.R")

message("[3/5] Main and appendix analysis")
source("code/04_main_analysis.R")

message("[4/5] Causal forest")
source("code/05_causal_forest.R")

message("[5/5] K-means heterogeneity")
source("code/06_kmeans.R")

message("Replication scripts finished. Outputs are under output/.")