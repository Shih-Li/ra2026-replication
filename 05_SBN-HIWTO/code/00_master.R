# ============================================================================
# Master file
# Parallel to: code/00_master.do
# Surviving Bad News: Health Information Without Treatment Options
# ============================================================================

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  this_file <- normalizePath(sub("^--file=", "", file_arg[1]))
  root <- normalizePath(file.path(dirname(this_file), ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
  if (basename(root) == "code") root <- dirname(root)
}
setwd(root)

source(file.path("code", "config_stata.R"))

dir.create(file.path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

message("[1/7] Preparing mortality analysis data")
source(file.path("code", "test_mortality_prep.R"), local = new.env(parent = globalenv()))

message("[2/7] Mortality analysis")
source(file.path("code", "test_mortality_analysis.R"), local = new.env(parent = globalenv()))

message("[3/7] Mechanisms / 2005 follow-up")
source(file.path("code", "multiple_test.R"), local = new.env(parent = globalenv()))

message("[4/7] Instrument tests")
source(file.path("code", "test_instruments.R"), local = new.env(parent = globalenv()))

message("[5/7] Selection robustness: 2008")
source(file.path("code", "selection_robustness_2008.R"), local = new.env(parent = globalenv()))

message("[6/7] Selection robustness: 2010")
source(file.path("code", "selection_robustness_2010.R"), local = new.env(parent = globalenv()))

message("[7/7] Selection robustness: 2018")
source(file.path("code", "selection_robustness_2018.R"), local = new.env(parent = globalenv()))

message("Replication workflow complete.")
