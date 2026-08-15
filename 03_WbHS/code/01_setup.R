# ============================================================
# 01_setup.R
# Common setup and helpers for paper 3 replication
# Gallego, Larroulet & Repetto, "What's Behind Her Smile?"
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(456)

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "sandwich", "lmtest",
  "car", "ivreg", "ggplot2", "knitr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(sandwich)
  library(lmtest)
  library(car)
  library(ivreg)
  library(ggplot2)
  library(knitr)
})

ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
RAW_DATA_DIR <- file.path(ROOT, "data", "source", "raw")
CLEAN_DATA_DIR <- file.path(ROOT, "data", "source", "cleaned")
PROCESSED_DATA_DIR <- file.path(ROOT, "data", "processed")
OUT_MAIN_TABLE <- file.path(ROOT, "output", "tables", "main")
OUT_APP_TABLE <- file.path(ROOT, "output", "tables", "appendix")
OUT_MAIN_FIG <- file.path(ROOT, "output", "figures", "main")
OUT_DIAG <- file.path(ROOT, "output", "diagnostics")

invisible(lapply(
  c(PROCESSED_DATA_DIR, OUT_MAIN_TABLE, OUT_APP_TABLE, OUT_MAIN_FIG, OUT_DIAG),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

# Match the original Stata repetition counts unless overridden by environment variables.
options(
  wbhs.rw_reps = as.integer(Sys.getenv("WBHS_RW_REPS", unset = "200")),
  wbhs.ipw_boot_reps = as.integer(Sys.getenv("WBHS_IPW_BOOT_REPS", unset = "200"))
)

source(file.path(ROOT, "code", "99_helpers.R"))
