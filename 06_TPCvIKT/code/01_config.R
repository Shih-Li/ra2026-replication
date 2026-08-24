# 01_config.R
# Project configuration for Paper 6:
# Testing Paternalism: Cash versus In-Kind Transfers
#
# This file has no build side effects beyond creating generated-data/output
# directories. It is sourced once by _00_master.R.

required_packages <- c(
  "haven", "readr", "dplyr", "tidyr", "fixest",
  "broom", "openxlsx", "ggplot2", "childsds"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

find_project_root <- function() {
  override <- Sys.getenv("TPCVIKT_ROOT", unset = "")
  if (nzchar(override)) return(normalizePath(override, mustWork = FALSE))

  if (exists(".master_code_dir", inherits = TRUE)) {
    candidate <- dirname(get(".master_code_dir", inherits = TRUE))
    if (dir.exists(file.path(candidate, "data"))) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }

  wd <- normalizePath(getwd(), mustWork = FALSE)
  candidates <- unique(c(
    wd,
    dirname(wd),
    dirname(dirname(wd))
  ))

  for (x in candidates) {
    if (
      dir.exists(file.path(x, "code")) &&
      dir.exists(file.path(x, "data"))
    ) {
      return(x)
    }
  }

  if (basename(wd) == "code") return(dirname(wd))
  wd
}

ROOT <- find_project_root()
CODE_DIR <- file.path(ROOT, "code")

RAW_DIR <- file.path(ROOT, "data", "source", "raw")
INTERMEDIATE_DIR <- file.path(ROOT, "data", "intermediate")
PROCESSED_DIR <- file.path(ROOT, "data", "processed")

OUTPUT_DIR <- file.path(ROOT, "output")
PAPER_DIR <- file.path(ROOT, "paper")

BASELINE_DIR <- file.path(RAW_DIR, "Baseline raw data")
FOLLOWUP_DIR <- file.path(RAW_DIR, "1st Followup raw data")

for (d in c(INTERMEDIATE_DIR, PROCESSED_DIR, OUTPUT_DIR, PAPER_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

raw_support <- function(name) file.path(RAW_DIR, name)
baseline_raw <- function(name) file.path(BASELINE_DIR, name)
followup_raw <- function(name) file.path(FOLLOWUP_DIR, name)

TPCVIKT_MODE <- tolower(Sys.getenv("TPCVIKT_MODE", unset = "fixed"))
TPCVIKT_REBUILD <- Sys.getenv("TPCVIKT_REBUILD", unset = "1") != "0"
TPCVIKT_RUN_ANALYSIS <- Sys.getenv("TPCVIKT_RUN_ANALYSIS", unset = "1") != "0"

if (!TPCVIKT_MODE %in% c("fixed", "original", "revised")) {
  stop(
    "TPCVIKT_MODE must be one of: fixed, original, revised.",
    call. = FALSE
  )
}
