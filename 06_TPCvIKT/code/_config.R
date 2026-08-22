# Paper 6: Testing Paternalism: Cash versus In-Kind Transfers
# Project-wide paths and dependency checks.
# All scripts in this package are intentionally flat under code/.

required_packages <- c(
  "haven", "readr", "dplyr", "tidyr", "fixest",
  "broom", "openxlsx", "ggplot2", "childsds"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

find_project_root <- function() {
  override <- Sys.getenv("TPCVIKT_ROOT", unset = "")
  if (nzchar(override)) return(normalizePath(override, mustWork = FALSE))

  wd <- normalizePath(getwd(), mustWork = FALSE)
  candidates <- unique(c(wd, dirname(wd)))
  for (x in candidates) {
    if (dir.exists(file.path(x, "data"))) return(x)
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

for (d in c(INTERMEDIATE_DIR, PROCESSED_DIR, OUTPUT_DIR, PAPER_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

baseline_dir <- file.path(RAW_DIR, "Baseline raw data")
followup_dir <- file.path(RAW_DIR, "1st Followup raw data")

# Files directly under data/source/raw/ in the clean R repository.
raw_support <- function(name) file.path(RAW_DIR, name)
baseline_raw <- function(name) file.path(baseline_dir, name)
followup_raw <- function(name) file.path(followup_dir, name)

message("TPCvIKT project root: ", ROOT)
