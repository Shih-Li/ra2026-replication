# 01_setup.R
# Paths, packages, and input checks for Paper 11.
# All paths are relative to the clean replication repository root.

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, "code")) && dir.exists(file.path(p, "data"))) {
      return(p)
    }
    parent <- dirname(p)
    if (identical(parent, p)) {
      stop("Could not find repository root containing code/ and data/.", call. = FALSE)
    }
    p <- parent
  }
}

project_root <- find_project_root()

paths <- list(
  root = project_root,
  code = file.path(project_root, "code"),
  source_cleaned = file.path(project_root, "data", "source", "cleaned"),
  results = file.path(project_root, "results"),
  tables = file.path(project_root, "results", "tables"),
  figures = file.path(project_root, "results", "figures"),
  result_prefix = "R-"
)

for (p in paths[c("results", "tables", "figures")]) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

paths$surveydata <- file.path(paths$source_cleaned, "surveydata.rds")

if (!file.exists(paths$surveydata)) {
  stop(
    "Missing required source input:\n- ", paths$surveydata,
    "\n\nPlace the authors' surveydata.rds under data/source/cleaned/.",
    call. = FALSE
  )
}

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "scales", "haven", "gtsummary",
  "MASS", "stargazer", "sandwich", "lmtest", "broom", "writexl",
  "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Missing R package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running the replication.",
    call. = FALSE
  )
}

# Attach the packages used throughout the authors' QMD. MASS is attached after
# dplyr, matching the original workflow; use dplyr::select explicitly where needed.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(gtsummary)
  library(MASS)
  library(stargazer)
  library(sandwich)
  library(lmtest)
  library(broom)
  library(writexl)
})

# The authors set the Norwegian UTF-8 locale. Availability varies by OS, so try
# the requested locale without making the whole replication fail if unavailable.
try(Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8"), silent = TRUE)

options(stringsAsFactors = FALSE, scipen = 999)

result_file <- function(kind = c("tables", "figures"), filename) {
  kind <- match.arg(kind)
  file.path(paths[[kind]], paste0(paths$result_prefix, filename))
}
