# 01_setup.R
# Paths and package checks for Cai and Szeidl, "Indirect Effects of Access to Finance".
# All paths are relative to the clean replication repository root.

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, "code")) && dir.exists(file.path(p, "data"))) return(p)
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
  intermediate = file.path(project_root, "data", "intermediate"),
  processed = file.path(project_root, "data", "processed"),
  results = file.path(project_root, "results"),
  tables = file.path(project_root, "results", "tables"),
  figures = file.path(project_root, "results", "figures"),
  result_prefix = "R-"
)

for (p in paths[c("intermediate", "processed", "results", "tables", "figures")]) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

paths$loanmain <- file.path(paths$source_cleaned, "loanmain.dta")
paths$market <- file.path(paths$source_cleaned, "market.dta")
paths$loan_welfare <- file.path(paths$source_cleaned, "loan_welfare.dta")
paths$figure3_xlsx <- file.path(paths$source_cleaned, "Figure3_do.xlsx")

required_inputs <- unlist(paths[c("loanmain", "market", "loan_welfare")])
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop(
    "Missing required source input(s):\n- ",
    paste(missing_inputs, collapse = "\n- "),
    "\n\nPlace loanmain.dta, market.dta, and loan_welfare.dta under data/source/cleaned/.",
    call. = FALSE
  )
}

# Figure3_do.xlsx is an author-prepared input used only by 09_figure3.R.
# It is optional for the rest of the replication and is checked there.

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "fixest", "ggplot2", "patchwork", "knitr"
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

options(stringsAsFactors = FALSE, scipen = 999)

# Match the original resampling settings by default.
options(
  ra2026.rwolf_reps = 1000L,
  ra2026.welfare_reps = 1000L
)
