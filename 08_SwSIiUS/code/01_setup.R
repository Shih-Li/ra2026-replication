# 01_setup.R
# Paths, required source inputs, result directories, and package checks.

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, "code")) && dir.exists(file.path(p, "data"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) stop("Could not find repository root containing code/ and data/.")
    p <- parent
  }
}

project_root <- find_project_root()

paths <- list(
  root = project_root,
  code = file.path(project_root, "code"),
  source_cleaned = file.path(project_root, "data", "source", "cleaned"),
  intermediate = file.path(project_root, "data", "intermediate"),
  results = file.path(project_root, "results"),
  tables = file.path(project_root, "results", "tables"),
  figures = file.path(project_root, "results", "figures")
)

for (p in paths[c("intermediate", "results", "tables", "figures")]) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

paths$data <- list(
  complete_bl_decider = file.path(paths$source_cleaned, "FinalData_CompleteBLDecider.dta"),
  complete_bl_decider_lasso = file.path(paths$source_cleaned, "FinalData_CompleteBLDecider_LASSO.dta"),
  complete_bl_demo = file.path(paths$source_cleaned, "FinalData_CompleteBLDemo.dta"),
  dyadic = file.path(paths$source_cleaned, "FinalData_DyadicData.dta"),
  multi_first = file.path(paths$source_cleaned, "FinalData_Multi_First_4Outcome_LASSO.dta"),
  multi_how = file.path(paths$source_cleaned, "FinalData_Multi_How_5Outcome_LASSO.dta")
)
paths$state <- file.path(paths$intermediate, "analysis_state.rds")
paths$result_prefix <- "R-"

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "stringr", "tibble",
  "fixest", "glmnet", "ggplot2", "survival"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running the replication."
  )
}

options(stringsAsFactors = FALSE, scipen = 999)
