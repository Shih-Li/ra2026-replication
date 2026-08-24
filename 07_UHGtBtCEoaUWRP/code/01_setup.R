# 01_setup.R
# Project paths and package setup. All analysis scripts are parallel in code/.

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (dir.exists(file.path(p, "code")) &&
        dir.exists(file.path(p, "data"))) {
      return(p)
    }
    
    parent <- dirname(p)
    
    if (identical(parent, p)) {
      stop("Could not find repository root containing code/ and data/.")
    }
    
    p <- parent
  }
}

project_root <- find_project_root()

paths <- list(
  root = project_root,
  
  code = file.path(project_root, "code"),
  
  source_cleaned = file.path(
    project_root, "data", "source", "cleaned"
  ),
  
  source_auxiliary = file.path(
    project_root, "data", "source", "auxiliary"
  ),
  
  intermediate = file.path(
    project_root, "data", "intermediate"
  ),
  
  processed = file.path(
    project_root, "data", "processed"
  ),
  
  results = file.path(
    project_root, "results"
  ),
  
  tables = file.path(
    project_root, "results", "tables"
  ),
  
  figures = file.path(
    project_root, "results", "figures"
  )
)

# Create generated-data and result directories
for (p in paths[c(
  "intermediate",
  "processed",
  "results",
  "tables",
  "figures"
)]) {
  dir.create(
    p,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# ------------------------------------------------------------------
# Source inputs
# ------------------------------------------------------------------

paths$panel <- file.path(
  paths$source_cleaned,
  "HD_panel_clean.dta"
)

paths$cost <- file.path(
  paths$source_cleaned,
  "HD_cost.xlsx"
)

paths$kitchensink <- file.path(
  paths$source_auxiliary,
  "kitchensink.csv"
)

# ------------------------------------------------------------------
# Generated intermediate data
# ------------------------------------------------------------------

paths$controls <- file.path(
  paths$intermediate,
  "HD_controls.csv"
)

# ------------------------------------------------------------------
# Result filename prefix
# ------------------------------------------------------------------

paths$result_prefix <- "R-"

# ------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------

required_packages <- c(
  "haven",
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "tibble",
  "fixest",
  "glmnet",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages)) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running the replication."
  )
}

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

set.seed(1234567)