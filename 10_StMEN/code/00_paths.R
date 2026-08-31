# Centralized project paths for Paper 10 replication
# Assumes this file is located at <project_root>/code/00_paths.R

code_dir <- dirname(normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE))
project_root <- normalizePath(file.path(code_dir, ".."), winslash = "/", mustWork = FALSE)

path_data_raw <- file.path(project_root, "data", "source", "raw")
path_data_cleaned <- file.path(project_root, "data", "source", "cleaned")
path_processed_data <- file.path(project_root, "data", "processed")
path_intermediate_data <- file.path(project_root, "data", "intermediate")

path_figures <- file.path(project_root, "output", "Figure")
path_tables <- file.path(project_root, "output", "Table")
path_functions <- file.path(code_dir, "functions")

# Generated-data and final-output locations only.
dir.create(path_processed_data, recursive = TRUE, showWarnings = FALSE)
dir.create(path_intermediate_data, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
