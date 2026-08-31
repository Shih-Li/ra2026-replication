# ==============================================================================
# Paper 10 - Master Replication Script
# Selecting the Most Effective Nudge
# Runs the full Haryana empirical replication workflow
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. Locate project root
# ------------------------------------------------------------------------------

if (!requireNamespace("this.path", quietly = TRUE)) {
  stop("Package 'this.path' is required. Install it with install.packages('this.path').")
}

master_file <- this.path::this.path()
code_dir <- dirname(normalizePath(master_file, winslash = "/", mustWork = TRUE))
project_root <- dirname(code_dir)

cat("\nPaper 10 replication\n")
cat("Project root:", project_root, "\n\n")

# ------------------------------------------------------------------------------
# 1. Ensure required folders exist
# ------------------------------------------------------------------------------

dirs <- c(
  file.path(project_root, "data", "processed"),
  file.path(project_root, "data", "intermediate"),
  file.path(project_root, "output", "Figure"),
  file.path(project_root, "output", "Table")
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# ------------------------------------------------------------------------------
# 2. Scripts to run, in dependency order
# ------------------------------------------------------------------------------

scripts <- c(
  "0_create_costs_fullsample.R",
  "01_uninteracted_analysis.R",
  "02_main_analysis.R",
  "03_robustness_regularization_path.R",
  "04_saturated_OLS.R",
  "05_bootstrap_main.R",
  "05B_bootstrap_plot.R",
  "06_descriptive_statistics.R",
  "07_evaluating_substitution.R"
)

# ------------------------------------------------------------------------------
# 3. Run all scripts
# ------------------------------------------------------------------------------

for (script in scripts) {

  script_path <- file.path(code_dir, script)

  if (!file.exists(script_path)) {
    stop("Missing script: ", script_path)
  }

  cat("\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
  cat("Running:", script, "\n")
  cat(paste(rep("=", 78), collapse = ""), "\n\n", sep = "")

  tryCatch(
    {
      source(script_path, local = new.env(parent = globalenv()))
      cat("\nCompleted:", script, "\n")
    },
    error = function(e) {
      stop(
        "\nReplication stopped while running: ", script,
        "\nError: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

# ------------------------------------------------------------------------------
# 4. Finish
# ------------------------------------------------------------------------------

cat("\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
cat("ALL HARYANA REPLICATION SCRIPTS COMPLETED\n")
cat("Figures:", file.path(project_root, "output", "Figure"), "\n")
cat("Tables: ", file.path(project_root, "output", "Table"), "\n")
cat("Processed data:", file.path(project_root, "data", "processed"), "\n")
cat("Intermediate data:", file.path(project_root, "data", "intermediate"), "\n")
cat(paste(rep("=", 78), collapse = ""), "\n", sep = "")
