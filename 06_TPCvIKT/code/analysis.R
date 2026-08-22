# Entry point corresponding to analysis.do.
# Run from the paper project root, e.g.:
#   Rscript code/analysis.R
#
# Set TPCVIKT_REBUILD=0 to reuse data/processed/hh.rds and individual.rds.

source(file.path(if (dir.exists(file.path(getwd(), "code"))) getwd() else dirname(getwd()), "code", "build_household.R"), local = FALSE)
source(file.path(ROOT, "code", "analysis_core.R"), local = FALSE)

rebuild <- Sys.getenv("TPCVIKT_REBUILD", unset = "1") != "0"
if (rebuild || !file.exists(file.path(PROCESSED_DIR, "hh.rds")) || !file.exists(file.path(PROCESSED_DIR, "individual.rds"))) {
  built <- build_household(save = TRUE, build_attrition_ready = FALSE)
  hh <- built$hh
  individual <- built$individual
} else {
  hh <- load_processed("hh")
  individual <- load_processed("individual")
}

# Main analysis.do path.
# The shared EM function is restart-safe; on a newly constructed hh dataset
# this reproduces the original definitions and results while avoiding Stata's
# interrupted-run variable-collision problem.
hh_analysis <- run_household_analysis(hh)
individual_analysis <- run_individual_analysis(individual, hh_analysis)

message("analysis.R complete. Tables: ", OUTPUT_DIR)
message("Figures: ", PAPER_DIR)

