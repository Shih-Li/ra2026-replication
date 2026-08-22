# Entry point corresponding to analysis_hhattrit_fixed.do.
# Run from the paper project root, e.g.:
#   Rscript code/analysis_hhattrit_fixed.R
#
# Set TPCVIKT_REBUILD=0 to reuse data/processed/hh.rds and individual.rds.

source(file.path(if (dir.exists(file.path(getwd(), "code"))) getwd() else dirname(getwd()), "code", "build_household.R"), local = FALSE)
source(file.path(ROOT, "code", "analysis_core.R"), local = FALSE)

rebuild <- Sys.getenv("TPCVIKT_REBUILD", unset = "1") != "0"
if (rebuild || !file.exists(file.path(PROCESSED_DIR, "hh.rds")) || !file.exists(file.path(PROCESSED_DIR, "individual.rds"))) {
  built <- build_household(save = TRUE, build_attrition_ready = TRUE)
  hh <- built$hh
  individual <- built$individual
} else {
  hh <- load_processed("hh")
  individual <- load_processed("individual")
}

# analysis_hhattrit_fixed.do explicitly builds the attrition-inclusive
# analysis-ready household sample before the Lee-bounds exercise.
if (rebuild) {
  hh_attrit <- built$hh_attrit
} else if (file.exists(file.path(PROCESSED_DIR, "hh_attrit.rds"))) {
  hh_attrit <- load_processed("hh_attrit")
} else {
  built <- build_household(save = TRUE, build_attrition_ready = TRUE)
  hh <- built$hh
  individual <- built$individual
  hh_attrit <- built$hh_attrit
}

# Run the standard analysis outputs plus the attrition bounds.
hh_analysis <- run_household_analysis(hh)
individual_analysis <- run_individual_analysis(individual, hh_analysis)

reps_diff <- as.integer(Sys.getenv("TPCVIKT_LEE_REPS_DIFF", unset = "500"))
reps_level <- as.integer(Sys.getenv("TPCVIKT_LEE_REPS_LEVEL", unset = "50"))
lee_results <- run_lee_bounds(hh_attrit, reps_diff = reps_diff, reps_level = reps_level)

message("analysis_hhattrit_fixed.R complete.")
message("Lee-bounds output: ", file.path(OUTPUT_DIR, "cons_agg_leebounds.xlsx"))

