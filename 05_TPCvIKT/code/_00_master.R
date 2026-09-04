# _00_master.R
# Single entry point for Paper 6.
#
# Default:
#   source("code/_00_master.R")
#
# Optional environment variables:
#   TPCVIKT_ROOT         project root override
#   TPCVIKT_REBUILD      1 = rebuild generated data (default), 0 = reuse RDS
#   TPCVIKT_RUN_ANALYSIS 1 = run tables/figures (default), 0 = build data only
#   TPCVIKT_MODE         fixed (default), original, revised

# Locate code/ before 01_config.R defines ROOT/CODE_DIR.
.master_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
  error = function(e) ""
)

if (nzchar(.master_file)) {
  .master_code_dir <- dirname(.master_file)
} else if (dir.exists(file.path(getwd(), "code"))) {
  .master_code_dir <- file.path(getwd(), "code")
} else if (basename(getwd()) == "code") {
  .master_code_dir <- getwd()
} else {
  stop(
    "Cannot locate code/. Run from the project root or set TPCVIKT_ROOT.",
    call. = FALSE
  )
}

source(file.path(.master_code_dir, "01_config.R"), local = FALSE)
options(error = rlang::entrace)

for (.script in c(
  "02_helpers.R",
  "03_checks.R",
  "04_build_samples.R",
  "05_build_household_modules.R",
  "06_build_individual.R",
  "07_merge_household.R",
  "08_build_analysis_data.R",
  "09_analysis_core.R",
  "10_analysis_hhattrit_fixed.R",
  "11_analysis_original.R",
  "12_analysis_revised.R"
)) {
  source(file.path(CODE_DIR, .script), local = FALSE)
}

message("TPCvIKT project root: ", ROOT)
message("Mode: ", TPCVIKT_MODE)

run_preflight()

if (TPCVIKT_REBUILD) {
  clear_generated_rds()
  
  samples <- make_hh_samples(save = TRUE)
  
  modules <- build_household_modules(
    samples,
    save = TRUE
  )
  
  individual <- build_individual(
    b_hh_sample = samples$b,
    f_hh_sample = samples$f,
    transfers = modules$transfers,
    save = TRUE
  )
  
  intermediates <- build_household_intermediates(
    modules = modules,
    individual = individual,
    save = TRUE
  )
  
  analysis_data <- build_analysis_datasets(
    intermediates,
    save = TRUE
  )
  
  hh <- analysis_data$hh
  hh_attrit <- analysis_data$hh_attrit
} else {
  needed_rds <- c(
    file.path(PROCESSED_DIR, "hh.rds"),
    file.path(PROCESSED_DIR, "hh_attrit.rds"),
    file.path(PROCESSED_DIR, "individual.rds")
  )
  
  if (!all(file.exists(needed_rds))) {
    stop(
      "TPCVIKT_REBUILD=0 but required processed RDS files are missing.",
      call. = FALSE
    )
  }
  
  hh <- load_processed("hh")
  hh_attrit <- load_processed("hh_attrit")
  individual <- load_processed("individual")
}

validate_analysis_sample(hh, "processed hh")

if (TPCVIKT_RUN_ANALYSIS) {
  analysis_result <- switch(
    TPCVIKT_MODE,
    fixed = run_analysis_hhattrit_fixed(
      hh = hh,
      individual = individual,
      hh_attrit = hh_attrit
    ),
    original = run_analysis_original(
      hh = hh,
      individual = individual
    ),
    revised = run_analysis_revised(
      hh = hh,
      individual = individual
    )
  )
}

message(
  "Build complete. Checkpoints: ",
  file.path(OUTPUT_DIR, "build_checkpoints.csv")
)
