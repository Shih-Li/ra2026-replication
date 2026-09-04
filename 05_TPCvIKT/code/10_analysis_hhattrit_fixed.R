# 10_analysis_hhattrit_fixed.R
# Preferred analysis entry corresponding to analysis_hhattrit_fixed.do.

run_analysis_hhattrit_fixed <- function(hh, individual, hh_attrit) {
  hh_analysis <- run_household_analysis(hh)
  individual_analysis <- run_individual_analysis(
    individual,
    hh_analysis
  )

  reps_diff <- as.integer(
    Sys.getenv("TPCVIKT_LEE_REPS_DIFF", unset = "500")
  )
  reps_level <- as.integer(
    Sys.getenv("TPCVIKT_LEE_REPS_LEVEL", unset = "50")
  )

  lee_results <- run_lee_bounds(
    hh_attrit,
    reps_diff = reps_diff,
    reps_level = reps_level
  )

  invisible(
    list(
      hh_analysis = hh_analysis,
      individual_analysis = individual_analysis,
      lee_results = lee_results
    )
  )
}
