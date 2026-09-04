# 12_analysis_revised.R
# Analysis entry corresponding to analysis_revised.do.
#
# The shared analysis core rebuilds the extra-marginal variables
# deterministically, so the revised path is restart-safe.

run_analysis_revised <- function(hh, individual) {
  hh_analysis <- run_household_analysis(hh)
  individual_analysis <- run_individual_analysis(
    individual,
    hh_analysis
  )

  invisible(
    list(
      hh_analysis = hh_analysis,
      individual_analysis = individual_analysis
    )
  )
}
