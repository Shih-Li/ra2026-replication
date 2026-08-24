# 11_analysis_original.R
# Analysis entry corresponding to the original analysis.do.

run_analysis_original <- function(hh, individual) {
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
