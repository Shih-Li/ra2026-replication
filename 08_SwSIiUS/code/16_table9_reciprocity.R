# 16_table9_reciprocity.R
# Stata Table 9 (original analysis lines ~1692-1739): reciprocity in dictator games.

d <- read_source("dyadic")
d <- d[d$games == 1 & !is.na(d$s23_7gamekeep_el) & d$s23_7gamekeep_el >= 0, , drop = FALSE]

rhs <- list(
  subsidy_sign = c("amt_given_bl", "nghbrsubsidy", "nghbrsubsidylow", "ngbr_highsign", "ngbr_lowsign"),
  subsidy_manu = c("amt_given_bl", "nghbrsubsidy", "nghbrsubsidylow", "nghbrdesrawmanualel", "nghbrmanuhigh", "nghbrmanulow"),
  subsidy_sign_pub = c("amt_given_bl", "nghbrsubsidy", "nghbrsubsidylow", "ngbr_highsign", "ngbr_lowsign",
                       "ngbr_highpub", "ngbr_lowpub", "ngbr_signpubhigh", "ngbr_signpublow"),
  subsidy_pub_manu = c("amt_given_bl", "nghbrsubsidy", "nghbrsubsidylow", "nghbrdesrawmanualel", "nghbrmanuhigh", "nghbrmanulow",
                       "ngbr_highpub", "ngbr_lowpub", "nghbrmanupub", "nghbrmanupubhigh", "nghbrmanupublow")
)
models <- list(); means <- numeric()
for (nm in names(rhs)) {
  fit <- fit_feols(d, "amt_given_el", rhs[[nm]], fe = "hhid", cluster = "s0d_dclusterid")
  models[[nm]] <- fit$model
  means[[nm]] <- mean(
    fit$data$amt_given_el[fit$data$nghbrsubsidy == 0 & fit$data$nghbrsubsidylow == 0], na.rm = TRUE
  )
}
terms <- unique(unlist(rhs, use.names = FALSE))
write_models_tex(
  models, result_path("Table9_Reciprocity.tex", "table"), terms = terms, labels = terms,
  titles = c("Signs", "Manual", "Signs + Public", "Manual + Public"), means = means, digits = 2
)
