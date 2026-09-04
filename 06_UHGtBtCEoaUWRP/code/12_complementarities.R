# 12_complementarities.R
# Translation of 4.1.Complementarities.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
all_outcomes <- existing_vars(d, c(primary, secondary))

# 1. Pre-specified test.
tx1 <- existing_vars(d, c("HD_complem", "GD_mid_complem", "treat_combined"))
sub1 <- d$round == 1 & (
  d$treat_control == 1 |
    d$treat_HD == 1 |
    d$treat_GD_middle == 1 |
    d$treat_combined == 1
)
r1 <- purrr::map_dfr(all_outcomes, function(y) {
  m <- fit_model(d, y, c(tx1, nonempty_vars(d, paste0("L", y)), rhs, blocks), weight = "attr_wgt", cluster = "hhid", subset = sub1)
  e <- extract_terms(m, tx1)
  e$outcome <- y; e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl]); e$n <- stats::nobs(m); e$r2 <- safe_r2(m)
  e
})
r1 <- apply_q_by_family(r1, list(primary = primary, welfare = secondary_welfare, wealth = secondary_wealth, skills = secondary_skills))
write_outputs(dplyr::filter(r1, outcome %in% primary), "complementarities_primary.tex")
write_outputs(dplyr::filter(r1, outcome %in% secondary), "complementarities_secondary.tex")

# 2. Alternative tests based on disaggregated cash arms.
tx2 <- existing_vars(d, c("treat_HD", "treat_GD_lower", "treat_GD_middle", "treat_GD_upper", "treat_GD_huge", "treat_combined"))
run_alt <- function(outcomes) purrr::map_dfr(outcomes, function(y) {
  m <- fit_model(d, y, c(tx2, nonempty_vars(d, paste0("L", y)), rhs, blocks), weight = "attr_wgt", cluster = "hhid", subset = d$round == 1)
  e <- extract_terms(m, tx2)
  b <- stats::coef(m)
  test1 <- linear_contrast(m, c(treat_combined = 1, treat_GD_middle = -1, treat_HD = -1))
  test2 <- linear_contrast(m, c(treat_combined = 1, treat_GD_huge = -1))
  test3 <- linear_contrast(m, c(treat_combined = 1, treat_GD_huge = -1, treat_HD = -1, treat_GD_lower = 1))
  e$outcome <- y; e$outcome_label <- var_label(d, y)
  e$complementarity_mid_est <- test1["estimate"]; e$complementarity_mid_p <- test1["p.value"]
  e$combined_minus_large_est <- test2["estimate"]; e$combined_minus_large_p <- test2["p.value"]
  e$alt_complementarity_est <- test3["estimate"]; e$alt_complementarity_p <- test3["p.value"]
  e
})
alt_p <- run_alt(existing_vars(d, primary))
alt_s <- run_alt(existing_vars(d, secondary))
write_outputs(alt_p, "complementarities_alt.tex")
write_outputs(alt_s, "complementarities2_alt.tex")
