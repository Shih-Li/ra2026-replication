# 08_itt.R
# Translation of 2.1.ITT.do.
# SOURCE NOTE: the uploaded Stata file contains an unconditional `exit` after
# diagnostics, before the main table-producing block. This R translation proceeds
# through that substantive block so the replication can regenerate reported output.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

costs <- read_costs()
rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
tx <- existing_vars(d, c("treat_HD", "treat_GD_lower", "treat_GD_middle", "treat_GD_upper", "treat_GD_huge", "treat_combined"))
all_outcomes <- existing_vars(d, c(primary, secondary_welfare, secondary_wealth, secondary_skills))

# Diagnostics that precede the Stata debug exit.
diag_zero <- purrr::map_dfr(intersect(c("bn_monthly_income", "bn_tot_prod_assetval", "hh_month_consumption_pc"), names(d)), function(y) {
  z <- d[[y]][d$round == 1 & !is.na(d[[y]])]
  tibble::tibble(outcome = y, n = length(z), zeros = sum(z == 0), share_zero = mean(z == 0))
})
readr::write_csv(diag_zero, file.path(paths$tables, "itt_zero_diagnostics.csv"))

rows <- list()
bcr_rows <- list()
for (y in all_outcomes) {
  lag_y <- paste0("L", y)
  terms <- c(tx, nonempty_vars(d, lag_y), rhs, blocks)
  m <- fit_model(d, y, terms, weight = "attr_wgt", cluster = "hhid", subset = d$round == 1)
  e <- extract_terms(m, tx)
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl])
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)

  cget <- function(nm) cost_value(costs, sub("^treat_", "", nm))
  p_hd_lower <- if (all(c("treat_HD", "treat_GD_lower") %in% tx)) linear_contrast(m, c(treat_HD = 1/cget("treat_HD"), treat_GD_lower = -1/cget("treat_GD_lower")))["p.value"] else NA
  p_lower_huge <- if (all(c("treat_GD_lower", "treat_GD_huge") %in% tx)) linear_contrast(m, c(treat_GD_lower = 1/cget("treat_GD_lower"), treat_GD_huge = -1/cget("treat_GD_huge")))["p.value"] else NA
  p_comb_huge <- if (all(c("treat_combined", "treat_GD_huge") %in% tx)) linear_contrast(m, c(treat_combined = 1/cget("treat_combined"), treat_GD_huge = -1/cget("treat_GD_huge")))["p.value"] else NA
  e$p_hd_vs_lower_per_dollar <- p_hd_lower
  e$p_lower_vs_large_per_dollar <- p_lower_huge
  e$p_combined_vs_large_per_dollar <- p_comb_huge
  rows[[y]] <- e

  b <- stats::coef(m); V <- stats::vcov(m)
  br <- purrr::map_dfr(tx, function(t) {
    cc <- cget(t)
    tibble::tibble(
      outcome = y, outcome_label = var_label(d, y), term = t,
      estimate = if (t %in% names(b)) b[[t]] / cc * 100 else NA_real_,
      std.error = if (t %in% names(b)) sqrt(V[t, t]) / cc * 100 else NA_real_,
      p_hd_vs_lower_per_dollar = p_hd_lower,
      p_lower_vs_large_per_dollar = p_lower_huge,
      p_combined_vs_large_per_dollar = p_comb_huge
    )
  })
  bcr_rows[[y]] <- br
}
res <- dplyr::bind_rows(rows)
families <- list(primary = primary, welfare = secondary_welfare, wealth = secondary_wealth, skills = secondary_skills)
res <- apply_q_by_family(res, families)

primary_res <- dplyr::filter(res, outcome %in% primary)
secondary_res <- dplyr::filter(res, outcome %in% secondary)
write_outputs(primary_res, "itt_fullspec_primary.tex")
write_outputs(secondary_res, "itt_fullspec_secondary.tex")

# Family-specific no-p-value display versions in the Stata workflow.
write_outputs(primary_res[, setdiff(names(primary_res), c("p_hd_vs_lower_per_dollar", "p_lower_vs_large_per_dollar", "p_combined_vs_large_per_dollar"))], "itt_fullspec_primary_nopvals.tex")
for (fam in c("secondary_welfare", "secondary_wealth", "secondary_skills")) {
  yy <- get(fam)
  z <- dplyr::filter(res, outcome %in% yy)
  z <- z[, setdiff(names(z), c("p_hd_vs_lower_per_dollar", "p_lower_vs_large_per_dollar", "p_combined_vs_large_per_dollar"))]
  write_outputs(z, paste0("itt_fullspec_", fam, "_nopvals.tex"))
}

bcr <- dplyr::bind_rows(bcr_rows)
write_outputs(dplyr::filter(bcr, outcome %in% primary), "benefit_cost_ratios_primary.tex")
write_outputs(dplyr::filter(bcr, outcome %in% secondary), "benefit_cost_ratios_secondary.tex")
