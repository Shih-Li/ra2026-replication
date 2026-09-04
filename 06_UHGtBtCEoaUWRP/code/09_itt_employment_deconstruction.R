# 09_itt_employment_deconstruction.R
# Translation of 2.2.ITT_EmploymentDeconstruction.do up to the source's active exit.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
tx <- existing_vars(d, c("treat_HD", "treat_GD_lower", "treat_GD_middle", "treat_GD_upper", "treat_GD_huge", "treat_combined"))

hrs_employ <- existing_vars(d, c("bn_employed_0", "bn_employed_10", "bn_employed_20", "bn_employed_30", "bn_employed_40"))
type_employ <- existing_vars(d, c("employed_enterp_0", "employed_semploy_0", "employed_agroprocess_0", "employed_farm_0", "employed_noagric_0"))
outcomes <- c(type_employ, hrs_employ)

res <- purrr::map_dfr(outcomes, function(y) {
  m <- fit_model(d, y, c(tx, nonempty_vars(d, "Lbn_employed"), rhs, blocks), weight = "attr_wgt", cluster = "hhid", subset = d$round == 1)
  e <- extract_terms(m, tx)
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl])
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)
  e$joint_p <- joint_zero_test(m, tx)
  e
})
res$q.value <- NA_real_
for (fam in list(type_employ, hrs_employ)) {
  idx <- which(res$outcome %in% fam)
  res$q.value[idx] <- sharpened_qvalues(res$p.value[idx])
}
write_outputs(res, "employment_breakdown.tex")
