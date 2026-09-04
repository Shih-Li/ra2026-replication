# 07_hd_uptake.R
# Translation of 1.3.HD_uptake.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("uptake")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

d <- read_panel()
if ("hh_asset_val" %in% names(d)) d$hh_asset_val <- d$hh_asset_val / 1e6
covars <- existing_vars(d, balancevars)
blocks <- block_vars(d)
outcomes <- existing_vars(d, uptake)
sub <- d$round == 0 & (d$treat_HD == 1 | d$treat_combined == 1)

res <- purrr::map_dfr(outcomes, function(y) {
  m <- fit_model(d, y, c(covars, blocks), weight = NULL, cluster = "hhid", subset = sub)
  e <- extract_terms(m, covars)
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  e$completion_rate_hd <- mean(d[[y]][d$round == 0 & d$treat_HD == 1], na.rm = TRUE)
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)
  e$joint_p <- joint_zero_test(m, covars)
  e
})
res$q.value <- sharpened_qvalues(res$p.value)
res <- res[, c("outcome", "outcome_label", "term", "estimate", "std.error", "p.value", "q.value", "completion_rate_hd", "n", "r2", "joint_p")]
write_outputs(res, "HD_completion.tex", digits = 4)
