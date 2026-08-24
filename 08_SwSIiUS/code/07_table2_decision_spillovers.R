# 07_table2_decision_spillovers.R
# Stata Table 2 (original analysis lines ~1021-1123): decision spillovers.
# This script also saves selected Table-2 controls for Tables 3/G3 and SEs for Table G2.

d <- read_source("complete_bl_decider_lasso")
treatments <- c("deposit", "neighbor_public", "split_who_s5", "split_number_s5")
base_candidates <- unique(c(controls_all, treatments, miss_vars(d)))

run_group <- function(outcomes, subset, interest, always = character(), candidates = base_candidates,
                      mean_subset, state_suffix, output, titles) {
  fits <- list(); means <- numeric(length(outcomes))
  names(means) <- outcomes
  for (y in outcomes) {
    fit <- dsregress_r(d, y, interest, always = always, candidates = candidates, subset = subset)
    fits[[y]] <- fit$model
    means[[y]] <- mean(fit$data[[y]][mean_subset(fit$data)], na.rm = TRUE)
    z <- coef_se(fit$model, interest[[1]])
    state_put(paste0(y, "_t2_", state_suffix, "_se"), z[["se"]])
    if (state_suffix == "f") state_put(paste0("selected_table2_", y), fit$selected)
  }
  write_models_tex(
    fits, result_path(output, "table"), terms = rev(interest), labels = rev(interest),
    titles = titles, means = means
  )
}

run_group(
  c("signup_all", "firstdslg", "since_mechaniz_el", "since_manual_el"),
  d$treated == 1, c("neighbor_high", "subsidy_high"),
  mean_subset = function(x) x$subsidy_high == 0,
  state_suffix = "t", output = "Table2_DecisionSpillover_Treated.tex",
  titles = c("Signed Up", "Subsidized Desludging", "Mechanized", "Manual")
)

run_group(
  c("since_mechaniz_el", "since_manual_el"),
  d$spillover == 1, "neighbor_high", candidates = unique(c(controls_all, miss_vars(d))),
  mean_subset = function(x) rep(TRUE, nrow(x)),
  state_suffix = "s", output = "Table2_DecisionSpillover_Spillover.tex",
  titles = c("Mechanized", "Manual")
)

run_group(
  c("since_mechaniz_el", "since_manual_el"),
  rep(TRUE, nrow(d)), c("neighbor_high", "subsidy_high"), always = "spillover",
  mean_subset = function(x) x$subsidy_high == 0,
  state_suffix = "f", output = "Table2_DecisionSpillover_Full.tex",
  titles = c("Mechanized", "Manual")
)
