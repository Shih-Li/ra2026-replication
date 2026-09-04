# 05_attrition.R
# Translation of 1.1.Attrition.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("balancevars")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

d <- read_panel()
d <- d[d$intensive_sample == 1 & d$round == 0, , drop = FALSE]
vars <- existing_vars(d, balancevars)

res <- purrr::map_dfr(vars, function(y) {
  m <- fit_model(d, y, "intensive_tracking", weight = NULL, cluster = "hhid")
  e <- extract_terms(m, "intensive_tracking")
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  e$control_mean <- mean(d[[y]][d$intensive_tracking == 0], na.rm = TRUE)
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)
  e
})
res$q.value <- sharpened_qvalues(res$p.value)
res <- res[, c("outcome", "outcome_label", "estimate", "std.error", "p.value", "q.value", "control_mean", "n", "r2")]
write_outputs(res, "IntensiveTracking.tex")
