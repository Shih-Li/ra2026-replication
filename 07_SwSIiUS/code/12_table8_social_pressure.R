# 12_table8_social_pressure.R
# Stata Table 8 (original analysis lines ~1465-1503): social pressure.

d <- read_source("complete_bl_decider_lasso")
outcomes <- c("signup_all", "firstdslg", "since_mechaniz_el", "since_manual_el")
interest <- c("subsidy_high", "neighbor_public", "int_public_high")
treatments <- c("deposit", "split_who_s5", "split_number_s5")
candidates <- unique(c(treatments, controls_all, miss_vars(d)))
sub <- d$treated == 1

models <- list(); means <- numeric()
for (y in outcomes) {
  fit <- dsregress_r(d, y, interest, candidates = candidates, subset = sub)
  models[[y]] <- fit$model
  means[[y]] <- mean(
    fit$data[[y]][fit$data$subsidy_high == 0 & fit$data$neighbor_public == 0 & fit$data$int_public_high == 0],
    na.rm = TRUE
  )
  state_put(paste0(y, "_t8_se"), coef_se(fit$model, "int_public_high")[["se"]])
}

write_models_tex(
  models, result_path("Table8-SocialPressure.tex", "table"),
  terms = interest,
  labels = c("High subsidy", "Public-price cluster", "High subsidy $\\times$ Public-price cluster"),
  titles = c("Signed Up", "Subsidized Desludging", "Mechanized", "Manual"), means = means
)
