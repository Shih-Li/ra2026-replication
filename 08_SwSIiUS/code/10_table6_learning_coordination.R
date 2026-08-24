# 10_table6_learning_coordination.R
# Stata Table 6 (original analysis lines ~1312-1354): learning and coordination.

d <- read_source("complete_bl_decider_lasso")
outcomes <- c("signup_all", "firstdslg", "since_mechaniz_el", "since_manual_el")
interest <- c("subsidy_high", "split_number_s5", "split_who_s5", "num_sign5", "hownum_sign5", "whonum_sign5")
treatments <- c("deposit", "neighbor_public")
candidates <- unique(c(treatments, controls_all, miss_vars(d)))
sub <- (d$sample_who == 1 | d$sample_how == 1) & d$problem == 0

models <- list(); means <- numeric()
for (y in outcomes) {
  fit <- dsregress_r(d, y, interest, candidates = candidates, subset = sub)
  models[[y]] <- fit$model
  means[[y]] <- mean(
    fit$data[[y]][fit$data$subsidy_high == 0 & fit$data$split_who_s5 == 0 & fit$data$split_number_s5 == 0],
    na.rm = TRUE
  )
  state_put(paste0(y, "_t6_how_se"), coef_se(fit$model, "hownum_sign5")[["se"]])
  state_put(paste0(y, "_t6_who_se"), coef_se(fit$model, "whonum_sign5")[["se"]])
}

write_models_tex(
  models, result_path("Table6_LearningCoordination.tex", "table"),
  terms = interest,
  labels = c("High subsidy", "Public-how-many", "Public-who", "# signed up first 5",
             "# signed up x Public-how-many", "# signed up x Public-who"),
  titles = c("Signed Up", "Subsidized Desludging", "Mechanized", "Manual"), means = means
)
