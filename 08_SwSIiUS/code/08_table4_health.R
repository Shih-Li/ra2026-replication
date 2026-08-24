# 08_table4_health.R
# Stata Table 4 (original analysis lines ~1126-1210): health outcomes.

d <- read_source("complete_bl_decider_lasso")
treatments <- c("deposit", "neighbor_public", "split_who_s5", "split_number_s5")
controls_no_health <- setdiff(controls_all, controls_health)
candidates <- unique(c(controls_no_health, treatments, miss_vars(d)))

models <- list(); means <- numeric()
for (root in c("diarrhea", "cough")) {
  specs <- list(
    all_num = list(y = paste0(root, "_all_el"), interest = c("neighbor_high", "subsidy_high"),
                   always = c("diarrhea_all", "cough_all", "num_members_el", "spillover"), key = "an"),
    all_share = list(y = paste0(root, "_share_el"), interest = c("neighbor_high", "subsidy_high"),
                     always = c("diarrhea_share", "cough_share", "spillover"), key = "as"),
    near_num = list(y = paste0(root, "_all_el"), interest = c("num_nearhigh5h", "subsidy_high"),
                    always = c("diarrhea_all", "cough_all", "num_members_el", "spillover"), key = "nn"),
    near_share = list(y = paste0(root, "_share_el"), interest = c("num_nearhigh5h", "subsidy_high"),
                      always = c("diarrhea_share", "cough_share", "spillover"), key = "ns")
  )
  for (nm in names(specs)) {
    s <- specs[[nm]]
    fit <- dsregress_r(d, s$y, s$interest, always = s$always, candidates = candidates)
    model_name <- paste(root, nm, sep = "_")
    models[[model_name]] <- fit$model
    means[[model_name]] <- mean(fit$data[[s$y]][fit$data$subsidy_high == 0], na.rm = TRUE)
    z <- coef_se(fit$model, s$interest[[1]])
    state_put(paste0(root, "_t4_", s$key, "_se"), z[["se"]])
  }
}

write_models_tex(
  models, result_path("Table4_Health.tex", "table"),
  terms = c("subsidy_high", "neighbor_high", "num_nearhigh5h"),
  labels = c("High subsidy", "# high subsidy hhds in cluster", "# high subsidy hhds in nearest 4"),
  titles = c("Diarrhea N", "Diarrhea share", "Diarrhea near N", "Diarrhea near share",
             "Cough N", "Cough share", "Cough near N", "Cough near share"),
  means = means
)
