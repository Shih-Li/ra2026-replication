# 13_tableG5_price.R
# Stata Table G-5 (original analysis lines ~1506-1543): mechanized desludging price.

d <- read_source("complete_bl_decider_lasso")
treatments <- c("deposit", "neighbor_public", "split_who_s5", "split_number_s5")
candidates <- unique(c(treatments, controls_all, miss_vars(d)))

specs <- list(
  LS_SO_cluster = list(interest = "neighbor_high", sub = d$subsidy_high == 0,
                       always = c("mechprice_bl", "mechprice_bl_miss", "spillover")),
  SO_cluster = list(interest = "neighbor_high", sub = d$spillover == 1,
                    always = c("mechprice_bl", "mechprice_bl_miss")),
  LS_SO_near = list(interest = "num_nearhigh5h", sub = d$subsidy_high == 0,
                    always = c("mechprice_bl", "mechprice_bl_miss", "spillover")),
  SO_near = list(interest = "num_nearhigh5h", sub = d$spillover == 1,
                 always = c("mechprice_bl", "mechprice_bl_miss"))
)
models <- list(); means <- numeric()
for (nm in names(specs)) {
  s <- specs[[nm]]
  fit <- dsregress_r(d, "mechprice_el", s$interest, always = s$always, candidates = candidates, subset = s$sub)
  models[[nm]] <- fit$model
  means[[nm]] <- mean(fit$data$mechprice_el, na.rm = TRUE)
}

write_models_tex(
  models, result_path("TableG5_ImpactOnPrice.tex", "table"),
  terms = c("neighbor_high", "num_nearhigh5h"),
  labels = c("# high subsidy hhds in cluster", "# high subsidy hhds in nearest 4"),
  titles = c("LS + SO", "SO", "LS + SO", "SO"), means = means, digits = 3
)
