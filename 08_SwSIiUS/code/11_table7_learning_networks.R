# 11_table7_learning_networks.R
# Stata Table 7 (original analysis lines ~1357-1462): learning/coordination within networks.

d <- read_source("complete_bl_decider_lasso")
outcomes <- c("signup_all", "firstdslg", "since_mechaniz_el", "since_manual_el")
treatments <- c("deposit", "neighbor_public")
controls_no_network <- setdiff(controls_all, controls_network)
candidates <- unique(c(treatments, controls_no_network, miss_vars(d)))
sub <- d$sample_who == 1 & d$problem == 0
networks <- c("testknow", "know", "tea", "leadhealth", "sanitation", "wealthy", "near5inc")

run_nw <- function(nw, save_mean = FALSE) {
  interest <- paste0("whonum_", nw, "_sign5")
  always <- c(
    "subsidy_high", "split_who_s5", "num_sign5", "whonum_sign5",
    paste0("num_", nw, "_first5"), paste0("whonum_", nw, "_first5"), paste0("num_", nw, "_sign5")
  )
  models <- list(); means <- numeric()
  for (y in outcomes) {
    fit <- dsregress_r(d, y, interest, always = always, candidates = candidates, subset = sub)
    models[[y]] <- fit$model
    if (save_mean) {
      means[[y]] <- mean(
        fit$data[[y]][fit$data$subsidy_high == 0 & fit$data$split_who_s5 == 0 & fit$data$split_number_s5 == 0],
        na.rm = TRUE
      )
    }
    yshort <- if (y == "since_mechaniz_el") "since_mech_el" else if (y == "since_manual_el") "since_man_el" else y
    state_put(paste0(yshort, "_t7_", nw, "_se"), coef_se(fit$model, interest)[["se"]])
  }
  list(models = models, means = if (save_mean) means else NULL, interest = interest)
}

# Stata uses wealthy for the stand-alone formatted table, then writes one fragment per network.
main <- run_nw("wealthy")
write_models_tex(
  main$models, result_path("Table7_LearningCoordination_SocialNetworks.tex", "table"),
  terms = main$interest, labels = "Network interaction",
  titles = c("Signed Up", "Used Subs Desl", "Mech Desl", "Manual Desl")
)

for (nw in networks) {
  res <- run_nw(nw)
  write_models_tex(
    res$models, result_path(paste0("Table7_LearningCoordination_SocialNetworks_", nw, ".tex"), "table"),
    terms = res$interest, labels = paste0("Public-who interaction: ", nw),
    titles = c("Signed Up", "Used Subs Desl", "Mech Desl", "Manual Desl")
  )
}

last <- run_nw("disin50", save_mean = TRUE)
write_models_tex(
  last$models, result_path("Table7_LearningCoordination_SocialNetworks_Last.tex", "table"),
  terms = character(), labels = character(),
  titles = c("Signed Up", "Used Subs Desl", "Mech Desl", "Manual Desl"), means = last$means
)
