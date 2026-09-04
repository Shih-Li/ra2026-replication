# 09_table5_social_networks.R
# Stata Table 5 (original analysis lines ~1213-1309): spillovers within social networks.

d <- read_source("complete_bl_decider_lasso")
treatments <- c("deposit", "neighbor_public", "split_who_s5", "split_number_s5")
controls_no_network <- setdiff(controls_all, controls_network)
candidates <- unique(c(controls_no_network, treatments, miss_vars(d)))
outcomes <- c("since_mechaniz_el", "since_manual_el")

network_map <- c(
  testknow = "testknow",
  nghbrtea = "nghbrtea",
  nghbrldhlth = "nghbrleadhealth",
  nghbrsani = "nghbrsanitation",
  hhwealthy = "hhwealthy",
  hhnear5 = "hhnear5"
)

run_network <- function(short, actual, save_mean = FALSE) {
  interest <- c("neighbor_high", paste0("num_high_", actual))
  always <- c(paste0("num_all_", actual), "subsidy_high", "spillover")
  models <- list(); means <- numeric()
  for (y in outcomes) {
    fit <- dsregress_r(d, y, interest, always = always, candidates = candidates)
    models[[y]] <- fit$model
    if (save_mean) means[[y]] <- mean(fit$data[[y]][fit$data$subsidy_high == 0], na.rm = TRUE)
    z <- coef_se(fit$model, interest[[2]])
    yshort <- if (y == "since_mechaniz_el") "since_mech_el" else "since_man_el"
    state_put(paste0(yshort, "_t5_", short, "_se"), z[["se"]])
  }
  list(models = models, means = if (save_mean) means else NULL, interest = interest)
}

# Main format table is based on testknow in the Stata script.
main <- run_network("testknow", "testknow")
write_models_tex(
  main$models, result_path("Table5_DecisionSpillover_SocialNetworks.tex", "table"),
  terms = main$interest,
  labels = c("# high subsidy hhds in cluster", "# high subsidy hhds in network"),
  titles = c("Mech Desl", "Manual Desl")
)

for (short in names(network_map)) {
  actual <- unname(network_map[[short]])
  res <- run_network(short, actual)
  write_models_tex(
    res$models,
    result_path(paste0("Table5_DecisionSpillover_SocialNetworks_", short, ".tex"), "table"),
    terms = res$interest,
    labels = c("# high subsidy hhds in cluster", paste0("# high subsidy hhds: ", short)),
    titles = c("Mech Desl", "Manual Desl")
  )
}

last <- run_network("testknow", "testknow", save_mean = TRUE)
write_models_tex(
  last$models, result_path("Table5_DecisionSpillover_SocialNetworks_last.tex", "table"),
  terms = character(), labels = character(), titles = c("Mech Desl", "Manual Desl"), means = last$means
)
