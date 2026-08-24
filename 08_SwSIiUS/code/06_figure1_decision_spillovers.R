# 06_figure1_decision_spillovers.R
# Stata Figure 1 (original analysis lines ~737-1019): decision spillovers.

d <- read_source("complete_bl_decider_lasso")
treatments <- c("deposit", "neighbor_public", "split_who_s5", "split_number_s5")
candidates <- unique(c(controls_all, treatments, miss_vars(d)))

build_panel <- function(outcome) {
  specs <- list(
    `High Subsidy Household` = d$subsidy_high == 1 & d$treated == 1,
    `Low Subsidy Household`  = d$subsidy_high == 0 & d$treated == 1,
    `Spillover Household`    = d$treated == 0
  )
  rows <- list()
  for (nm in names(specs)) {
    fit <- dsregress_r(
      d, outcome, c("neighbor_highsub_medium", "neighbor_highsub_high"),
      candidates = candidates, subset = specs[[nm]]
    )
    es <- fit$data
    base <- es[[outcome]][es$neighbor_highsub_low == 1]
    base_mean <- mean(base, na.rm = TRUE)
    zmed <- coef_se(fit$model, "neighbor_highsub_medium")
    zhigh <- coef_se(fit$model, "neighbor_highsub_high")
    rows[[nm]] <- data.frame(
      household = nm,
      bin = factor(c("Low (1-3)", "Medium (4-6)", "High (7-9)"),
                   levels = c("Low (1-3)", "Medium (4-6)", "High (7-9)")),
      estimate = c(base_mean, base_mean + zmed[["estimate"]], base_mean + zhigh[["estimate"]]),
      low = c(NA, base_mean + zmed[["estimate"]] - 1.96 * zmed[["se"]],
              base_mean + zhigh[["estimate"]] - 1.96 * zhigh[["se"]]),
      high = c(NA, base_mean + zmed[["estimate"]] + 1.96 * zmed[["se"]],
               base_mean + zhigh[["estimate"]] + 1.96 * zhigh[["se"]]),
      baseline = base_mean
    )
  }
  dplyr::bind_rows(rows)
}

save_plot <- function(outcome, title, filename, ylim) {
  pdat <- build_panel(outcome)
  hdat <- dplyr::distinct(pdat, household, baseline)
  p <- ggplot2::ggplot(pdat, ggplot2::aes(x = bin, y = estimate)) +
    ggplot2::geom_hline(data = hdat, ggplot2::aes(yintercept = baseline), linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = high), width = 0.12, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", estimate)), vjust = -0.8, size = 3) +
    ggplot2::facet_wrap(~ household, nrow = 1) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(
      x = "# of high subsidy households in cluster", y = "Share of All HHs", title = title
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(result_path(filename, "figure"), p, width = 10.5, height = 4.5)
}

save_plot(
  "since_manual_el", "Used manual desludging between baseline and endline",
  "Figure1_DecisionSpillovers_Manual.pdf", c(0.20, 0.55)
)
save_plot(
  "since_mechaniz_el", "Used mechanized desludging between baseline and endline",
  "Figure1_DecisionSpillovers_Mechanized.pdf", c(0.15, 0.50)
)
