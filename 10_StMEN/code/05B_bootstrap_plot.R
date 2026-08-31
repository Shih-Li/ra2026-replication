################################################################################
#
# PROJECT: Smart Pooling and Pruning
# PURPOSE: Bootstrap plots
# NOTE: Updated for current ggplot2 and arbitrary numbers of bootstrap supports
#
################################################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggnewscale)
library(this.path)

# ------------------------------------------------------------------------------
# Project paths
# ------------------------------------------------------------------------------

script_dir <- dirname(
  normalizePath(this.path::this.path(), winslash = "/", mustWork = TRUE)
)
source(file.path(script_dir, "00_paths.R"))

set.seed(534)

# ------------------------------------------------------------------------------
# Draw bootstrap plots
# ------------------------------------------------------------------------------

for (outcome in c("shot_Measles1", "shots_per_dollar")) {
  
  simulations_file <- file.path(
    path_intermediate_data,
    paste0("Bootstrap_simulations_data_", outcome, ".csv")
  )
  
  best_policies_file <- file.path(
    path_intermediate_data,
    paste0("_best_policies_data_", outcome, ".csv")
  )
  
  if (!file.exists(simulations_file)) {
    stop("Missing bootstrap file: ", simulations_file)
  }
  
  if (!file.exists(best_policies_file)) {
    stop("Missing bootstrap file: ", best_policies_file)
  }
  
  simulations_data <- read.csv(simulations_file) %>%
    mutate(pl_names = sub(",", ",\n", pl_names))
  
  best_policies_data <- read.csv(best_policies_file) %>%
    mutate(
      bootstrapped_best_policy_name =
        sub(",", ",\n", bootstrapped_best_policy_name)
    )
  
  # Recover summary quantities
  initial_best_policy <- best_policies_data %>%
    filter(Is_true == 1) %>%
    pull(bootstrapped_best_policy_name)
  
  nsamples <- nrow(best_policies_data) - 1
  best_policy_accuracy <- unique(best_policies_data$best_policy_accuracy)
  best_policy_accuracy <- best_policy_accuracy[!is.na(best_policy_accuracy)][1]
  
  # ---------------------------------------------------------------------------
  # Dynamic support legend
  #
  # Original code only defined scales for 2, 3, or 4 support categories.
  # Bootstrap realizations can produce more, so construct scales dynamically.
  # ---------------------------------------------------------------------------
  
  support_levels <- sort(unique(as.character(simulations_data$support_category)))
  n_supports <- length(support_levels)
  
  support_cols <- setNames(
    rep("black", n_supports),
    support_levels
  )
  
  nonzero_levels <- support_levels[support_levels != "0"]
  
  if (length(nonzero_levels) > 0) {
    support_cols[nonzero_levels] <- grDevices::hcl.colors(
      length(nonzero_levels),
      palette = "Dark 3"
    )
  }
  
  # Preserve the authors' convention for the data support.
  if ("0" %in% support_levels) {
    support_cols["0"] <- "black"
  }
  
  support_shapes <- setNames(
    ifelse(support_levels == "0", 22, 16),
    support_levels
  )
  
  support_sizes <- setNames(
    rep(5, n_supports),
    support_levels
  )
  
  support_labels <- ifelse(
    support_levels == "0",
    "Data Support",
    paste("Bootstrap Support", support_levels)
  )
  
  # ---------------------------------------------------------------------------
  # Plot
  # ---------------------------------------------------------------------------
  
  total_graph <- ggplot() +
    geom_point(
      data = simulations_data,
      aes(
        x = pl_names,
        y = pl_effects,
        color = factor(support_category),
        shape = factor(support_category),
        size = factor(support_category)
      ),
      alpha = 0.8
    ) +
    scale_color_manual(
      name = "Selected pooled policies",
      values = support_cols,
      breaks = support_levels,
      labels = support_labels,
      guide = guide_legend(order = 1)
    ) +
    scale_size_manual(
      name = "Selected pooled policies",
      values = support_sizes,
      breaks = support_levels,
      labels = support_labels,
      guide = guide_legend(order = 1)
    ) +
    scale_shape_manual(
      name = "Selected pooled policies",
      values = support_shapes,
      breaks = support_levels,
      labels = support_labels,
      guide = guide_legend(order = 1)
    ) +
    ggnewscale::new_scale_color() +
    geom_point(
      data = best_policies_data,
      aes(
        x = bootstrapped_best_policy_name,
        y = bootstrapped_best_coef,
        color = factor(Is_true)
      ),
      size = 1.5
    ) +
    scale_color_manual(
      name = "Selected best policy",
      values = c("0" = "firebrick", "1" = "chartreuse3"),
      labels = c(
        "Bootstrap winner's curse\nadjusted estimate",
        "Data winner's curse\nadjusted estimate"
      ),
      guide = guide_legend(order = 2)
    ) +
    geom_point(
      data = filter(simulations_data, support_category == 0),
      aes(x = pl_names, y = pl_effects),
      color = "black",
      shape = 22,
      size = 7,
      fill = "white"
    ) +
    geom_point(
      data = filter(best_policies_data, Is_true == 1),
      aes(
        x = bootstrapped_best_policy_name,
        y = bootstrapped_best_coef
      ),
      color = "chartreuse3",
      shape = 16,
      size = 3
    ) +
    theme_minimal() +
    theme(
      # Current ggplot2 does not officially support a vector of colors in
      # element_text(), so use a stable single axis-text color.
      axis.text.x = element_text(
        colour = "black",
        size = 12,
        angle = 90
      ),
      axis.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(
        margin = margin(t = 10, unit = "pt")
      ),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(
        size = 12,
        margin = margin(b = 5, t = 5, unit = "pt")
      ),
      plot.title = element_text(size = 16, face = "bold")
    ) +
    ggtitle(
      paste0(
        "Post-LASSO Estimates for Bootstrapped Samples (",
        nsamples,
        " simulations).\nBest Policy Selection Accuracy = ",
        round(best_policy_accuracy, 3)
      )
    ) +
    ylab("Treatment Effects") +
    xlab("Pooled policy name")
  
  output_file <- file.path(
    path_figures,
    paste0(
      "bootstrapping_",
      outcome,
      "_n=",
      nsamples,
      "_WC.pdf"
    )
  )
  
  ggsave(
    output_file,
    plot = total_graph,
    width = 14,
    height = 8
  )
  
  cat("Saved:", output_file, "\n")
}