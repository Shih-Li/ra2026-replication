# ============================================================
# 03_figures.R
# Main-text Figures 2-7
# ============================================================

individual <- read_clean("individual_level.dta")
lit <- read_clean("lit_review.dta")

theme_rep <- ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = "black"),
    legend.title = ggplot2::element_blank()
  )

# ------------------------------------------------------------
# Figure 2: vaccination rate before/after mobile vaccination program
# ------------------------------------------------------------
fig2_data <- individual |>
  dplyr::filter(above18 == 1, in_census == 1) |>
  dplyr::group_by(any_treat) |>
  dplyr::summarise(
    mean_baseline = safe_mean(vaccinated_baseline),
    mean_endline = safe_mean(vaccinated_endline),
    se_baseline = safe_se(vaccinated_baseline),
    se_endline = safe_se(vaccinated_endline),
    .groups = "drop"
  ) |>
  dplyr::mutate(group = factor(any_treat, levels = c(0, 1), labels = c("Control", "Pooled Treatment")))

p2 <- ggplot2::ggplot(fig2_data, ggplot2::aes(x = group)) +
  ggplot2::geom_col(
    data = dplyr::filter(fig2_data, any_treat == 1),
    ggplot2::aes(y = mean_endline), width = 0.70, fill = "maroon", color = "black"
  ) +
  ggplot2::geom_col(ggplot2::aes(y = mean_baseline), width = 0.70, fill = "grey85", color = "black") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean_baseline - se_baseline, ymax = mean_baseline + se_baseline),
    width = 0.10
  ) +
  ggplot2::geom_errorbar(
    data = dplyr::filter(fig2_data, any_treat == 1),
    ggplot2::aes(ymin = mean_endline - se_endline, ymax = mean_endline + se_endline),
    width = 0.10
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = mean_baseline + 0.015, label = sprintf("%.3f", mean_baseline)),
    size = 3.8
  ) +
  ggplot2::geom_text(
    data = dplyr::filter(fig2_data, any_treat == 1),
    ggplot2::aes(y = mean_endline + 0.015, label = sprintf("%.3f", mean_endline)),
    size = 3.8
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 0.40), breaks = seq(0, 0.40, 0.05)) +
  ggplot2::labs(x = NULL, y = "Proportion Vaccinated") +
  theme_rep +
  ggplot2::theme(legend.position = "none")

save_plot(p2, file.path(OUT_MAIN_FIG, "vacrate_pooled"), width = 7, height = 5)

# ------------------------------------------------------------
# Figure 3: count vaccinated per site before/after program
# ------------------------------------------------------------
base <- individual

community_counts <- list(
  any = base,
  census = dplyr::filter(base, in_census == 1),
  outside = dplyr::filter(base, diff_comm == 1 | in_census == 1)
) |>
  purrr::imap(function(d, nm) {
    d |>
      dplyr::group_by(community_code) |>
      dplyr::summarise(
        vaccinated_endline = sum(vaccinated_endline, na.rm = TRUE),
        vaccinated_baseline = sum(vaccinated_baseline, na.rm = TRUE),
        any_treat = dplyr::first(any_treat[!is.na(any_treat)]),
        .groups = "drop"
      ) |>
      dplyr::mutate(condition = nm)
  }) |>
  dplyr::bind_rows()

fig3_end <- community_counts |>
  dplyr::group_by(any_treat, condition) |>
  dplyr::summarise(
    mean = safe_mean(vaccinated_endline),
    se = safe_se(vaccinated_endline),
    .groups = "drop"
  ) |>
  dplyr::filter(any_treat == 1)

fig3_base <- community_counts |>
  dplyr::filter(condition == "any") |>
  dplyr::group_by(any_treat) |>
  dplyr::summarise(mean = safe_mean(vaccinated_baseline), se = safe_se(vaccinated_baseline), .groups = "drop") |>
  dplyr::mutate(group = factor(any_treat, levels = c(0, 1), labels = c("Control", "Pooled Treatment")))

fig3_end <- fig3_end |>
  dplyr::mutate(
    group = factor("Pooled Treatment", levels = c("Control", "Pooled Treatment")),
    condition = factor(condition, levels = c("any", "outside", "census"))
  )

p3 <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = dplyr::filter(fig3_end, condition == "any"),
    ggplot2::aes(x = group, y = mean), width = 0.72, fill = "maroon", color = "black"
  ) +
  ggplot2::geom_col(
    data = dplyr::filter(fig3_end, condition == "outside"),
    ggplot2::aes(x = group, y = mean), width = 0.56, fill = "grey55", color = "black"
  ) +
  ggplot2::geom_col(
    data = dplyr::filter(fig3_end, condition == "census"),
    ggplot2::aes(x = group, y = mean), width = 0.40, fill = "grey75", color = "black"
  ) +
  ggplot2::geom_col(
    data = fig3_base,
    ggplot2::aes(x = group, y = mean), width = 0.26, fill = "grey90", color = "black"
  ) +
  ggplot2::geom_errorbar(
    data = fig3_end,
    ggplot2::aes(x = group, ymin = mean - se, ymax = mean + se), width = 0.08
  ) +
  ggplot2::geom_errorbar(
    data = fig3_base,
    ggplot2::aes(x = group, ymin = mean - se, ymax = mean + se), width = 0.08
  ) +
  ggplot2::geom_text(
    data = fig3_end,
    ggplot2::aes(x = group, y = mean + 2.2, label = sprintf("%.2f", mean)), size = 3.2
  ) +
  ggplot2::geom_text(
    data = fig3_base,
    ggplot2::aes(x = group, y = mean + 2.2, label = sprintf("%.2f", mean)), size = 3.2
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  ggplot2::labs(x = NULL, y = "Number Vaccinated") +
  theme_rep

save_plot(p3, file.path(OUT_MAIN_FIG, "vaccount_pooled"), width = 7, height = 5)

# ------------------------------------------------------------
# Figure 4: pooled treatment effect on knowledge and attitudes
# ------------------------------------------------------------
fig4_data <- individual |>
  dplyr::filter(incomplete_observations == 0)

fig4_outcomes <- c(KNOWLEDGE_OUTCOMES, TRUST_OUTCOMES)
fig4_models <- setNames(lapply(fig4_outcomes, function(y) {
  fit_fe_model(fig4_data, y, "any_treat", fe = "grpID", cluster = "community_code")
}), fig4_outcomes)

fig4_coef <- purrr::imap_dfr(fig4_models, function(m, y) {
  tibble::tibble(
    outcome = y,
    label = var_label(fig4_data, y),
    estimate = coef_stat(m, "any_treat", "estimate"),
    se = coef_stat(m, "any_treat", "std_error"),
    p = coef_stat(m, "any_treat", "p_value")
  )
}) |>
  dplyr::mutate(
    label = factor(label, levels = rev(unique(label))),
    sig_label = paste0(sprintf("%.3f", estimate), vapply(p, stars, character(1)))
  )

p4 <- ggplot2::ggplot(fig4_coef, ggplot2::aes(x = label, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.20) +
  ggplot2::geom_point(size = 2.3) +
  ggplot2::geom_text(ggplot2::aes(label = sig_label), hjust = -0.15, size = 3.2) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(limits = c(-0.10, 0.30), breaks = seq(-0.10, 0.30, 0.05)) +
  ggplot2::labs(x = NULL, y = "Pooled Treatment Effect") +
  theme_rep

save_plot(p4, file.path(OUT_MAIN_FIG, "knowledge_attitudes"), width = 8, height = 6)

# ------------------------------------------------------------
# Figure 5: pooled treatment effects by respondent characteristics
# ------------------------------------------------------------
fig5_data <- individual |>
  dplyr::filter(above18 == 1, in_census == 1)

subgroups <- list(
  "Full sample" = rep(TRUE, nrow(fig5_data)),
  "Female" = fig5_data$female == 1,
  "Male" = fig5_data$female == 0,
  "Aged 18-24" = fig5_data$Age18_24 == 1,
  "Aged 25-54" = fig5_data$Age25_54 == 1,
  "Aged 55+" = fig5_data$Age55 == 1,
  "HH head any schooling" = fig5_data$anyschooling == 1,
  "HH head no schooling" = fig5_data$anyschooling == 0,
  "HH owns any land" = fig5_data$BSL_owns_land == 1,
  "HH owns no land" = fig5_data$BSL_owns_land == 0,
  "HH reduced portions of food" = fig5_data$BSL_reduced_portions == 1,
  "HH did not reduce portions of food" = fig5_data$BSL_reduced_portions == 0
)

fig5_coef <- purrr::imap_dfr(subgroups, function(idx, label) {
  d <- fig5_data[!is.na(idx) & idx, , drop = FALSE]
  m <- fit_fe_model(d, "vaccinated_endline", "any_treat", fe = "grpID", cluster = "community_code")
  tibble::tibble(
    label = label,
    estimate = coef_stat(m, "any_treat", "estimate"),
    se = coef_stat(m, "any_treat", "std_error"),
    p = coef_stat(m, "any_treat", "p_value")
  )
}) |>
  dplyr::mutate(
    label = factor(label, levels = rev(names(subgroups))),
    sig_label = paste0(sprintf("%.3f", estimate), vapply(p, stars, character(1)))
  )

p5 <- ggplot2::ggplot(fig5_coef, ggplot2::aes(x = label, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.20) +
  ggplot2::geom_point(size = 2.3) +
  ggplot2::geom_text(ggplot2::aes(label = sig_label), hjust = -0.15, size = 3.1) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
  ggplot2::labs(x = NULL, y = "Pooled Treatment Effect") +
  theme_rep

save_plot(p5, file.path(OUT_MAIN_FIG, "respondent_characteristics"), width = 9, height = 7)

# ------------------------------------------------------------
# Figure 6: effect sizes in previous vaccine uptake RCTs
# ------------------------------------------------------------
lit_fig6 <- lit |>
  dplyr::filter(author != "THIS STUDY") |>
  dplyr::mutate(
    classification_label = as.character(haven::as_factor(classification, levels = "labels")),
    classification_label = dplyr::if_else(
      as.numeric(classification) == 5,
      "Healthcare Improv.",
      classification_label
    )
  )

fig6_n <- lit_fig6 |>
  dplyr::group_by(classification_label) |>
  dplyr::summarise(n = sum(!is.na(effectsize)), .groups = "drop")

p6 <- ggplot2::ggplot(lit_fig6, ggplot2::aes(x = classification_label, y = effectsize)) +
  ggplot2::geom_boxplot(width = 0.45, fill = "maroon", color = "black", outlier.shape = 1) +
  ggplot2::geom_text(
    data = fig6_n,
    ggplot2::aes(x = classification_label, y = -0.75, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  ggplot2::coord_cartesian(ylim = c(-1, 3)) +
  ggplot2::labs(x = "Intervention Type", y = "Effect Size (%p)") +
  theme_rep +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))

save_plot(p6, file.path(OUT_MAIN_FIG, "literature_effects"), width = 9, height = 6)

# ------------------------------------------------------------
# Figure 7: cost per person vaccinated compared with other studies
# Note the source Figure 7 continues from Figure 6's lit_review dataset.
# This translation reloads/constructs the required data explicitly.
# ------------------------------------------------------------
lit_fig7 <- lit |>
  dplyr::mutate(
    cost_effect = dplyr::if_else(!is.na(cost_effect) & cost_effect > 2000, 600, cost_effect),
    classification_label = as.character(haven::as_factor(classification, levels = "labels")),
    display_study = dplyr::if_else(id == 244, "THIS STUDY (2022)", as.character(study_label))
  ) |>
  dplyr::filter(!is.na(cost_effect), !is.na(display_study))

p7 <- ggplot2::ggplot(
  lit_fig7,
  ggplot2::aes(x = stats::reorder(display_study, cost_effect), y = cost_effect, fill = classification_label)
) +
  ggplot2::geom_col(color = "black") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", cost_effect)), hjust = -0.08, size = 2.7) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(limits = c(0, 700), breaks = seq(0, 600, 100), expand = ggplot2::expansion(mult = c(0, 0.03))) +
  ggplot2::labs(x = NULL, y = "Cost per vaccine administered in 2000 USD", fill = NULL) +
  theme_rep +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8), legend.position = "bottom")

save_plot(p7, file.path(OUT_MAIN_FIG, "literature_cost"), width = 9, height = 12)

message("  Main-text figures written to output/figures/main/")
