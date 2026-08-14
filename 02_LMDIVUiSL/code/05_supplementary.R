# ============================================================
# 05_supplementary.R
# Supplementary information: CONSORT counts, Figures A2-A4,
# and systematic literature-review Table A1.
# ============================================================

individual <- read_clean("individual_level.dta")
community <- read_clean("community_data.dta")
lit <- read_clean("lit_review.dta")

theme_rep <- ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, color = "black"))

# ------------------------------------------------------------
# 1.1 CONSORT diagram counts
# ------------------------------------------------------------
consort <- tibble::tibble(level = character(), metric = character(), value = numeric())
add_consort <- function(level, metric, value) {
  consort <<- dplyr::bind_rows(
    consort,
    tibble::tibble(level = level, metric = metric, value = as.numeric(value))
  )
}

add_consort("1", "communities_total", safe_n_unique(community$community_code))
add_consort("1", "control_villages", sum(community$treatment == 0, na.rm = TRUE))
add_consort("1", "door_to_door_villages", sum(community$treatment == 1, na.rm = TRUE))
add_consort("1", "small_group_villages", sum(community$treatment == 2, na.rm = TRUE))

add_consort("2", "control_census_population", sum(individual$treatment == 0 & individual$in_census == 1, na.rm = TRUE))
add_consort("2", "control_baseline_households", safe_n_unique(individual$hh_id[individual$in_baseline == 1 & individual$treatment == 0]))
add_consort("2", "control_complete_baseline_households", safe_n_unique(individual$hh_id[individual$incomplete_observations == 0 & individual$in_baseline == 1 & individual$treatment == 0]))
add_consort("2", "door_to_door_census_population", sum(individual$treatment == 1 & individual$in_census == 1, na.rm = TRUE))
add_consort("2", "door_to_door_baseline_households", safe_n_unique(individual$hh_id[individual$in_baseline == 1 & individual$treatment == 1]))
add_consort("2", "door_to_door_complete_baseline_households", safe_n_unique(individual$hh_id[individual$incomplete_observations == 0 & individual$in_baseline == 1 & individual$treatment == 1]))
add_consort("2", "small_group_census_population", sum(individual$treatment == 2 & individual$in_census == 1, na.rm = TRUE))
add_consort("2", "small_group_baseline_households", safe_n_unique(individual$hh_id[individual$in_baseline == 1 & individual$treatment == 2]))
add_consort("2", "small_group_complete_baseline_households", safe_n_unique(individual$hh_id[individual$incomplete_observations == 0 & individual$in_baseline == 1 & individual$treatment == 2]))

add_consort("3", "door_to_door_intervention_vaccinated", sum(individual$treatment == 1 & individual$vaccinated_team == 1, na.rm = TRUE))
add_consort("3", "door_to_door_attrited", sum(individual$treatment == 1 & individual$in_baseline == 1 & individual$in_endline == 0, na.rm = TRUE))
add_consort("3", "door_to_door_new_endline", sum(individual$treatment == 1 & individual$in_baseline == 0 & individual$in_endline == 1, na.rm = TRUE))
add_consort("3", "small_group_intervention_vaccinated", sum(individual$treatment == 2 & individual$vaccinated_team == 1, na.rm = TRUE))
add_consort("3", "small_group_attrited", sum(individual$treatment == 2 & individual$in_baseline == 1 & individual$in_endline == 0, na.rm = TRUE))
add_consort("3", "small_group_new_endline", sum(individual$treatment == 2 & individual$in_baseline == 0 & individual$in_endline == 1, na.rm = TRUE))

add_consort("4", "door_to_door_endline_households", safe_n_unique(individual$hh_id[individual$treatment == 1 & individual$in_endline == 1]))
add_consort("4", "door_to_door_complete_endline_households", safe_n_unique(individual$hh_id[individual$treatment == 1 & individual$incomplete_observations == 0 & individual$in_endline == 1]))
add_consort("4", "small_group_endline_households", safe_n_unique(individual$hh_id[individual$treatment == 2 & individual$in_endline == 1]))
add_consort("4", "small_group_complete_endline_households", safe_n_unique(individual$hh_id[individual$treatment == 2 & individual$incomplete_observations == 0 & individual$in_endline == 1]))

utils::write.csv(consort, file.path(OUT_DIAG, "consort_counts.csv"), row.names = FALSE)

# ------------------------------------------------------------
# SI Figure A2: variation in endline vaccination rate
# ------------------------------------------------------------
a2 <- individual |>
  dplyr::filter(above18 == 1, in_census == 1) |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_endline = safe_mean(vaccinated_endline),
    treatment = max(treatment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(treatment %in% c(1, 2))

p_a2 <- ggplot2::ggplot(a2, ggplot2::aes(x = vaccinated_endline)) +
  ggplot2::geom_histogram(binwidth = 0.05, boundary = 0, fill = "grey70", color = "black") +
  ggplot2::scale_x_continuous(breaks = seq(0, 0.75, 0.05)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 15, 5)) +
  ggplot2::labs(x = "Share of People Vaccinated", y = "Frequency") +
  theme_rep
save_plot(p_a2, file.path(OUT_APP_FIG, "comm_vax_rate_dist"), width = 8, height = 5)

# ------------------------------------------------------------
# SI Figure A3: variation in number vaccinated in each community
# ------------------------------------------------------------
a3 <- individual |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_team = sum(vaccinated_team, na.rm = TRUE),
    treatment = max(treatment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(treatment %in% c(1, 2))

p_a3 <- ggplot2::ggplot(a3, ggplot2::aes(x = vaccinated_team)) +
  ggplot2::geom_histogram(binwidth = 10, boundary = 0, fill = "grey70", color = "black") +
  ggplot2::scale_x_continuous(breaks = seq(0, 150, 10)) +
  ggplot2::labs(x = "Number of Vaccines Administered in Community", y = "Frequency") +
  theme_rep
save_plot(p_a3, file.path(OUT_APP_FIG, "comm_vax_count_dist"), width = 8, height = 5)

# ------------------------------------------------------------
# SI Figure A4: vaccination count by vaccination team
# ------------------------------------------------------------
a4 <- individual |>
  dplyr::group_by(socialmob, community_code) |>
  dplyr::summarise(vaccinated_team = sum(vaccinated_team, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(team = factor(socialmob))

a4_n <- a4 |>
  dplyr::group_by(team) |>
  dplyr::summarise(n = sum(!is.na(vaccinated_team)), .groups = "drop")

p_a4 <- ggplot2::ggplot(a4, ggplot2::aes(x = team, y = vaccinated_team)) +
  ggplot2::geom_boxplot(width = 0.45, fill = "maroon", color = "black", outlier.shape = 1) +
  ggplot2::geom_text(
    data = a4_n,
    ggplot2::aes(x = team, y = -2, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  ggplot2::labs(x = "Vaccination Team", y = "Number of Vaccines Administered in Community") +
  theme_rep +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
save_plot(p_a4, file.path(OUT_APP_FIG, "vac_team"), width = 8, height = 5)

# ------------------------------------------------------------
# SI Table A1: systematic literature review, split into 16 pages
# ------------------------------------------------------------
lit_table <- lit |>
  dplyr::filter(author1 != "THIS STUDY") |>
  dplyr::transmute(
    `Intervention Type` = as.character(haven::as_factor(classification, levels = "labels")),
    country = country,
    vaccine = stringr::str_replace_all(as.character(vaccine), "≥", "\\\\$\\\\geq\\\\$"),
    author1 = author1,
    publicationyear = publicationyear,
    effectsize = effectsize
  ) |>
  dplyr::arrange(`Intervention Type`, publicationyear, author1)

for (i in seq_len(16L)) {
  start <- (i - 1L) * 15L + 1L
  end <- min(i * 15L, nrow(lit_table))
  part <- if (start <= nrow(lit_table)) lit_table[start:end, , drop = FALSE] else lit_table[0, , drop = FALSE]
  write_table(
    part,
    file.path(OUT_APP_TABLE, paste0("effect_size_table_", i)),
    escape = FALSE
  )
}

writexl::write_xlsx(lit_table, file.path(OUT_APP_TABLE, "lit_review_table.xlsx"))
utils::write.csv(lit_table, file.path(OUT_APP_TABLE, "lit_review_table.csv"), row.names = FALSE, na = "")

message("  Supplementary outputs written to output/figures/appendix/, output/tables/appendix/, and output/diagnostics/")
