# ============================================================
# 02_intext_calculations.R
# Translation of the "In-Text Calculations" section
# ============================================================

message("  Reading source data for in-text calculations")
individual <- read_clean("individual_level.dta")
lit <- read_clean("lit_review.dta")

metrics <- tibble::tibble(section = character(), metric = character(), value = numeric())
distributions <- list()

add_metric <- function(section, metric, value) {
  metrics <<- dplyr::bind_rows(
    metrics,
    tibble::tibble(section = section, metric = metric, value = as.numeric(value))
  )
}

add_distribution <- function(name, data, var) {
  require_vars(data, var, name)
  tab <- data |>
    dplyr::count(.data[[var]], name = "n", .drop = FALSE) |>
    dplyr::mutate(share = n / sum(n), table = name, variable = var) |>
    dplyr::relocate(table, variable)
  distributions[[length(distributions) + 1L]] <<- tab
}

# Average count vaccinated within treatment communities, baseline vs intervention period.
tmp <- individual |>
  dplyr::filter(any_treat == 1) |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_baseline = sum(vaccinated_baseline, na.rm = TRUE),
    vaccinated_endline = sum(vaccinated_endline, na.rm = TRUE),
    .groups = "drop"
  )
add_metric("vaccination_count", "mean_vaccinated_baseline_treatment_community", safe_mean(tmp$vaccinated_baseline))
add_metric("vaccination_count", "sd_vaccinated_baseline_treatment_community", safe_sd(tmp$vaccinated_baseline))
add_metric("vaccination_count", "mean_vaccinated_endline_treatment_community", safe_mean(tmp$vaccinated_endline))
add_metric("vaccination_count", "sd_vaccinated_endline_treatment_community", safe_sd(tmp$vaccinated_endline))

# Cost per person vaccinated: source do-file hard-codes total program cost 156,023.5 USD.
total_team_vaccinations <- safe_sum(individual$vaccinated_team)
add_metric("cost", "team_vaccinations_total", total_team_vaccinations)
add_metric("cost", "cost_per_person_vaccinated_usd", 156023.5 / total_team_vaccinations)

# Systematic-review counts.
lit_other <- lit |> dplyr::filter(author != "THIS STUDY")
add_metric("literature_review", "unique_studies_excluding_this_study", safe_n_unique(lit_other$study))
null <- lit_other |> dplyr::mutate(is_null = effectsize == 0)
add_metric("literature_review", "share_interventions_null_effect", safe_mean(as.numeric(null$is_null)))
add_metric("literature_review", "count_effectsize_below_26_1", sum(lit_other$effectsize < 26.1, na.rm = TRUE))
add_metric("literature_review", "count_nonmissing_effectsize", sum(!is.na(lit_other$effectsize)))
add_metric(
  "literature_review",
  "share_effectsize_below_26_1",
  sum(lit_other$effectsize < 26.1, na.rm = TRUE) / sum(!is.na(lit_other$effectsize))
)

# Village census size.
census <- individual |> dplyr::filter(in_census == 1)
community_size <- census |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    number_of_people = if (all(is.na(villsize))) NA_real_ else max(villsize, na.rm = TRUE),
    number_of_households = if (all(is.na(hhs_in_community))) NA_real_ else max(hhs_in_community, na.rm = TRUE),
    .groups = "drop"
  )
add_metric("census", "mean_people_per_community", safe_mean(community_size$number_of_people))
add_metric("census", "total_people_enumerated", safe_sum(community_size$number_of_people))
add_metric("census", "mean_households_per_community", safe_mean(community_size$number_of_households))
add_metric("census", "sd_households_per_community", safe_sd(community_size$number_of_households))

# Census demographic tabulations. The source reports Stata tabulations rather than
# assuming which numeric value corresponds to a prose category, so retain full distributions.
add_metric("census_demographics", "mean_age", safe_mean(census$age))
for (v in c("hh_gender", "hh_size", "anyschooling", "farmer")) {
  add_distribution(paste0("census_", v), census, v)
}

# Baseline/endline vaccination rates among adults enumerated in the census.
adults <- census |> dplyr::filter(above18 == 1)
add_metric("adult_vaccination", "baseline_control_mean", safe_mean(adults$vaccinated_baseline[adults$any_treat == 0]))
add_metric("adult_vaccination", "baseline_treatment_mean", safe_mean(adults$vaccinated_baseline[adults$any_treat == 1]))
add_metric("adult_vaccination", "endline_treatment_mean", safe_mean(adults$vaccinated_endline[adults$any_treat == 1]))

tt <- stats::t.test(vaccinated_baseline ~ any_treat, data = adults, var.equal = TRUE)
add_metric("adult_vaccination", "baseline_ttest_p_value", tt$p.value)

baseline_fe <- fit_fe_model(
  adults,
  outcome = "vaccinated_baseline",
  rhs = "any_treat",
  fe = "grpID",
  cluster = "community_code"
)
add_metric("adult_vaccination", "baseline_fe_clustered_p_value", coef_stat(baseline_fe, "any_treat", "p_value"))

# Treatment communities where no vaccination was administered.
attempt <- individual |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    any_treat = dplyr::first(any_treat[!is.na(any_treat)]),
    intervention_attempted = if (all(is.na(vaccinated_team))) NA_real_ else max(vaccinated_team, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(any_treat == 1)
add_metric("implementation", "treatment_communities_zero_team_vaccination", sum(attempt$intervention_attempted == 0, na.rm = TRUE))
add_metric("implementation", "treatment_communities_positive_team_vaccination", sum(attempt$intervention_attempted > 0, na.rm = TRUE))

# Treatment villages with >=50% of enumerated adults vaccinated by the team.
tmp <- individual |>
  dplyr::filter(any_treat == 1, above18 == 1, in_census == 1) |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_team = sum(vaccinated_team, na.rm = TRUE),
    adults_in_census = sum(!is.na(any_treat)),
    .groups = "drop"
  ) |>
  dplyr::mutate(pct_vaccinated = 100 * vaccinated_team / adults_in_census)
add_metric("implementation", "communities_at_least_50pct_adults_vaccinated", sum(tmp$pct_vaccinated >= 50, na.rm = TRUE))

# Baseline count vaccinated among people aged 12+.
tmp <- individual |>
  dplyr::filter(age >= 12, !is.na(age)) |>
  dplyr::group_by(community_code) |>
  dplyr::summarise(
    vaccinated_baseline = sum(vaccinated_baseline, na.rm = TRUE),
    any_treat = max(any_treat, na.rm = TRUE),
    .groups = "drop"
  )
add_metric("baseline_count", "control_mean", safe_mean(tmp$vaccinated_baseline[tmp$any_treat == 0]))
add_metric("baseline_count", "treatment_mean", safe_mean(tmp$vaccinated_baseline[tmp$any_treat == 1]))
tt <- stats::t.test(vaccinated_baseline ~ any_treat, data = tmp, var.equal = TRUE)
add_metric("baseline_count", "ttest_p_value", tt$p.value)

# Provenance of vaccinated people in treatment villages.
prov <- individual |>
  dplyr::filter(any_treat == 1, vaccinated_team == 1) |>
  dplyr::mutate(
    vax_provenance = dplyr::case_when(
      in_census == 1 ~ "From community census",
      in_census == 0 & diff_comm == 1 ~ "Different community",
      in_census == 0 & (is.na(diff_comm) | diff_comm != 1) ~ "Other",
      TRUE ~ NA_character_
    )
  )
if (any(is.na(prov$vax_provenance))) stop("Unclassified vaccination provenance records found")
distributions[[length(distributions) + 1L]] <- prov |>
  dplyr::filter(vax_provenance != "From community census") |>
  dplyr::count(vax_provenance, name = "n") |>
  dplyr::mutate(
    share = n / sum(n),
    table = "vaccination_provenance_outside_census",
    variable = "vax_provenance"
  ) |>
  dplyr::relocate(table, variable)

# Vaccine type distribution.
add_distribution("vaccine_type", individual, "vaccine_type")

# Meeting attendance at household level and vaccination by attendance.
treat_census <- individual |> dplyr::filter(any_treat == 1, in_census == 1)
hh_attend <- treat_census |>
  dplyr::group_by(hh_id) |>
  dplyr::summarise(
    attended = if (all(is.na(attended))) NA_real_ else max(attended, na.rm = TRUE),
    .groups = "drop"
  )
add_distribution("household_meeting_attendance", hh_attend, "attended")

distributions[[length(distributions) + 1L]] <- treat_census |>
  dplyr::filter(!is.na(attended), !is.na(vaccinated_team)) |>
  dplyr::count(attended, vaccinated_team, name = "n") |>
  dplyr::group_by(attended) |>
  dplyr::mutate(share_within_attendance = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(table = "vaccinated_by_meeting_attendance", variable = "attended_x_vaccinated_team") |>
  dplyr::relocate(table, variable)

# Literature review cost reporting.
lit_cost <- lit |>
  dplyr::mutate(
    cost_stated = !is.na(cost_effect) | costeffectiveness == "same as other arms of study"
  )
add_metric(
  "literature_cost",
  "share_interventions_with_cost_stated_excluding_this_study",
  safe_mean(as.numeric(lit_cost$cost_stated[lit_cost$author != "THIS STUDY"]))
)
add_metric(
  "literature_cost",
  "unique_studies_cost_same_as_other_arms",
  safe_n_unique(lit_cost$study[lit_cost$costeffectiveness == "same as other arms of study"])
)

# Source do-file changes one extreme observation to 600 before reporting mean/SD.
lit_cost$cost_effect[lit_cost$study == 96 & lit_cost$order == 229] <- 600
cost_other <- lit_cost$cost_effect[lit_cost$author != "THIS STUDY"]
add_metric("literature_cost", "mean_cost_effect_topcoded_source_rule", safe_mean(cost_other))
add_metric("literature_cost", "sd_cost_effect_topcoded_source_rule", safe_sd(cost_other))

# Remote structures omitted by mobilizers.
remote <- individual |>
  dplyr::filter(periphery == 1, dtd != 1, age >= 12, !is.na(age))
add_metric("implementation", "remote_structures_excluded", safe_n_unique(remote$structure_id))
add_metric("implementation", "people_age12plus_in_remote_structures", nrow(remote))

utils::write.csv(metrics, file.path(OUT_DIAG, "in_text_calculations.csv"), row.names = FALSE, na = "")
if (length(distributions)) {
  utils::write.csv(
    dplyr::bind_rows(distributions),
    file.path(OUT_DIAG, "in_text_distributions.csv"),
    row.names = FALSE,
    na = ""
  )
}

message("  Wrote in-text calculations to output/diagnostics/")
