# ==============================================================================
# File: 9_StMEN/code/11_spatial_concentration.R
#
# Purpose:
#   Produce a 1 x 2 spatial figure for the Paper 9 MIS application.
#
#   Panel A:
#     Number of observations in the first sign-reversing MIS by district.
#
#   Panel B:
#     District shares in the full estimation sample and in the first
#     sign-reversing MIS.
#
# Data sources:
#   1. Paper 9 processed village-month data:
#        9_StMEN/data/processed/Tablet_VillageXMonth_Costs.csv
#
#   2. Paper 9 MIS influential-observation output:
#        audit/output/09_StMEN/audit_influential_ids.csv
#
#   3. DataMeet Census 2011 district boundaries:
#        9_StMEN/data/source/raw/datameet_districts_2011/2011_Dist.shp
#
# Main outputs:
#   audit/output/09_StMEN/
#     fig_spatial_concentration.pdf
#     district_spatial_summary.csv
#
# Interpretation:
#   The figure is deliberately descriptive. It does not define a new
#   concentration or enrichment statistic.
#
#   Panel A shows where the 214 observations in the first sign-reversing
#   MIS are located across the seven study districts.
#
#   Panel B directly compares each district's share of the full estimation
#   sample with its share of the 214-observation sign-reversing MIS.
#
# Notes:
#   - The MIS audit assigns source_row_id before any sample restriction.
#     This script reconstructs that identifier in exactly the same way.
#
#   - In the current replication data, the paper restrictions
#
#         seedsrisk == 1
#         first_implementation == 1
#
#     produce the validated N = 7,370 estimation sample used by the audit.
#
#   - The first zero crossing occurs at k = 214 in the "Decrease" direction.
#
#   - Census 2011 district boundaries are used because they are compatible
#     with the district definitions retained in the study data.
# ==============================================================================


# ==============================================================================
# 0. General options
# ==============================================================================

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ==============================================================================
# 1. Required packages
# ==============================================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "sf",
  "patchwork"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install the following required package(s) first: ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(patchwork)
})


# ==============================================================================
# 2. Locate repository root
# ==============================================================================

find_repo_root <- function() {
  
  current <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
  
  for (i in 0:6) {
    
    candidate <- current
    
    if (
      dir.exists(file.path(candidate, "9_StMEN")) &&
      dir.exists(file.path(candidate, "audit"))
    ) {
      return(candidate)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      break
    }
    
    current <- parent
  }
  
  stop(
    paste0(
      "Could not locate the ra2026-replication repository root.\n",
      "Run this script from the repository root, from 9_StMEN/, ",
      "or from a subdirectory of the repository."
    ),
    call. = FALSE
  )
}


REPO_ROOT <- find_repo_root()

PAPER_ROOT <- file.path(
  REPO_ROOT,
  "9_StMEN"
)

AUDIT_OUTPUT_DIR <- file.path(
  REPO_ROOT,
  "audit",
  "output",
  "09_StMEN"
)


# ==============================================================================
# 3. Input and output paths
# ==============================================================================

DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "processed",
  "Tablet_VillageXMonth_Costs.csv"
)

MIS_IDS_FILE <- file.path(
  AUDIT_OUTPUT_DIR,
  "audit_influential_ids.csv"
)

AUDIT_SUMMARY_FILE <- file.path(
  AUDIT_OUTPUT_DIR,
  "audit_summary.csv"
)

AUDIT_BASELINE_FILE <- file.path(
  AUDIT_OUTPUT_DIR,
  "audit_baseline.csv"
)

SHAPEFILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "raw",
  "datameet_districts_2011",
  "2011_Dist.shp"
)

FIGURE_FILE <- file.path(
  AUDIT_OUTPUT_DIR,
  "fig_spatial_concentration.pdf"
)

SUMMARY_FILE <- file.path(
  AUDIT_OUTPUT_DIR,
  "district_spatial_summary.csv"
)


required_files <- c(
  DATA_FILE,
  MIS_IDS_FILE,
  AUDIT_SUMMARY_FILE,
  AUDIT_BASELINE_FILE,
  SHAPEFILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "The following required file(s) are missing:\n\n",
      paste(missing_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

dir.create(
  AUDIT_OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 4. Study constants
# ==============================================================================

STUDY_ID <- "09_StMEN"

EXPECTED_N <- 7370L

CROSS_ZERO_K <- 214L

CROSS_ZERO_DIRECTION <- "Decrease"

STUDY_DISTRICTS <- c(
  "Bhiwani",
  "Jhajjar",
  "Mewat",
  "Palwal",
  "Panipat",
  "Rewari",
  "Sonipat"
)

DISTRICT_ORDER <- STUDY_DISTRICTS


# ==============================================================================
# 5. Read and validate MIS summary
# ==============================================================================

audit_summary <- utils::read.csv(
  AUDIT_SUMMARY_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (nrow(audit_summary) != 1L) {
  stop(
    "Expected exactly one row in audit_summary.csv.",
    call. = FALSE
  )
}

if (
  !"min_k_cross_zero" %in% names(audit_summary) ||
  !"direction_cross_zero" %in% names(audit_summary)
) {
  stop(
    paste0(
      "audit_summary.csv does not contain the expected ",
      "cross-zero variables."
    ),
    call. = FALSE
  )
}

if (
  as.integer(audit_summary$min_k_cross_zero[[1]]) != CROSS_ZERO_K
) {
  stop(
    paste0(
      "Unexpected first zero-crossing k.\n",
      "Expected: ", CROSS_ZERO_K, "\n",
      "Obtained: ", audit_summary$min_k_cross_zero[[1]]
    ),
    call. = FALSE
  )
}

if (
  as.character(audit_summary$direction_cross_zero[[1]]) !=
  CROSS_ZERO_DIRECTION
) {
  stop(
    paste0(
      "Unexpected zero-crossing direction.\n",
      "Expected: ", CROSS_ZERO_DIRECTION, "\n",
      "Obtained: ",
      audit_summary$direction_cross_zero[[1]]
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 6. Reconstruct the validated Paper 9 estimation sample
# ==============================================================================

villagexmonth_all <- utils::read.csv(
  DATA_FILE,
  stringsAsFactors = FALSE,
  check.names = TRUE
)

# The processed Paper 9 CSV contains a leading unnamed index column.
# read.csv(check.names = TRUE) renames it to "X". It is not an analysis
# variable and can be removed without changing the original row order.
if (
  ncol(villagexmonth_all) > 0L &&
  names(villagexmonth_all)[1] == "X"
) {
  villagexmonth_all <- villagexmonth_all[
    ,
    -1,
    drop = FALSE
  ]
}

# dplyr requires every column to have a valid, non-empty name.
if (
  anyNA(names(villagexmonth_all)) ||
  any(names(villagexmonth_all) == "")
) {
  stop(
    "Processed Paper 9 data contain unnamed columns.",
    call. = FALSE
  )
}

if ("source_row_id" %in% names(villagexmonth_all)) {
  stop(
    paste0(
      "`source_row_id` unexpectedly already exists in ",
      "Tablet_VillageXMonth_Costs.csv."
    ),
    call. = FALSE
  )
}

# Match audit/scripts/09_StMEN.R exactly:
# assign the stable row identifier before applying sample restrictions.
villagexmonth_all$source_row_id <- seq_len(
  nrow(villagexmonth_all)
)

required_variables <- c(
  "source_row_id",
  "seedsrisk",
  "first_implementation",
  "id_district",
  "district",
  "id_village_grp",
  "final_village_name",
  "village_population"
)

missing_variables <- setdiff(
  required_variables,
  names(villagexmonth_all)
)

if (length(missing_variables) > 0L) {
  stop(
    paste0(
      "Processed Paper 9 data are missing required variable(s): ",
      paste(missing_variables, collapse = ", ")
    ),
    call. = FALSE
  )
}


analysis_sample <- villagexmonth_all %>%
  filter(
    seedsrisk == 1,
    first_implementation == 1
  ) %>%
  arrange(source_row_id)


# Validate against the stored audit baseline rather than silently accepting
# a changed sample.
audit_baseline <- utils::read.csv(
  AUDIT_BASELINE_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

baseline_n <- if ("n" %in% names(audit_baseline)) {
  as.integer(audit_baseline$n[[1]])
} else if ("N" %in% names(audit_baseline)) {
  as.integer(audit_baseline$N[[1]])
} else {
  NA_integer_
}

if (!is.na(baseline_n) && baseline_n != EXPECTED_N) {
  stop(
    paste0(
      "Stored audit baseline N differs from the expected Paper 9 N.\n",
      "Expected: ", EXPECTED_N, "\n",
      "Stored: ", baseline_n
    ),
    call. = FALSE
  )
}

if (nrow(analysis_sample) != EXPECTED_N) {
  stop(
    paste0(
      "Paper 9 sample reconstruction failed.\n",
      "Expected N = ", EXPECTED_N, "\n",
      "Obtained N = ", nrow(analysis_sample), "\n\n",
      "Do not make the spatial figure until the sample mismatch is resolved."
    ),
    call. = FALSE
  )
}

if (anyNA(analysis_sample$source_row_id)) {
  stop(
    "`source_row_id` contains missing values in the estimation sample.",
    call. = FALSE
  )
}

if (anyDuplicated(analysis_sample$source_row_id)) {
  stop(
    "`source_row_id` is not unique in the estimation sample.",
    call. = FALSE
  )
}


# ==============================================================================
# 7. Extract the first sign-reversing MIS
# ==============================================================================

mis_ids_all <- utils::read.csv(
  MIS_IDS_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_mis_variables <- c(
  "k",
  "direction",
  "observation_id"
)

missing_mis_variables <- setdiff(
  required_mis_variables,
  names(mis_ids_all)
)

if (length(missing_mis_variables) > 0L) {
  stop(
    paste0(
      "audit_influential_ids.csv is missing required variable(s): ",
      paste(missing_mis_variables, collapse = ", ")
    ),
    call. = FALSE
  )
}


sign_reversing_ids <- mis_ids_all %>%
  filter(
    k == CROSS_ZERO_K,
    direction == CROSS_ZERO_DIRECTION
  ) %>%
  distinct(
    observation_id,
    .keep_all = TRUE
  )


if (nrow(sign_reversing_ids) != CROSS_ZERO_K) {
  stop(
    paste0(
      "The first sign-reversing MIS does not contain the expected ",
      CROSS_ZERO_K,
      " unique observations.\n",
      "Obtained: ",
      nrow(sign_reversing_ids)
    ),
    call. = FALSE
  )
}


# Join the saved MIS IDs back to the validated estimation sample.
mis_sample <- sign_reversing_ids %>%
  transmute(
    source_row_id = as.integer(observation_id)
  ) %>%
  left_join(
    analysis_sample %>%
      select(
        source_row_id,
        id_district,
        district,
        id_village_grp,
        final_village_name
      ),
    by = "source_row_id"
  )


if (anyNA(mis_sample$district)) {
  
  bad_ids <- mis_sample$source_row_id[
    is.na(mis_sample$district)
  ]
  
  stop(
    paste0(
      "Some saved MIS observation IDs could not be matched to the ",
      "validated Paper 9 estimation sample.\n",
      "Unmatched source_row_id values: ",
      paste(bad_ids, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (nrow(mis_sample) != CROSS_ZERO_K) {
  stop(
    "MIS join changed the number of sign-reversing observations.",
    call. = FALSE
  )
}


# ==============================================================================
# 8. District-level descriptive summary
# ==============================================================================

full_district <- analysis_sample %>%
  count(
    id_district,
    district,
    name = "n_full"
  ) %>%
  mutate(
    share_full = n_full / sum(n_full)
  )


mis_district <- mis_sample %>%
  count(
    id_district,
    district,
    name = "n_mis"
  ) %>%
  mutate(
    share_mis = n_mis / sum(n_mis)
  )


district_summary <- full_district %>%
  left_join(
    mis_district,
    by = c(
      "id_district",
      "district"
    )
  ) %>%
  mutate(
    n_mis = tidyr::replace_na(n_mis, 0L),
    share_mis = tidyr::replace_na(share_mis, 0)
  ) %>%
  arrange(id_district)


observed_districts <- sort(
  unique(district_summary$district)
)

expected_districts <- sort(
  STUDY_DISTRICTS
)

if (!identical(observed_districts, expected_districts)) {
  stop(
    paste0(
      "Study-district names do not match the expected seven districts.\n\n",
      "Expected:\n",
      paste(expected_districts, collapse = ", "),
      "\n\nObserved:\n",
      paste(observed_districts, collapse = ", ")
    ),
    call. = FALSE
  )
}


if (sum(district_summary$n_full) != EXPECTED_N) {
  stop(
    "District counts do not sum to the full estimation-sample N.",
    call. = FALSE
  )
}

if (sum(district_summary$n_mis) != CROSS_ZERO_K) {
  stop(
    "District MIS counts do not sum to 214.",
    call. = FALSE
  )
}


utils::write.csv(
  district_summary,
  SUMMARY_FILE,
  row.names = FALSE,
  na = ""
)


# ==============================================================================
# 9. Read DataMeet Census 2011 district boundaries
# ==============================================================================

india_districts <- sf::st_read(
  SHAPEFILE,
  quiet = TRUE
)


# DataMeet documentation uses the variables DISTRICT and ST_NM.
# The checks below make any unexpected shapefile schema explicit.
if (!"DISTRICT" %in% names(india_districts)) {
  stop(
    paste0(
      "The shapefile does not contain the expected `DISTRICT` variable.\n",
      "Available fields:\n",
      paste(names(india_districts), collapse = ", ")
    ),
    call. = FALSE
  )
}

if (!"ST_NM" %in% names(india_districts)) {
  stop(
    paste0(
      "The shapefile does not contain the expected `ST_NM` variable.\n",
      "Available fields:\n",
      paste(names(india_districts), collapse = ", ")
    ),
    call. = FALSE
  )
}


haryana_map <- india_districts %>%
  filter(
    trimws(ST_NM) == "Haryana"
  )


if (nrow(haryana_map) == 0L) {
  stop(
    "No Haryana districts were found in the Census 2011 shapefile.",
    call. = FALSE
  )
}


# Standardize district names for the join.
haryana_map <- haryana_map %>%
  mutate(
    district = trimws(DISTRICT)
  )


missing_map_districts <- setdiff(
  STUDY_DISTRICTS,
  haryana_map$district
)

if (length(missing_map_districts) > 0L) {
  stop(
    paste0(
      "The following study districts are absent from the shapefile:\n",
      paste(missing_map_districts, collapse = ", "),
      "\n\nInspect the shapefile district names before changing the join."
    ),
    call. = FALSE
  )
}


# Attach study-district MIS counts.
haryana_map <- haryana_map %>%
  left_join(
    district_summary %>%
      select(
        district,
        n_full,
        share_full,
        n_mis,
        share_mis
      ),
    by = "district"
  ) %>%
  mutate(
    study_district = district %in% STUDY_DISTRICTS
  )


# ==============================================================================
# 10. Label positions for the seven study districts
# ==============================================================================

# Calculate label positions in a projected CRS and transform them back.
# EPSG:32643 = WGS 84 / UTM zone 43N, appropriate for Haryana.

study_map <- haryana_map %>%
  filter(study_district)


study_label_geometry <- study_map %>%
  st_transform(32643) %>%
  st_geometry() %>%
  st_point_on_surface() %>%
  st_transform(st_crs(haryana_map))


label_coordinates <- sf::st_coordinates(
  study_label_geometry
)

study_labels <- study_map %>%
  st_drop_geometry() %>%
  mutate(
    label_x = label_coordinates[, 1],
    label_y = label_coordinates[, 2],
    
    map_label = paste0(
      district,
      "\n",
      n_mis
    ),
    
    label_colour = if_else(
      n_mis >= 40,
      "#FFFFFF",
      "#333333"
    )
  )


# ==============================================================================
# 11. Visual design
# ==============================================================================

COL_MIS <- "#8B6FCB"
COL_MIS_DARK <- "#6849A8"

COL_FULL <- "#7A7A7A"

COL_NONSTUDY <- "#EEEEEE"
COL_BOUNDARY <- "#FFFFFF"
COL_TEXT <- "#333333"
COL_GRID <- "#E6E6E6"


theme_application <- function(base_size = 11) {
  
  theme_minimal(
    base_size = base_size
  ) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      
      plot.tag = element_text(
        face = "bold",
        size = base_size + 1
      ),
      
      axis.title = element_text(
        colour = COL_TEXT
      ),
      
      axis.text = element_text(
        colour = COL_TEXT
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.title = element_text(
        size = base_size - 1
      ),
      
      legend.text = element_text(
        size = base_size - 1
      )
    )
}


# ==============================================================================
# 12. Panel A: district map
# ==============================================================================

# Keep a modest amount of geographic context around the seven study districts.
# The contextual grey lines below are actual Census 2011 district boundaries
# from the DataMeet shapefile. Non-study districts are not filled.

study_bbox <- sf::st_bbox(
  study_map
)

CONTEXT_PAD <- 0.00001

x_pad <- CONTEXT_PAD * (
  study_bbox["xmax"] - study_bbox["xmin"]
)

y_pad <- CONTEXT_PAD * (
  study_bbox["ymax"] - study_bbox["ymin"]
)


panel_a <- ggplot() +
  
  # Nearby Census 2011 district boundaries.
  #
  # Because these boundaries are drawn without fill and the plotting window
  # is tightly cropped around the study districts, only short surrounding
  # boundary segments remain visible.
  geom_sf(
    data = haryana_map,
    fill = NA,
    colour = "#CCCCCC",
    linewidth = 0.32
  ) +
  
  # Seven study districts.
  geom_sf(
    data = study_map,
    aes(
      fill = n_mis,
      geometry = geometry
    ),
    colour = COL_BOUNDARY,
    linewidth = 0.45
  ) +
  
  # District names and MIS counts.
  geom_text(
    data = study_labels,
    aes(
      x = label_x,
      y = label_y,
      label = map_label,
      colour = label_colour
    ),
    size = 2.8,
    lineheight = 0.95,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_colour_identity() +
  
  scale_colour_identity() +
  
  scale_fill_gradient(
    low = "#E7DDF8",
    high = COL_MIS_DARK,
    name = "Sign-reversing MIS\nobservations"
  ) +
  
  coord_sf(
    xlim = c(
      study_bbox["xmin"] - x_pad,
      study_bbox["xmax"] + x_pad
    ),
    ylim = c(
      study_bbox["ymin"] - y_pad,
      study_bbox["ymax"] + y_pad
    ),
    datum = NA,
    expand = FALSE
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_application() +
  
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    
    plot.margin = margin(
      t = 2,
      r = 2,
      b = 2,
      l = 2
    ),
    
    legend.margin = margin(
      t = 2,
      r = 0,
      b = 0,
      l = 0
    ),
    
    legend.key.width = grid::unit(
      1.4,
      "cm"
    )
  )

# ==============================================================================
# 13. Panel B: full-sample share versus MIS share
# ==============================================================================

plot_summary <- district_summary %>%
  mutate(
    district = factor(
      district,
      levels = rev(DISTRICT_ORDER)
    )
  )


panel_b <- ggplot(
  plot_summary,
  aes(
    y = district
  )
) +
  
  # Connect the two observed shares within each district.
  geom_segment(
    aes(
      x = share_full,
      xend = share_mis,
      yend = district
    ),
    linewidth = 0.7,
    colour = "#C8C8C8"
  ) +
  
  # Full estimation sample.
  geom_point(
    aes(
      x = share_full,
      shape = "Full estimation sample"
    ),
    size = 2.8,
    colour = COL_FULL
  ) +
  
  # First sign-reversing MIS.
  geom_point(
    aes(
      x = share_mis,
      shape = "Sign-reversing MIS"
    ),
    size = 3.0,
    colour = COL_MIS_DARK
  ) +
  
  scale_shape_manual(
    name = NULL,
    values = c(
      "Full estimation sample" = 16,
      "Sign-reversing MIS" = 17
    )
  ) +
  
  scale_x_continuous(
    labels = scales::label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  
  labs(
    x = "Share of observations",
    y = NULL
  ) +
  
  theme_application() +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


# ==============================================================================
# 14. Combine 1 x 2 figure
# ==============================================================================

figure_spatial <- panel_a + panel_b +
  
  patchwork::plot_layout(
    nrow = 1,
    widths = c(1.05, 1)
  ) +
  
  patchwork::plot_annotation(
    tag_levels = "A"
  )


# ==============================================================================
# 15. Save publication figure
# ==============================================================================

ggplot2::ggsave(
  filename = FIGURE_FILE,
  plot = figure_spatial,
  width = 10.5,
  height = 5.2,
  units = "in",
  device = grDevices::cairo_pdf
)


# ==============================================================================
# 16. Console report
# ==============================================================================

message("")
message("============================================================")
message("Paper 9 spatial MIS figure complete")
message("============================================================")
message("")

message(
  "Full estimation sample N: ",
  format(
    nrow(analysis_sample),
    big.mark = ","
  )
)

message(
  "First sign-reversing MIS k: ",
  nrow(mis_sample)
)

message(
  "Direction: ",
  CROSS_ZERO_DIRECTION
)

message("")

message(
  "Figure saved to:\n  ",
  FIGURE_FILE
)

message("")

message(
  "District summary saved to:\n  ",
  SUMMARY_FILE
)

message("")

print(
  district_summary %>%
    mutate(
      share_full = scales::percent(
        share_full,
        accuracy = 0.1
      ),
      share_mis = scales::percent(
        share_mis,
        accuracy = 0.1
      )
    )
)

message("")