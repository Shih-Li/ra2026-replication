# 03_checks.R
# Preflight checks and build checkpoints.
#
# This script implements the data-handling rule for this paper:
# start from the raw files actually read by the official Stata workflow,
# regenerate all cleaned/intermediate datasets in R, and fail early when a
# key or survey wave becomes structurally invalid.

required_source_files <- function() {
  c(
    # Household workflow: baseline
    baseline_raw("s0105"),
    baseline_raw("s0405"),
    baseline_raw("s0505"),
    baseline_raw("s0606_basal_nva version_w.dta"),
    baseline_raw("s06e05"),
    baseline_raw("s0705"),
    baseline_raw("s0805"),
    baseline_raw("s0905"),
    baseline_raw("sr2406_hogar"),
    baseline_raw("field notes - Baseline - edit"),
    
    # Individual workflow: baseline
    baseline_raw("s0205"),
    baseline_raw("s0305"),
    baseline_raw("s1005"),
    baseline_raw("s1105_mujeres.dta"),
    baseline_raw("s1105ninios.dta"),
    baseline_raw("sr2406_mujer"),
    baseline_raw("sr2406_ninios"),
    
    # Household workflow: follow-up
    followup_raw("s102_2"),
    followup_raw("s402a_2"),
    followup_raw("s402b_2"),
    followup_raw("s502_2"),
    followup_raw("s602_2.dta"),
    followup_raw("s602e_2.dta"),
    followup_raw("s702_2"),
    followup_raw("s802_2"),
    followup_raw("s902_2"),
    followup_raw("s1302_2"),
    followup_raw("s1402_hogar"),
    followup_raw("field notes - 1st followup - edit"),
    
    # Individual workflow: follow-up
    followup_raw("s102a_2.dta"),
    followup_raw("s202_2.dta"),
    followup_raw("s302_2.dta"),
    followup_raw("s1002_2.dta"),
    followup_raw("s1102_2_mujeres"),
    followup_raw("s1102_2_ninios.dta"),
    followup_raw("s1402_mujeres"),
    followup_raw("s1402_ninios"),
    followup_raw("s1602_2_anemia"),
    followup_raw("s1602_2_glucosa"),
    
    # Supporting raw/source files
    raw_support("inegi_geodata.dta"),
    raw_support("diconsa_dir_2002_2005_pal.dta"),
    raw_support("Base_valornutritivo_edited.csv"),
    raw_support("est_avg_rec_nutrients.csv"),
    raw_support("p_pal_matched_treatment.dta"),
    raw_support("p_pal_matched_control.dta")
  )
}

source_file_exists <- function(path) {
  candidates <- unique(c(
    path,
    paste0(path, ".dta"),
    sub("\\.dta$", "", path, ignore.case = TRUE)
  ))
  any(file.exists(candidates))
}

run_preflight <- function() {
  files <- required_source_files()
  missing <- files[!vapply(files, source_file_exists, logical(1))]
  
  if (length(missing)) {
    stop(
      "Missing REQUIRED RAW INPUT file(s):\n",
      paste0(" - ", missing, collapse = "\n"),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

clear_generated_rds <- function() {
  for (d in c(INTERMEDIATE_DIR, PROCESSED_DIR)) {
    if (!dir.exists(d)) next
    old <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
    if (length(old)) unlink(old)
  }
  
  chk <- file.path(OUTPUT_DIR, "build_checkpoints.csv")
  if (file.exists(chk)) unlink(chk)
  
  invisible(TRUE)
}

BUILD_CHECKPOINTS <- data.frame(
  stage = character(),
  N = integer(),
  etapa1 = integer(),
  etapa2 = integer(),
  id_1_unique = integer(),
  cve_viv_unique = integer(),
  id_loc_unique = integer(),
  duplicate_keys = integer(),
  stringsAsFactors = FALSE
)

count_nonblank_unique <- function(x) {
  if (is.null(x)) return(NA_integer_)
  x <- as.character(x)
  keep <- !stata_missing(x)
  length(unique(x[keep]))
}

checkpoint <- function(stage, df, key = NULL, both_waves = FALSE, require_unique = TRUE) {
  if (!is.data.frame(df)) {
    stop("Checkpoint `", stage, "` did not receive a data frame.", call. = FALSE)
  }
  
  w <- wave_counts(df)
  
  dup_n <- NA_integer_
  if (!is.null(key)) {
    missing_key <- setdiff(key, names(df))
    if (length(missing_key)) {
      stop(
        stage, ": missing checkpoint key(s): ",
        paste(missing_key, collapse = ", "),
        call. = FALSE
      )
    }
    
    dup_n <- df |>
      dplyr::count(dplyr::across(dplyr::all_of(key)), name = ".n") |>
      dplyr::filter(.n > 1) |>
      nrow()
  }
  
  BUILD_CHECKPOINTS <<- rbind(
    BUILD_CHECKPOINTS,
    data.frame(
      stage = stage,
      N = nrow(df),
      etapa1 = unname(w[["1"]]),
      etapa2 = unname(w[["2"]]),
      id_1_unique = if ("id_1" %in% names(df)) count_nonblank_unique(df$id_1) else NA_integer_,
      cve_viv_unique = if ("cve_viv" %in% names(df)) count_nonblank_unique(df$cve_viv) else NA_integer_,
      id_loc_unique = if ("id_loc" %in% names(df)) count_nonblank_unique(df$id_loc) else NA_integer_,
      duplicate_keys = dup_n,
      stringsAsFactors = FALSE
    )
  )
  
  utils::write.csv(
    BUILD_CHECKPOINTS,
    file.path(OUTPUT_DIR, "build_checkpoints.csv"),
    row.names = FALSE
  )
  
  if (both_waves) require_both_waves(df, stage)
  
  if (require_unique && !is.null(key) && !is.na(dup_n) && dup_n > 0L) {
    assert_unique_key(df, key, stage)
  }
  
  invisible(df)
}


validate_analysis_sample <- function(hh, stage = "processed hh") {
  require_both_waves(hh, stage)
  
  required <- c(
    "cve_viv", "id_1", "id_loc", "etapa",
    "group", "diconsa", "mm_ent"
  )
  
  missing <- setdiff(required, names(hh))
  if (length(missing)) {
    stop(
      stage, " is missing required analysis variable(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  n_loc <- count_nonblank_unique(hh$id_loc)
  if (is.na(n_loc) || n_loc < 2L) {
    stop(
      stage, " has only ", n_loc,
      " usable id_loc value(s). Household construction is invalid.",
      call. = FALSE
    )
  }
  
  group_vals <- unique(as_num(hh$group))
  group_vals <- group_vals[!is.na(group_vals)]
  if (length(group_vals) < 2L) {
    stop(
      stage, " has no treatment-group variation.",
      call. = FALSE
    )
  }
  
  mm <- unique(as_num(hh$mm_ent))
  mm <- mm[!is.na(mm)]
  if (!length(mm)) {
    stop(
      stage, " has no non-missing mm_ent values.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}
