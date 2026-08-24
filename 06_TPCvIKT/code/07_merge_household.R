# 07_merge_household.R
# Merge the R-generated household modules, aggregate individual information,
# assign treatment groups, merge administrative PAL data, and create:
#   data/intermediate/hh_intermediate_attrit.rds
#   data/intermediate/hh_intermediate.rds

aggregate_individual_to_hh <- function(individual) {
  age_indicators <- c("0_5", "6_12", "0_11", "0_14", "13_18", "19_54_male", "19_54_female", "55_98", "65_98")
  d <- individual |>
    dplyr::mutate(
      m_jefe = dplyr::if_else(male == 1 & jefe == 1, 1, NA_real_),
      jefe_educ_i = dplyr::if_else(jefe == 1, educ, NA_real_),
      jefe_age_i = dplyr::if_else(jefe == 1, age, NA_real_),
      farmer = as.integer(act_t %in% c(1, 2, 13) | (!is.na(act2) & act2 == 7)),
      farm_hh_i = as.integer(farmer > 0),
      jefe_union_i = dplyr::if_else(!is.na(rel_a_jefe) & rel_a_jefe == 2, 1, 0),
      in_union = dplyr::if_else(!is.na(civ_stat) & civ_stat %in% c(1, 5), 1, 0)
    )
  rda_names <- sub("^rda_", "", grep("^rda_", names(d), value = TRUE))
  out <- d |>
    dplyr::group_by(cve_viv, etapa) |>
    dplyr::summarise(
      n_hh = dplyr::n(), ae_hh = sum(ae, na.rm = TRUE), jefe_male = sum(!is.na(m_jefe)),
      jefe_educ = ifelse(any(!is.na(jefe_educ_i)), jefe_educ_i[which(!is.na(jefe_educ_i))[1]], NA_real_),
      jefe_age = ifelse(any(!is.na(jefe_age_i)), jefe_age_i[which(!is.na(jefe_age_i))[1]], NA_real_),
      emax = if (any(!is.na(educ))) max(educ, na.rm = TRUE) else NA_real_,
      n_age5_15_noschool = sum(!is.na(d_age5_15_noschool)), n_age12_15_worked = sum(!is.na(d_age12_15_worked)),
      indig_hh = as.integer(sum(indig, na.rm = TRUE) >= 1), no_insur_hh = if (any(!is.na(no_insur))) max(no_insur, na.rm = TRUE) else NA_real_,
      n_working = sum(worked, na.rm = TRUE), work_hrs = sum(work_h, na.rm = TRUE), farmer_n = sum(farmer, na.rm = TRUE),
      farm_hh = mean(farm_hh_i, na.rm = TRUE), farm_hrs = sum(work_h[farmer == 1], na.rm = TRUE),
      pal_bene_fem = ifelse(any(!is.na(pal_bene_fem)), pal_bene_fem[which(!is.na(pal_bene_fem))[1]], NA_real_),
      cal_24_kids = ifelse(any(!is.na(cal[age < 7])), sum(cal[age < 7], na.rm = TRUE), NA_real_),
      cal_24_moms = ifelse(any(!is.na(cal[age > 12])), sum(cal[age > 12], na.rm = TRUE), NA_real_),
      jefe_union = as.integer(any(jefe_union_i == 1, na.rm = TRUE)), n_in_union = sum(in_union, na.rm = TRUE),
      dplyr::across(dplyr::all_of(paste0("d_age", age_indicators)), ~ sum(!is.na(.x)), .names = "n_{.col}"),
      dplyr::across(dplyr::all_of(paste0("rda_", rda_names)), ~ ifelse(any(!is.na(.x)), sum(.x, na.rm = TRUE), NA_real_), .names = "hh_{.col}"),
      .groups = "drop"
    )
  names(out) <- sub("^n_d_age", "n_age", names(out))
  out$emax_d <- dplyr::case_when(dplyr::between(out$emax, 0, 5) ~ 1, out$emax == 6 ~ 2, dplyr::between(out$emax, 7, 9) ~ 3, dplyr::between(out$emax, 10, 25) ~ 4, TRUE ~ NA_real_)
  out |>
    dplyr::filter(ae_hh > .26) |>
    dplyr::arrange(cve_viv, etapa)
}

merge_household_modules <- function(modules, individual) {
  # Stata starts the household merge from the income tempfile.
  hh <- modules$income
  
  assert_unique_key(hh, c("id_1", "etapa"), "merge_start_income")
  checkpoint(
    "07a_merge_start_income",
    hh,
    key = c("id_1", "etapa"),
    both_waves = TRUE
  )
  
  # Stata:
  # use `income', clear
  # foreach x in location_data transfers vivienda credit exp_food food_extra
  #              exp_nfood food_24hr_hogar inventory {
  #     merge id_1 etapa using ``x''
  #     drop _m
  # }
  module_order <- c(
    "location", "transfers", "vivienda", "credit",
    "food", "food_extra", "nfood", "food24", "inventory"
  )
  
  for (nm in module_order) {
    z <- modules[[nm]]
    
    hh <- stata_merge(
      hh,
      z,
      by = c("id_1", "etapa"),
      update = FALSE,
      replace = FALSE
    )
    
    hh$._merge <- NULL
    
    checkpoint(
      paste0("07_merge_", nm),
      hh,
      key = c("id_1", "etapa"),
      both_waves = TRUE,
      require_unique = FALSE
    )
  }
  
  # State / municipality identifiers.
  hh <- drop_existing(hh, c("entidad", "cve_mun"))
  hh$cve_ent <- suppressWarnings(as.numeric(substr(hh$id_loc, 1, 2)))
  hh$id_mun <- substr(hh$id_loc, 1, 5)
  
  hh <- fill_within(
    hh,
    "id_1",
    intersect(c("nom_ent", "nom_mun", "nom_loc"), names(hh)),
    order = "etapa"
  )
  
  # Month of interview: baseline month is the recoded s0105/mm1 value.
  if (all(c("mm_ent", "b_mm_ent") %in% names(hh))) {
    hh$mm_ent <- as_num(hh$mm_ent)
    idx <- true_idx(as_num(hh$etapa) == 1)
    hh$mm_ent[idx] <- as_num(hh$b_mm_ent)[idx]
  }
  hh <- drop_existing(hh, "b_mm_ent")
  
  # Fill baseline cve_viv using the follow-up household ID. Remaining blanks
  # are genuine attriters and receive id_1, exactly as in Stata.
  hh <- fill_within(
    hh,
    "id_1",
    "cve_viv",
    order = "etapa"
  )
  
  idx_blank <- true_idx(stata_missing(hh$cve_viv))
  if (length(idx_blank)) {
    hh$cve_viv[idx_blank] <- hh$id_1[idx_blank]
  }
  
  # -----------------------------------------------------------------------
  # Stata drop-if diagnostics.  The original script applies three filters
  # here.  Check each one separately so a translation error cannot silently
  # wipe out an entire survey wave and surface much later in the analysis.
  # -----------------------------------------------------------------------
  wave_drop <- function(data, condition, label, extra = NULL) {
    cond <- !is.na(condition) & condition
    et <- as_num(data$etapa)
    
    b1 <- sum(et == 1, na.rm = TRUE)
    b2 <- sum(et == 2, na.rm = TRUE)
    d1 <- sum(cond & et == 1, na.rm = TRUE)
    d2 <- sum(cond & et == 2, na.rm = TRUE)
    a1 <- b1 - d1
    a2 <- b2 - d2
    
    message(
      sprintf(
        "%s: before (e1=%d, e2=%d), flagged (e1=%d, e2=%d), after (e1=%d, e2=%d)",
        label, b1, b2, d1, d2, a1, a2
      )
    )
    
    if (!is.null(extra)) message(extra)
    
    if ((b1 > 0 && a1 == 0) || (b2 > 0 && a2 == 0)) {
      stop(
        label,
        " would delete an entire survey wave. ",
        "Before: etapa1=", b1, ", etapa2=", b2,
        "; flagged: etapa1=", d1, ", etapa2=", d2,
        "; after: etapa1=", a1, ", etapa2=", a2,
        ". This indicates an upstream translation mismatch; no rows were dropped.",
        call. = FALSE
      )
    }
    
    out <- stata_drop_rows(data, condition)
    checkpoint(
      paste0("07_filter_", label),
      out,
      key = c("id_1", "etapa"),
      both_waves = TRUE,
      require_unique = FALSE
    )
    out
  }
  
  # Drop localities flagged in field notes.
  if ("comment_code_ffn" %in% names(hh)) {
    cond_field <- as_num(hh$comment_code_ffn) %in% c(1, 3, 5, 6, 7)
    hh <- wave_drop(hh, cond_field, "field_notes")
  }
  
  # Drop the one control household reporting PAL receipt.
  if (all(c("p_pal", "tapoyo") %in% names(hh))) {
    cond_pal <- as_num(hh$p_pal) == 1 & as_num(hh$tapoyo) == 1
    hh <- wave_drop(hh, cond_pal, "pal_control")
  }
  
  # Drop households with many missing food/non-food items.
  food_ate <- grep("^f...ate$", names(hh), value = TRUE)
  nfood <- grep("^n...exp$", names(hh), value = TRUE)
  
  hh$food_mis <- row_missing(hh, food_ate)
  hh$nfood_mis <- row_missing(hh, nfood)
  
  food_diag <- hh |>
    dplyr::group_by(etapa) |>
    dplyr::summarise(
      N = dplyr::n(),
      food_vars = length(food_ate),
      nfood_vars = length(nfood),
      food_mis_min = min(food_mis, na.rm = TRUE),
      food_mis_med = stats::median(food_mis, na.rm = TRUE),
      food_mis_max = max(food_mis, na.rm = TRUE),
      nfood_mis_min = min(nfood_mis, na.rm = TRUE),
      nfood_mis_med = stats::median(nfood_mis, na.rm = TRUE),
      nfood_mis_max = max(nfood_mis, na.rm = TRUE),
      .groups = "drop"
    )
  
  diag_text <- paste(
    capture.output(print(food_diag, n = Inf)),
    collapse = "\n"
  )
  
  cond_missing <- hh$nfood_mis >= 23 | hh$food_mis >= 34
  hh <- wave_drop(hh, cond_missing, "food_nfood_missingness", diag_text)
  
  hh <- dplyr::select(hh, -nfood_mis, -food_mis)
  
  checkpoint(
    "07b_hh_before_individual",
    hh,
    key = c("id_1", "etapa"),
    both_waves = TRUE,
    require_unique = FALSE
  )
  
  # Aggregate individual data to household-wave level and retain matched
  # household-wave observations, matching:
  # merge cve_viv etapa using `individual_aggregate'
  # keep if _m==3
  ind_agg <- aggregate_individual_to_hh(individual)
  
  assert_unique_key(
    ind_agg,
    c("cve_viv", "etapa"),
    "individual_aggregate"
  )
  
  checkpoint(
    "07c_individual_aggregate",
    ind_agg,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE
  )
  
  hh <- stata_merge(
    hh,
    ind_agg,
    by = c("cve_viv", "etapa"),
    update = FALSE,
    replace = FALSE
  )
  hh <- hh[which(hh$._merge == 3L), , drop = FALSE]
  hh$._merge <- NULL
  
  checkpoint(
    "07d_hh_after_individual",
    hh,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE,
    require_unique = FALSE
  )
  
  hh$d_age0_5 <- as.integer(hh$n_age0_5 > 0)
  hh$d_age6_12 <- as.integer(hh$n_age6_12 > 0)
  hh$ae_hh2 <- hh$ae_hh^2
  hh$n_hh2 <- hh$n_hh^2
  hh$jefe_age2 <- hh$jefe_age^2
  
  # Treatment group.
  hh <- fill_within(
    hh,
    "id_1",
    "tapoyo",
    order = "etapa"
  )
  
  hh <- hh |>
    dplyr::group_by(id_loc) |>
    dplyr::mutate(
      tapoyo_loc = if (all(is.na(tapoyo))) NA_real_ else mean(tapoyo, na.rm = TRUE),
      tapoyo = dplyr::if_else(is.na(tapoyo), tapoyo_loc, tapoyo)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-tapoyo_loc) |>
    dplyr::rename(o_group = tapoyo) |>
    dplyr::mutate(
      group = dplyr::case_when(
        o_group == 1 ~ 1,
        o_group %in% c(2, 3) ~ 2,
        o_group == 4 ~ 3,
        TRUE ~ NA_real_
      )
    )
  
  # Administrative treatment data. These are plain Stata merges, not update
  # merges; p_pal_m is supplied by the using file.
  admin_t <- read_dta_safe(
    raw_support("p_pal_matched_treatment.dta")
  )
  
  hh <- stata_merge(
    hh,
    admin_t,
    by = "cve_viv",
    update = FALSE,
    replace = FALSE
  )
  
  if ("p_pal_m" %in% names(hh)) {
    names(hh)[names(hh) == "p_pal_m"] <- "p_pal_a"
  }
  hh$._merge <- NULL
  
  admin_c <- read_dta_safe(
    raw_support("p_pal_matched_control.dta")
  )
  
  hh <- stata_merge(
    hh,
    admin_c,
    by = "cve_viv",
    update = FALSE,
    replace = FALSE
  )
  
  # Stata drops control-admin observations not in the household master.
  hh <- hh[which(hh$._merge != 2L), , drop = FALSE]
  hh$._merge <- NULL
  
  if (all(c("p_pal_a", "p_pal_m", "group") %in% names(hh))) {
    idx <- true_idx(as_num(hh$group) == 1)
    hh$p_pal_a <- as_num(hh$p_pal_a)
    hh$p_pal_a[idx] <- as_num(hh$p_pal_m)[idx]
  }
  
  checkpoint(
    "07e_hh_intermediate_attrit",
    hh,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE,
    require_unique = FALSE
  )
  
  hh
}

build_household_intermediates <- function(
    modules,
    individual,
    save = TRUE
) {
  hh_attrit <- merge_household_modules(
    modules,
    individual
  )
  
  if (save) {
    save_intermediate(
      hh_attrit,
      "hh_intermediate_attrit"
    )
  }
  
  # Stata:
  # duplicates tag cve_viv, gen(t)
  # drop if t==0
  #
  # Retain only households represented in more than one wave.
  counts <- table(hh_attrit$cve_viv)
  keep_ids <- names(counts[counts > 1L])
  
  hh_panel <- hh_attrit[
    hh_attrit$cve_viv %in% keep_ids,
    ,
    drop = FALSE
  ]
  
  checkpoint(
    "07f_hh_intermediate",
    hh_panel,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE,
    require_unique = FALSE
  )
  
  if (save) {
    save_intermediate(
      hh_panel,
      "hh_intermediate"
    )
  }
  
  list(
    attrit = hh_attrit,
    panel = hh_panel
  )
}
