# 08_build_analysis_data.R
# Build analysis-ready household datasets from the two intermediate household
# samples. This corresponds to the repeated post-cleaning blocks in
# analysis_hhattrit_fixed.do.

make_food_categories <- function(hh) {
  rt <- function(prefix, codes) row_total(hh, paste0("f", codes, prefix))
  for (kind in c("exp", "ate", "cal")) {
    hh[[paste0(kind, "_frvg")]] <- rt(kind, c(101:117, 415, 416)); hh[[paste0(kind, "_veg")]] <- rt(kind, c(101:109, 415, 416)); hh[[paste0(kind, "_frt")]] <- rt(kind, 110:117)
    hh[[paste0(kind, "_grn")]] <- rt(kind, 201:212); hh[[paste0(kind, "_corn_fl")]] <- hh[[paste0("f203", kind)]]; hh[[paste0(kind, "_corn_oth")]] <- rt(kind, 201:202); hh[[paste0(kind, "_corn_all")]] <- rt(kind, 201:203)
    hh[[paste0(kind, "_rice")]] <- hh[[paste0("f210", kind)]]; hh[[paste0(kind, "_cookie")]] <- hh[[paste0("f211", kind)]]; hh[[paste0(kind, "_pasta")]] <- hh[[paste0("f208", kind)]]; hh[[paste0(kind, "_cereal")]] <- hh[[paste0("f212", kind)]]; hh[[paste0(kind, "_oth_wht")]] <- rt(kind, c(204:207, 209))
    hh[[paste0(kind, "_pulse")]] <- rt(kind, 213:214); hh[[paste0(kind, "_beans")]] <- hh[[paste0("f213", kind)]]; hh[[paste0(kind, "_lentil")]] <- hh[[paste0("f214", kind)]]
    hh[[paste0(kind, "_meat")]] <- rt(kind, c(301:306, 311, 412)); hh[[paste0(kind, "_fish")]] <- rt(kind, c(304, 305)); hh[[paste0(kind, "_meat_oth")]] <- hh[[paste0(kind, "_meat")]] - hh[[paste0(kind, "_fish")]]
    for (pair in list(chicken = 301, beef_pork = 302, seafood = 304, can_fish = 305, eggs = 306)) hh[[paste0(kind, "_", names(pair))]] <- hh[[paste0("f", pair[[1]], kind)]]
    hh[[paste0(kind, "_dairy")]] <- rt(kind, c(307, 308, 309, 312)); hh[[paste0(kind, "_milk")]] <- rt(kind, c(307, 312)); hh[[paste0(kind, "_milk_lq")]] <- hh[[paste0("f307", kind)]]; hh[[paste0(kind, "_milk_pwd")]] <- hh[[paste0("f312", kind)]]; hh[[paste0(kind, "_yogurt")]] <- hh[[paste0("f308", kind)]]; hh[[paste0(kind, "_cheese")]] <- hh[[paste0("f309", kind)]]; hh[[paste0(kind, "_dairy_oth")]] <- rt(kind, c(308, 309)); hh[[paste0(kind, "_dairy_oth2")]] <- rt(kind, c(307:309))
    hh[[paste0(kind, "_cookfat")]] <- rt(kind, c(310, 406, 410)); hh[[paste0(kind, "_oil")]] <- hh[[paste0("f406", kind)]]; hh[[paste0(kind, "_cookfat_oth")]] <- rt(kind, c(310, 410)); hh[[paste0(kind, "_lard")]] <- hh[[paste0("f310", kind)]]; hh[[paste0(kind, "_mayo")]] <- hh[[paste0("f410", kind)]]
    hh[[paste0(kind, "_othfood")]] <- rt(kind, c(215, 216, 401:405, 407:409, 411, 413, 414)); hh[[paste0(kind, "_oth_starch")]] <- rt(kind, c(215, 216, 414)); hh[[paste0(kind, "_junk")]] <- rt(kind, c(401, 402, 407, 408, 409, 411, 413)); hh[[paste0(kind, "_junk_drink")]] <- rt(kind, c(402, 411, 413)); hh[[paste0(kind, "_junk_food")]] <- rt(kind, c(401, 407, 408, 409)); hh[[paste0(kind, "_alcohol")]] <- hh[[paste0("f403", kind)]]; hh[[paste0(kind, "_cafe")]] <- hh[[paste0("f404", kind)]]; hh[[paste0(kind, "_sugar")]] <- hh[[paste0("f405", kind)]]
  }
  hh
}

make_analysis_ready <- function(hh_intermediate, save_name = NULL) {
  hh <- hh_intermediate
  hh$pc_hh <- hh$n_hh
  hh <- hh |>
    dplyr::group_by(etapa, id_loc) |>
    dplyr::mutate(vil_N = dplyr::n()) |>
    dplyr::ungroup()
  
  # Unit values and geographic imputation.
  for (code in food_codes) {
    spent <- paste0("f", code, "spent"); bought <- paste0("f", code, "bought"); pr_hh <- paste0("f", code, "pr_hh")
    if (!all(c(spent, bought) %in% names(hh))) next
    hh[[pr_hh]] <- as_num(hh[[spent]]) / as_num(hh[[bought]])
    idx <- true_idx(as_num(hh$etapa) == 2); z <- as_num(hh[[pr_hh]]); z[idx] <- z[idx] * (100 / 109.4); z[true_idx(z == 0)] <- NA_real_; hh[[pr_hh]] <- z
    for (e in c(1, 2)) {
      idx <- hh$etapa == e & !is.na(hh[[pr_hh]]); if (any(idx)) { cap <- stata_pctile(hh[[pr_hh]][idx], 95); hh[[pr_hh]][idx & hh[[pr_hh]] > cap] <- cap }
    }
    hh <- hh |>
      dplyr::group_by(etapa, id_loc) |>
      dplyr::mutate("{paste0('f', code, 'pr_uv_vil')}" := median(.data[[pr_hh]], na.rm = TRUE), "{paste0('f', code, 'uv_vil_count')}" := sum(!is.na(.data[[pr_hh]]))) |>
      dplyr::ungroup() |>
      dplyr::group_by(etapa, id_mun) |>
      dplyr::mutate("{paste0('f', code, 'pr_uv_mun')}" := median(.data[[pr_hh]], na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::group_by(etapa, cve_ent) |>
      dplyr::mutate("{paste0('f', code, 'pr_uv_ent')}" := median(.data[[pr_hh]], na.rm = TRUE)) |>
      dplyr::ungroup()
    vil <- paste0("f", code, "pr_uv_vil"); mun <- paste0("f", code, "pr_uv_mun"); ent <- paste0("f", code, "pr_uv_ent"); cnt <- paste0("f", code, "uv_vil_count"); imp <- paste0("f", code, "pr_uv_imp"); pr <- paste0("f", code, "pr")
    hh[[imp]] <- dplyr::coalesce(hh[[mun]], hh[[ent]])
    hh[[pr]] <- ifelse((hh[[cnt]] / hh$vil_N) > .2, hh[[vil]], hh[[imp]]); hh[[pr]][is.na(hh[[pr]])] <- hh[[imp]][is.na(hh[[pr]])]
    hh <- drop_existing(hh, c(mun, ent))
  }
  
  # Baseline prices and food expenditure at baseline prices.
  for (code in food_codes) {
    pr <- paste0("f", code, "pr"); ate <- paste0("f", code, "ate"); prb <- paste0("f", code, "pr_b"); exp <- paste0("f", code, "exp")
    if (!all(c(pr, ate) %in% names(hh))) next
    hh[[prb]] <- ifelse(hh$etapa == 1, hh[[pr]], NA_real_)
    hh <- fill_within(hh, "id_loc", prb, order = "etapa")
    hh[[exp]] <- hh[[prb]] * as_num(hh[[ate]])
  }
  
  # Calories and nutrients from conversion table.
  nutr <- read_csv_safe(raw_support("Base_valornutritivo_edited.csv"))
  if (ncol(nutr) >= 2) {
    food_id <- suppressWarnings(as.integer(nutr[[1]]))
    nutrient_names <- names(nutr)[2:ncol(nutr)]
    for (j in seq_along(nutrient_names)) {
      nm <- nutrient_names[[j]]
      item_cols <- character()
      for (r in seq_len(nrow(nutr))) {
        code <- food_id[[r]]; ate <- paste0("f", code, "ate")
        if (is.na(code) || !ate %in% names(hh)) next
        item <- paste0("f", code, nm); hh[[item]] <- as_num(hh[[ate]]) * suppressWarnings(as.numeric(nutr[[nm]][r])) * 10; item_cols <- c(item_cols, item)
      }
      hh[[nm]] <- row_total(hh, item_cols); hh[[paste0("pc_", nm)]] <- hh[[nm]] / hh$pc_hh
      if (j != 1) hh <- drop_existing(hh, item_cols) # Stata retains item-level calories only (column 2).
    }
  }
  
  # Top-code HH-level consumption using per-capita 3 sd + mean.
  top_vars <- c(grep("^f...exp$", names(hh), value = TRUE), grep("^n...exp$", names(hh), value = TRUE), grep("^f...ate$", names(hh), value = TRUE), grep("^f...cal$", names(hh), value = TRUE), intersect("exp_food_away", names(hh)))
  for (nm in unique(top_vars)) {
    pc <- as_num(hh[[nm]]) / hh$pc_hh; nz <- pc[!is.na(pc) & pc != 0]
    if (length(nz) > 1) { cap <- mean(nz) + 3 * stats::sd(nz); idx <- !is.na(pc) & pc > cap; hh[[nm]][idx] <- cap * hh$pc_hh[idx] }
  }
  
  hh <- make_food_categories(hh)
  # Named prices.
  price_map <- c(corn_fl = 203, rice = 210, cookie = 211, pasta = 208, cereal = 212, beans = 213, lentil = 214, chicken = 301, beef_pork = 302, seafood = 304, can_fish = 305, eggs = 306, milk_lq = 307, milk_pwd = 312, oil = 406, alcohol = 403, cafe = 404, sugar = 405)
  for (nm in names(price_map)) if (paste0("f", price_map[[nm]], "pr") %in% names(hh)) hh[[paste0("pr_", nm)]] <- hh[[paste0("f", price_map[[nm]], "pr")]]
  hh$pr_ikbasic <- 3 * hh$f203pr_b + 2 * hh$f213pr_b + 2 * hh$f210pr_b + 1.2 * hh$f208pr_b + hh$f406pr_b + 1.92 * hh$f312pr_b + hh$f211pr_b
  hh$pr_ikcomp <- hh$f214pr_b + .6 * hh$f305pr_b + .2 * hh$f212pr_b; hh$pr_ik <- hh$pr_ikbasic + hh$pr_ikcomp
  
  # Non-food categories.
  hh$exp_sch <- row_total(hh, c("n101exp", "n205exp", "n304exp", "n308exp", "n312exp", "n313exp")); hh$exp_clh <- row_total(hh, c("n305exp", "n306exp", "n307exp", "n309exp", "n310exp", "n311exp")); hh$exp_clh_c <- row_total(hh, c("n305exp", "n309exp")); hh$exp_clh_w <- row_total(hh, c("n306exp", "n310exp")); hh$exp_clh_m <- row_total(hh, c("n307exp", "n311exp"))
  hh$exp_med_hyg <- row_total(hh, c("n201exp", "n203exp", "n204exp")); hh$exp_med <- row_total(hh, c("n203exp", "n204exp")); hh$exp_hh_items <- row_total(hh, c("n202exp", "n206exp", "n207exp", "n301exp", "n302exp")); hh$exp_energy <- row_total(hh, c("n206exp", "n207exp")); hh$exp_oth_trans <- hh$n102exp; hh$exp_toys <- hh$n303exp; hh$exp_tbc <- hh$n103exp
  hh$exp_food <- row_total(hh, c(grep("^f[1-4]..exp$", names(hh), value = TRUE), "exp_food_away"))
  # Match Stata exactly: egen exp_nfood = rowtotal(n10* n201exp-n207exp n301exp-n313exp), missing.
  # n208exp, n314exp, and n315exp exist in the Section 7 module but are intentionally excluded from exp_nfood.
  nfood_total_vars <- c(
    paste0("n10", 1:3, "exp"),
    paste0("n20", 1:7, "exp"),
    paste0("n3", sprintf("%02d", 1:13), "exp")
  )
  hh$exp_nfood <- row_total(hh, nfood_total_vars)
  hh$exp_total <- row_total(hh, c("exp_food", "exp_nfood"))
  hh$exp_inkind <- row_total(hh, paste0("f", c(203, 208, 210, 211, 312, 213, 214, 305, 406, 212), "exp"))
  hh$exp_non_inkind <- hh$exp_total - hh$exp_inkind
  hh$cal_food <- row_total(hh, grep("^f[1-4]..cal$", names(hh), value = TRUE)); hh$cal_inkind <- row_total(hh, paste0("f", c(203, 208, 210, 211, 212, 213, 214, 305, 312, 406), "cal")); hh$cal_non_inkind <- hh$cal_food - hh$cal_inkind
  
  pc_vars <- unique(c(grep("^f...exp$|^n...exp$|^f...ate$|^f...cal$", names(hh), value = TRUE), grep("^(exp|ate)", names(hh), value = TRUE)))
  for (nm in pc_vars) if (!startsWith(nm, "pc_")) hh[[paste0("pc_", nm)]] <- as_num(hh[[nm]]) / hh$pc_hh
  
  # Fill demographic variables within household and generate baseline values.
  demo <- c("farm", "npal_pckg", "coverage", "p_opor_any", "p_dif", "p_liconsa", "p_desay_esc", "indig_hh", "n_hh", "n_hh2", grep("^n_age", names(hh), value = TRUE), grep("^d_age", names(hh), value = TRUE), "n_working", grep("^emax", names(hh), value = TRUE), "jefe_male", "jefe_educ", "jefe_age", "jefe_age2", "dirt_flr", "temp_wall_roof", "no_kitchen", "n_rooms", "agua_tube", "bath_in", "bath_out", "lights", "fridge", "washer", "gas_stove", "car_moto", "own_home")
  demo <- intersect(unique(demo), names(hh)); hh <- fill_within(hh, "cve_viv", demo, order = "etapa")
  baseline_vars <- unique(c(demo, grep("^ate", names(hh), value = TRUE), grep("^f...ate$", names(hh), value = TRUE), grep("^pr", names(hh), value = TRUE), c("exp_food", "exp_total", "exp_inkind")))
  for (nm in intersect(baseline_vars, names(hh))) { bn <- paste0("b_", nm); hh[[bn]] <- ifelse(hh$etapa == 1, hh[[nm]], NA); hh <- fill_within(hh, "cve_viv", bn, order = "etapa") }
  
  # Treatment receipt prediction for control households.
  hh$jefe_no_educ <- as.integer(hh$jefe_educ == 0); hh$jefe_primaria <- as.integer(dplyr::between(hh$jefe_educ, 1, 5)); hh$jefe_secundaria <- as.integer(dplyr::between(hh$jefe_educ, 6, 8))
  hh$depen_ratio <- (hh$n_age0_14 + hh$n_age65_98) / (hh$n_hh - hh$n_age0_14 - hh$n_age65_98)
  idx <- !is.finite(hh$depen_ratio); hh$depen_ratio[idx] <- (hh$n_age0_14[idx] + hh$n_age65_98[idx]) / (1 + hh$n_hh[idx] - hh$n_age0_14[idx] - hh$n_age65_98[idx])
  hh$crowding_index <- hh$n_hh / hh$n_rooms; if ("vcr" %in% names(hh)) hh$vcr[is.na(hh$vcr)] <- 0; if ("p_seg_pop" %in% names(hh)) hh$p_seg_pop[is.na(hh$p_seg_pop)] <- 0
  cedula <- intersect(c("dirt_flr", "bath_in", "jefe_no_educ", "jefe_primaria", "jefe_secundaria", "jefe_age", "depen_ratio", "crowding_index", "vcr", "gas_stove", "fridge", "washer", "car_moto", "jefe_male", "p_seg_pop", "n_age5_15_noschool", "n_age0_11"), names(hh))
  train <- hh$group != 1 & hh$etapa == 1 & !is.na(hh$p_pal)
  if (sum(train, na.rm = TRUE) > length(cedula) + 5) {
    pf <- stats::as.formula(paste("p_pal ~", paste(cedula, collapse = " + ")))
    probit <- stats::glm(pf, data = hh[train, ], family = stats::binomial(link = "probit"))
    hh$p_pal_hat <- NA_real_; idx <- true_idx(as_num(hh$etapa) == 1); hh$p_pal_hat[idx] <- stats::predict(probit, newdata = hh[idx, , drop = FALSE], type = "response")
    rate <- mean(hh$p_pal[train], na.rm = TRUE); ctrl_missing <- hh$group == 1 & hh$etapa == 1 & is.na(hh$p_pal_a)
    cutoff <- if (any(ctrl_missing, na.rm = TRUE)) stata_pctile(hh$p_pal_hat[ctrl_missing], 100 * (1 - rate)) else NA_real_
    hh$p_pal_predicted <- ifelse(ctrl_missing, as.integer(hh$p_pal_hat > cutoff), NA_integer_); hh <- fill_within(hh, "cve_viv", "p_pal_predicted", order = "etapa")
  } else { hh$p_pal_hat <- NA_real_; hh$p_pal_predicted <- NA_real_ }
  hh$p_pal_f_source <- dplyr::case_when(hh$group != 1 ~ "survey", hh$group == 1 & !is.na(hh$p_pal_a) ~ "admin", hh$group == 1 ~ "predicted", TRUE ~ NA_character_)
  hh$p_pal_f <- dplyr::case_when(hh$group != 1 ~ hh$p_pal, hh$group == 1 & !is.na(hh$p_pal_a) ~ hh$p_pal_a, hh$group == 1 ~ hh$p_pal_predicted, TRUE ~ NA_real_)
  hh <- drop_existing(hh, intersect(c("p_pal_hat", "p_pal_predicted", "p_pal_a", "p_pal_m", "p_pal"), names(hh)))
  if ("special_event" %in% names(hh)) { hh$special_event[true_idx(is.na(hh$special_event) | as_num(hh$special_event) == 2)] <- 0; hh <- hh |> dplyr::group_by(cve_viv) |> dplyr::mutate(event = as.integer(sum(special_event, na.rm = TRUE) == 1)) |> dplyr::ungroup(); hh$special_etapa1 <- as.integer(hh$special_event == 1 & hh$etapa == 1) }
  
  if (!is.null(save_name)) save_processed(hh, save_name)
  hh
}


build_analysis_datasets <- function(intermediates, save = TRUE) {
  hh <- make_analysis_ready(
    intermediates$panel,
    save_name = if (save) "hh" else NULL
  )
  
  checkpoint(
    "08a_hh_processed",
    hh,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE
  )
  
  hh_attrit <- make_analysis_ready(
    intermediates$attrit,
    save_name = if (save) "hh_attrit" else NULL
  )
  
  checkpoint(
    "08b_hh_attrit_processed",
    hh_attrit,
    key = c("cve_viv", "etapa"),
    both_waves = TRUE
  )
  
  list(
    hh = hh,
    hh_attrit = hh_attrit
  )
}