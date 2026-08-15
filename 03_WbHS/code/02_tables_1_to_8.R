# Translate 01_Tables1to8.do: descriptive/balance tables, treatment effects,
# Romano-Wolf adjusted p-values, and heterogeneity analysis.

prepare_analysis_vars <- function(d) {
  d$cavities <- d$seg_indice_cariados
  d$ltotal_1a <- ifelse(!is.na(d$ltotal_bl), d$ltotal, NA_real_)
  d$ltotal_2a <- ifelse(!is.na(d$ltotal_bl), d$ltotal_2, NA_real_)
  d$interact <- d$index_interactions
  d$Employed_All <- d$SEG2_employed
  d$new_index_interactions <- d$index_interactions

  d$new_sd_ohip1_bl <- d$new_sd_ohip0
  d$new_sd_rosen1_bl <- d$new_sd_rosen0
  d$cavities_bl <- d$lb_indice_cariados
  d$needtreat_bl <- d$lb_indice_sintrat
  d$employed_bl <- d$lb_employ
  d$ltotal_1a_bl <- d$ltotal_bl
  d$new_sd_ohip2_bl <- d$new_sd_ohip0
  d$new_sd_rosen2_bl <- d$new_sd_rosen0
  d$SEG2_employed_bl <- d$lb_employ
  d$ltotal_2a_bl <- d$ltotal_bl
  d$smiles_bl <- d$new_sd_ohip0
  d$sd_index_photos_bl <- d$new_sd_ohip0
  d$interact_bl <- d$new_sd_rosen0
  d$efforts_bl <- d$new_sd_rosen0
  d$sf12_ph_sd_bl <- d$new_sd_rosen0
  d$sf12_mh_sd_bl <- d$new_sd_rosen0
  for (v in c("completed", "dental_service", "dental_serviceFU2", "kkk1", "kkk2", "prosthesis1", "prosthesis2")) {
    d[[paste0(v, "_bl")]] <- 0
  }
  d$n_estrato <- make_n_estrato(d)
  d
}

balance_vars <- c(
  "edad", "lb_genero", "casado_o_convive", "lb_jefe_hogar", "lb_residentes_totales",
  "lb_residentes_menores", "lb_residentes_ninos", "lb_residentes_adulto",
  "lb_residentes_adulto_mayor", "ed_8menos", "ed_9_11", "ed_12", "ed_mas_12",
  "lb_years_education", "lb_employ", "lb_ingreso_th", "lb_cont_formal",
  "fonasa_1", "fonasa_2", "fonasa_3", "fonasa_4", "lb_ohip14", "lb_rosenberg_score",
  "lb_dientes_falt_tot", "lb_dientes_falt_front", "lb_prosthesis_up", "lb_prosthesis_down"
)

balance_labels <- c(
  edad = "Age", lb_genero = "Gender", casado_o_convive = "Married or cohabitates",
  lb_jefe_hogar = "Head of household", lb_residentes_totales = "Number of residents at home",
  lb_residentes_menores = "Residents under 5 years at home",
  lb_residentes_ninos = "Residents between 5 and 18 years at home",
  lb_residentes_adulto = "Adults in HH", lb_residentes_adulto_mayor = "Residents over 65 years at home",
  ed_8menos = "Education 8 or less years", ed_9_11 = "Education between 9 and 11 years",
  ed_12 = "Education 12 years", ed_mas_12 = "Education more than 12 years",
  lb_years_education = "Years of education", lb_employ = "Employed",
  lb_ingreso_th = "Labor income (thousand pesos)", lb_cont_formal = "Has a formal contract",
  fonasa_1 = "FONASA A", fonasa_2 = "FONASA B", fonasa_3 = "FONASA C", fonasa_4 = "FONASA D",
  lb_ohip14 = "OHIP score", lb_rosenberg_score = "Rosenberg score",
  lb_dientes_falt_tot = "Total missing teeth", lb_dientes_falt_front = "Front missing teeth",
  lb_prosthesis_up = "Upper prosthetic need", lb_prosthesis_down = "Lower prosthetic need"
)

make_table1_panel <- function(d, panel, sample_mask) {
  require_cols(d, balance_vars)
  rows <- lapply(balance_vars, function(v) {
    z <- d[sample_mask, , drop = FALSE]
    fit <- try(robust_lm(z, v, "lb_genero"), silent = TRUE)
    p <- if (inherits(fit, "try-error")) NA_real_ else coef_stats(fit, "lb_genero")[["p"]]
    data.frame(
      panel = panel, variable = v, label = unname(balance_labels[v]),
      mean = safe_mean(z[[v]]), men = safe_mean(z[[v]][z$lb_genero == 0]),
      women = safe_mean(z[[v]][z$lb_genero == 1]), p_gender = p,
      n = sum(!is.na(z[[v]])), stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

make_table2_panel <- function(d, panel, sample_mask) {
  z <- d[sample_mask, , drop = FALSE]
  rows <- lapply(balance_vars, function(v) {
    fit <- try(robust_lm(z, v, c("treatment", strata_vars(z, include_base = TRUE))), silent = TRUE)
    p <- if (inherits(fit, "try-error")) NA_real_ else coef_stats(fit, "treatment")[["p"]]
    data.frame(
      panel = panel, variable = v, label = unname(balance_labels[v]),
      treatment_mean = safe_mean(z[[v]][z$treatment == 1]),
      control_mean = safe_mean(z[[v]][z$treatment == 0]), p_difference = p,
      n = if (inherits(fit, "try-error")) NA_integer_ else stats::nobs(fit$model),
      stringsAsFactors = FALSE
    )
  })
  # Joint orthogonality test: treatment as LHS, all balance variables plus strata as RHS.
  joint_fit <- try(robust_lm(z, "treatment", c(balance_vars, strata_vars(z, include_base = TRUE))), silent = TRUE)
  joint_p <- if (inherits(joint_fit, "try-error")) NA_real_ else wald_p(joint_fit, balance_vars)
  out <- dplyr::bind_rows(rows)
  out$joint_f_p <- joint_p
  out
}

main_outcome_block <- function(d, outcomes, baselines, panel, sample_masks = NULL,
                               control_type = "unb", rw = NULL, rw_key = NULL,
                               gender_controls = NULL) {
  groups <- c("All", "Men", "Women")
  pieces <- list(); k <- 1L
  for (i in seq_along(outcomes)) {
    y <- outcomes[i]
    bl <- baselines[i]
    mask <- if (is.null(sample_masks)) NULL else sample_masks[[i]]
    ctr <- if (control_type == "all") controls_all() else controls_unb()
    for (g in groups) {
      r <- outcome_result(d, y, baseline = if (nzchar(bl)) bl else NULL, group = g,
                          controls = ctr, sample_mask = mask)
      r$panel <- panel
      r$gender_diff_p <- NA_real_
      pieces[[k]] <- r; k <- k + 1L
    }
    gc <- gender_controls %||% ctr
    extra <- if (nzchar(bl)) paste0(bl, ":lb_genero") else character()
    pd <- gender_diff_p(d, y, baseline = if (nzchar(bl)) bl else NULL,
                        controls = gc, sample_mask = mask, extra_terms = extra)
    for (j in (k - 3L):(k - 1L)) pieces[[j]]$gender_diff_p <- pd
  }
  out <- dplyr::bind_rows(pieces)
  if (!is.null(rw)) {
    out$rw_p <- mapply(function(y, g) {
      key <- if (!is.null(rw_key) && y %in% names(rw_key)) rw_key[[y]] else y
      z <- rw[[g]]
      if (is.null(z) || !key %in% names(z)) NA_real_ else unname(z[[key]])
    }, out$outcome, out$group)
  } else out$rw_p <- NA_real_
  out
}

run_tables_1_to_8 <- function(root) {
  out_dir <- ensure_dir(file.path(root, "output", "tables"))
  dfile <- file.path(root, "data", "processed", "main_dataset.rds")
  require_files(dfile)
  d <- readRDS(dfile)
  d <- prepare_analysis_vars(d)

  # Table 1: descriptive statistics.
  t1a <- make_table1_panel(d, "Baseline", rep(TRUE, nrow(d)))
  t1b <- make_table1_panel(d, "First Follow-up", d$Encuestado_SEG1 == 1)
  t1c <- make_table1_panel(d, "Second Follow-up", d$Encuestado_SEG2 == 1)
  write_table_outputs(t1a, "WBHS_Table1A", "Descriptive Statistics: Baseline", out_dir)
  write_table_outputs(t1b, "WBHS_Table1B", "Descriptive Statistics: First Follow-up", out_dir)
  write_table_outputs(t1c, "WBHS_Table1C", "Descriptive Statistics: Second Follow-up", out_dir)
  write_table_outputs(dplyr::bind_rows(t1a,t1b,t1c), "WBHS_Table1", "Descriptive Statistics", out_dir)

  # Table 2: balance tests.
  t2a <- make_table2_panel(d, "Baseline", rep(TRUE, nrow(d)))
  t2b <- make_table2_panel(d, "First Follow-up", d$Encuestado_SEG1 == 1)
  t2c <- make_table2_panel(d, "Second Follow-up", d$Encuestado_SEG2 == 1)
  write_table_outputs(t2a, "WBHS_Table2A", "Balance Tests: Baseline", out_dir)
  write_table_outputs(t2b, "WBHS_Table2B", "Balance Tests: First Follow-up", out_dir)
  write_table_outputs(t2c, "WBHS_Table2C", "Balance Tests: Second Follow-up", out_dir)
  write_table_outputs(dplyr::bind_rows(t2a,t2b,t2c), "WBHS_Table2", "Balance Tests", out_dir)

  # Romano-Wolf adjusted p-values needed in Tables 3-6.
  fu1 <- c("new_sd_ohip1", "new_sd_rosen1", "smiles", "sd_index_photos", "employed",
           "ltotal_1a", "completed", "kkk1", "dental_service", "cavities", "needtreat", "prosthesis1")
  fu2 <- c("new_sd_ohip2", "new_sd_rosen2", "ltotal_2a", "interact", "efforts",
           "sf12_ph_sd", "sf12_mh_sd", "SEG2_employed", "dental_serviceFU2", "prosthesis2")
  rw_reps <- as.integer(getOption("wbhs.rw_reps", 200L))
  rw <- list()
  for (g in c("All", "Men", "Women")) {
    message("Romano-Wolf: FU1, ", g, " (", rw_reps, " permutations)")
    rw[[paste0("FU1_", g)]] <- romano_wolf(d, fu1, reps = rw_reps, seed = 456L, group = g)
    message("Romano-Wolf: FU2, ", g, " (", rw_reps, " permutations)")
    rw[[paste0("FU2_", g)]] <- romano_wolf(d, fu2, reps = rw_reps, seed = 456L, group = g)
  }
  rw_long <- dplyr::bind_rows(lapply(names(rw), function(nm) {
    parts <- strsplit(nm, "_", fixed = TRUE)[[1]]
    data.frame(followup = parts[1], group = parts[2], outcome = names(rw[[nm]]), rw_p = unname(rw[[nm]]))
  }))
  utils::write.csv(rw_long, file.path(out_dir, "romano_wolf_adjusted_p.csv"), row.names = FALSE)
  rw_fu1 <- setNames(lapply(c("All","Men","Women"), function(g) rw[[paste0("FU1_",g)]]), c("All","Men","Women"))
  rw_fu2 <- setNames(lapply(c("All","Men","Women"), function(g) rw[[paste0("FU2_",g)]]), c("All","Men","Women"))

  # Table 3: take-up of dental services.
  t3a <- main_outcome_block(
    d, c("completed", "dental_service", "prosthesis1"), c("", "", ""), "First Follow-up",
    sample_masks = list(NULL, d$FirstFU == 1, NULL), control_type = "all", rw = rw_fu1,
    gender_controls = controls_all_gender()
  )
  t3b <- main_outcome_block(
    d, c("dental_serviceFU2", "prosthesis2"), c("", ""), "Second Follow-up",
    sample_masks = list(d$SecondFU == 1, NULL), control_type = "all", rw = rw_fu2,
    gender_controls = controls_all_gender()
  )
  t3 <- dplyr::bind_rows(t3a, t3b)
  t3$across_survey_p <- NA_real_
  for (g in c("All","Men","Women")) {
    p1 <- panel_treatment_diff(d, "dental_service", "dental_serviceFU2", g)
    p2 <- panel_treatment_diff(d, "prosthesis1", "prosthesis2", g)
    t3$across_survey_p[t3$outcome == "dental_serviceFU2" & t3$group == g] <- p1
    t3$across_survey_p[t3$outcome == "prosthesis2" & t3$group == g] <- p2
  }
  write_table_outputs(t3, "WBHS_Table3", "Take-Up of Dental Services", out_dir)

  # Table 4: dental health outcomes.
  map4_fu1 <- c(seg_indice_cariados = "cavities", needtreat = "needtreat", sd_ohip1 = "new_sd_ohip1", smiles = "smiles")
  t4a <- main_outcome_block(d, c("seg_indice_cariados","needtreat"), c("",""), "Objective Dental Health",
                            control_type = "all", rw = rw_fu1, rw_key = map4_fu1,
                            gender_controls = controls_all_gender())
  t4b <- main_outcome_block(d, "sd_ohip1", "sd_ohip0", "Subjective Oral Health: First Follow-up",
                            control_type = "unb", rw = rw_fu1, rw_key = map4_fu1,
                            gender_controls = controls_unb_gender())
  map4_fu2 <- c(sd_ohip2 = "new_sd_ohip2")
  t4c <- main_outcome_block(d, "sd_ohip2", "sd_ohip0", "Subjective Oral Health: Second Follow-up",
                            control_type = "unb", rw = rw_fu2, rw_key = map4_fu2,
                            gender_controls = controls_unb_gender())
  t4d <- main_outcome_block(d, "smiles", "", "Smiling Behavior",
                            control_type = "all", rw = rw_fu1, rw_key = map4_fu1,
                            gender_controls = controls_all_gender())
  # The full-sample Stata smiling regression uses baseline controls interacted with gender.
  smile_all <- outcome_result(d, "smiles", group="All", controls=controls_all_gender())
  idx_smile_all <- t4d$outcome=="smiles" & t4d$group=="All"
  t4d[idx_smile_all, c("control_mean","estimate","se","p","stars","n")] <- smile_all[1, c("control_mean","estimate","se","p","stars","n")]
  t4 <- dplyr::bind_rows(t4a,t4b,t4c,t4d)
  t4$across_survey_p <- NA_real_
  for (g in c("All","Men","Women")) {
    t4$across_survey_p[t4$outcome == "sd_ohip2" & t4$group == g] <- panel_treatment_diff(d, "sd_ohip1", "sd_ohip2", g)
  }
  write_table_outputs(t4, "WBHS_Table4", "Treatment Effects on Dental Health Outcomes", out_dir)

  # Table 5: psychological, social, economic and health outcomes.
  t5a <- main_outcome_block(d, c("sd_rosen1","employed","ltotal_1a"), c("sd_rosen0","lb_employ","ltotal_bl"),
                            "First Follow-up", control_type = "unb", rw = rw_fu1,
                            rw_key = c(sd_rosen1="new_sd_rosen1", employed="employed", ltotal_1a="ltotal_1a"),
                            gender_controls = controls_unb_gender())
  t5b <- main_outcome_block(d, c("sd_rosen2","sf12_ph_sd","sf12_mh_sd","Employed_All","ltotal_2a"),
                            c("sd_rosen0","sd_rosen0","sd_rosen0","lb_employ","ltotal_bl"),
                            "Second Follow-up", control_type = "unb", rw = rw_fu2,
                            rw_key = c(sd_rosen2="new_sd_rosen2", sf12_ph_sd="sf12_ph_sd", sf12_mh_sd="sf12_mh_sd",
                                       Employed_All="SEG2_employed", ltotal_2a="ltotal_2a"),
                            gender_controls = controls_unb_gender())
  # In the supplied Stata file, $controls_all_unb is undefined in the SF-12 gender-difference
  # regressions, so those tests include the baseline Rosenberg interaction plus strata/interacted strata only.
  for (yy in c("sf12_ph_sd","sf12_mh_sd")) {
    pp <- gender_diff_p(d, yy, baseline="sd_rosen0", controls=character(), extra_terms="sd_rosen0:lb_genero")
    t5b$gender_diff_p[t5b$outcome==yy] <- pp
  }
  t5 <- dplyr::bind_rows(t5a,t5b)
  t5$across_survey_p <- NA_real_
  for (g in c("All","Men","Women")) {
    t5$across_survey_p[t5$outcome == "sd_rosen2" & t5$group == g] <- panel_treatment_diff(d, "sd_rosen1", "sd_rosen2", g)
    t5$across_survey_p[t5$outcome == "Employed_All" & t5$group == g] <- panel_treatment_diff(d, "employed", "Employed_All", g)
    t5$across_survey_p[t5$outcome == "ltotal_2a" & t5$group == g] <- panel_treatment_diff(d, "ltotal", "ltotal_2", g, baseline_work_filter = TRUE)
  }
  write_table_outputs(t5, "WBHS_Table5", "Treatment Effects on Psychological, Social, Economic and Health Outcomes", out_dir)

  # Table 6: additional outcomes.
  t6 <- main_outcome_block(d, c("efforts","new_index_interactions","sd_index_photos"), c("","",""),
                           "Additional Outcomes", control_type = "all", gender_controls = controls_all_gender())
  # Attach adjusted p-values from the appropriate family/follow-up.
  t6$rw_p <- mapply(function(y,g) {
    if (y == "sd_index_photos") rw_fu1[[g]][["sd_index_photos"]]
    else rw_fu2[[g]][[if (y == "new_index_interactions") "interact" else y]]
  }, t6$outcome, t6$group)
  write_table_outputs(t6, "WBHS_Table6", "Effects on Additional Outcomes", out_dir)

  # Tables 7-8: heterogeneity.
  d$n_total <- d$lb_dientes_faltantes_totales
  front_missing <- if ("lb_dientes_faltantes_front" %in% names(d)) d$lb_dientes_faltantes_front else d$lb_dientes_falt_front
  d$n_nofron <- d$lb_dientes_faltantes_totales - front_missing
  d$n_fron <- front_missing
  d$c_total <- d$n_total - safe_mean(d$n_total)
  q_nofron <- stats::quantile(d$n_nofron, probs = c(.50,.75), na.rm = TRUE, type = 2)
  d$c_nofron <- d$n_nofron - safe_mean(d$n_nofron)
  d$d_nofron0 <- as.numeric(!is.na(d$n_nofron) & d$n_nofron >= q_nofron[[1]] & d$n_nofron < q_nofron[[2]])
  d$d_nofron1 <- as.numeric(!is.na(d$n_nofron) & d$n_nofron >= q_nofron[[2]])
  d$dummy_no0 <- as.numeric(!is.na(d$n_nofron) & d$n_nofron >= q_nofron[[1]])
  d$c_fron <- d$n_fron - safe_mean(d$n_fron)
  d$dummy_smile1 <- as.numeric(!is.na(d$n_fron) & d$n_fron > 1)
  d$dummy_smile0 <- as.numeric(!is.na(d$n_fron) & d$n_fron == 1)
  d$dummy_fron0 <- as.numeric(!is.na(d$n_fron) & d$n_fron > 0)
  d$emplo_av <- (d$employed + d$SEG2_employed) / 2
  d$ly_av <- (d$ltotal + d$ltotal_2) / 2
  d$ohip_av <- (d$new_sd_ohip1 + d$new_sd_ohip2) / 2
  d$rosen_av <- (d$new_sd_rosen1 + d$new_sd_rosen2) / 2

  het_spec <- function(outcome, baseline, group, type) {
    mask <- subgroup_mask(d, group)
    if (type == "median_front") {
      terms <- c("treatment", baseline, "treatment * dummy_fron0", "treatment * dummy_no0", controls_unb(), strata_vars(d))
      fit <- robust_lm(d, outcome, terms, subset = mask)
      a <- coef_stats(fit, "treatment:dummy_fron0")
      b <- coef_stats(fit, "treatment:dummy_no0")
      dplyr::bind_rows(
        data.frame(heterogeneity="Missing at least one front tooth", estimate=a[["estimate"]],se=a[["se"]],p=a[["p"]]),
        data.frame(heterogeneity="Many non-front teeth (>= median)", estimate=b[["estimate"]],se=b[["se"]],p=b[["p"]])
      )
    } else if (type == "p75_twofront") {
      terms <- c("treatment", baseline, "treatment * dummy_smile1", "treatment * d_nofron1", controls_unb(), strata_vars(d))
      fit <- robust_lm(d, outcome, terms, subset = mask)
      a <- coef_stats(fit, "treatment:dummy_smile1")
      b <- coef_stats(fit, "treatment:d_nofron1")
      dplyr::bind_rows(
        data.frame(heterogeneity="Missing at least two front teeth", estimate=a[["estimate"]],se=a[["se"]],p=a[["p"]]),
        data.frame(heterogeneity="Many non-front teeth (>= p75)", estimate=b[["estimate"]],se=b[["se"]],p=b[["p"]])
      )
    } else {
      mask <- mask & !is.na(d$ltotal_bl)
      terms <- c("treatment", paste0(baseline, " * dummy_fron0"), "treatment * dummy_fron0",
                 "treatment * c_total * dummy_fron0", "treatment * c_fron * dummy_fron0",
                 controls_unb(), strata_vars(d))
      fit <- robust_lm(d, outcome, terms, subset = mask)
      a <- coef_stats(fit, "treatment:dummy_fron0")
      data.frame(heterogeneity="Missing at least one front tooth (fully interacted controls)",
                 estimate=a[["estimate"]],se=a[["se"]],p=a[["p"]])
    }
  }

  het_outcomes <- data.frame(
    outcome = c("ohip_av","rosen_av","emplo_av","ly_av"),
    baseline = c("new_sd_ohip0","new_sd_rosen0","lb_employ","ltotal_bl"),
    table = c(7,7,8,8), stringsAsFactors = FALSE
  )
  hres <- list(); k <- 1L
  for (i in seq_len(nrow(het_outcomes))) for (g in c("Women","Men")) for (sp in c("median_front","p75_twofront","full_controls")) {
    z <- het_spec(het_outcomes$outcome[i], het_outcomes$baseline[i], g, sp)
    z$outcome <- het_outcomes$outcome[i]; z$group <- g; z$specification <- sp; z$table <- het_outcomes$table[i]
    hres[[k]] <- z; k <- k + 1L
  }
  hres <- dplyr::bind_rows(hres)
  write_table_outputs(hres[hres$table == 7, c("outcome","group","specification","heterogeneity","estimate","se","p")],
                      "WBHS_Table7", "Heterogeneity Analysis: Subjective Oral Health and Self-Esteem", out_dir)
  write_table_outputs(hres[hres$table == 8, c("outcome","group","specification","heterogeneity","estimate","se","p")],
                      "WBHS_Table8", "Heterogeneity Analysis: Labor Market Outcomes", out_dir)

  saveRDS(d, file.path(root, "data", "processed", "analysis_dataset.rds"))
  message("Tables 1-8 written to output/tables.")
  invisible(d)
}
