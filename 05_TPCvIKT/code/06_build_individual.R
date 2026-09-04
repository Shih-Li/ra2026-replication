# 06_build_individual.R
# Translation of individual.do.
# Builds the person-level panel from raw survey modules.

build_individual <- function(b_hh_sample = NULL, f_hh_sample = NULL, transfers = NULL, save = TRUE) {
  if (is.null(b_hh_sample)) b_hh_sample <- load_intermediate("b_hh_sample")
  if (is.null(f_hh_sample)) f_hh_sample <- load_intermediate("f_hh_sample")
  if (is.null(transfers) && file.exists(file.path(INTERMEDIATE_DIR, "transfers.rds"))) transfers <- load_intermediate("transfers")
  
  needed <- c(
    baseline_raw("s0205"), baseline_raw("s0305"), baseline_raw("s1005"),
    baseline_raw("s1105_mujeres.dta"), baseline_raw("s1105ninios.dta"),
    baseline_raw("sr2406_mujer"), baseline_raw("sr2406_ninios"),
    followup_raw("s102a_2.dta"), followup_raw("s202_2.dta"), followup_raw("s302_2.dta"),
    followup_raw("s1002_2.dta"), followup_raw("s1102_2_mujeres"), followup_raw("s1102_2_ninios.dta"),
    followup_raw("s1402_mujeres"), followup_raw("s1402_ninios"),
    followup_raw("s1602_2_anemia"), followup_raw("s1602_2_glucosa"),
    raw_support("est_avg_rec_nutrients.csv")
  )
  assert_raw_files(needed)
  
  # ---------------------------------------------------------------------------
  # Defining sample of individuals
  # ---------------------------------------------------------------------------
  b20 <- read_dta_safe(baseline_raw("s0205"))
  b_p_sample <- dplyr::inner_join(b20, b_hh_sample, by = "id_1", suffix = c("", ".hh"))
  
  # Stata's merge leaves a single `etapa` when only the using dataset has it.
  # dplyr creates `etapa.hh` only when both inputs contain `etapa`, so handle
  # both layouts explicitly instead of assuming the suffix exists.
  if ("etapa.hh" %in% names(b_p_sample)) {
    if ("etapa" %in% names(b_p_sample)) {
      b_p_sample$etapa <- dplyr::coalesce(as_num(b_p_sample$etapa), as_num(b_p_sample$etapa.hh))
    } else {
      b_p_sample$etapa <- as_num(b_p_sample$etapa.hh)
    }
    b_p_sample <- dplyr::select(b_p_sample, -etapa.hh)
  } else if ("etapa" %in% names(b_p_sample)) {
    b_p_sample$etapa <- as_num(b_p_sample$etapa)
  } else {
    stop("Baseline person sample merge produced neither `etapa` nor `etapa.hh`.", call. = FALSE)
  }
  
  b_p_sample <- b_p_sample |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::select(id_1, etapa, pid) |>
    dplyr::arrange(pid)
  save_intermediate(b_p_sample, "b_p_sample")
  
  f102a <- read_dta_safe(followup_raw("s102a_2.dta"))
  fp0 <- stata_merge(f_hh_sample, f102a, by = "cve_viv") |>
    dplyr::filter(._merge != 2, !(as_num(a6_2) %in% c(3, 4, 9))) |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::filter(is.na(split) | split != 2)
  if ("tipo_individuo" %in% names(fp0) && !"tipo_ind" %in% names(fp0)) fp0$tipo_ind <- fp0$tipo_individuo
  fp0 <- fp0 |>
    dplyr::select(dplyr::any_of(c("id_loc", "cve_viv", "cve_res", "id_1", "pid", "linea", "tapoyo", "etapa", "tipo_ind")))
  
  f202_ids <- read_dta_safe(followup_raw("s202_2.dta")) |>
    dplyr::select(cve_res) |>
    dplyr::distinct()
  f_p_sample <- dplyr::inner_join(f202_ids, fp0, by = "cve_res") |>
    dplyr::arrange(cve_res)
  save_intermediate(f_p_sample, "f_p_sample")
  
  # ---------------------------------------------------------------------------
  # Section 2 - Individual general data
  # ---------------------------------------------------------------------------
  bgen <- b20 |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::inner_join(b_p_sample, by = "pid", suffix = c("", ".sample")) |>
    dplyr::select(dplyr::matches("^s2"), id_1, pid, etapa, linea)
  
  fgen <- read_dta_safe(followup_raw("s202_2.dta"))
  fgen <- rename_existing(fgen, c(
    a2_2 = "s23", a3a_2 = "s24a", a3m_2 = "s24m", s22b_2 = "s25",
    s23b_2 = "s26a", s24_2 = "s27", s25a_2 = "s28a", s25b_2 = "s28b",
    s26_2 = "s211", s27_2 = "s212", s28_2 = "s213", s29_2 = "s214",
    s210b_2 = "s220", s211_2 = "s221", s212_2 = "s215", s213_2 = "s217",
    s214a_2 = "s218", s215a_2 = "s216", s216_2 = "s219"
  ))
  fgen <- drop_existing(fgen, c("s22a_2", "s23a_2", "s23c_2", "s210a_2", "s214b_2"))
  fgen <- dplyr::inner_join(fgen, f_p_sample, by = "cve_res", suffix = c("", ".sample"))
  if ("tipo_individuo" %in% names(fgen) && !"tipo_ind" %in% names(fgen)) fgen$tipo_ind <- fgen$tipo_individuo
  fgen <- fgen |>
    dplyr::select(dplyr::matches("^s"), dplyr::any_of(c("id_loc", "cve_viv", "cve_res", "linea", "pid", "id_1", "etapa", "tapoyo", "tipo_ind")))
  
  personal <- dplyr::bind_rows(fgen, bgen)
  personal <- rename_existing(personal, c(
    s23 = "male", s24a = "age", s24m = "age_month", s25 = "rel_a_jefe",
    s27 = "leer", s28a = "grado", s28b = "nivel", s29 = "sc_span", s210 = "sc_math",
    s211 = "attend", s212 = "indig", s213 = "espanol", s214 = "civ_stat",
    s215 = "act", s216 = "act2", s217 = "act_d", s218 = "act_t", s219 = "work_h",
    s220 = "mother", s221 = "cuidado"
  ))
  personal <- zap_all_labels(personal)
  
  personal <- personal |>
    dplyr::mutate(
      male = dplyr::case_when(as_num(male) == 2 ~ 0, TRUE ~ as_num(male)),
      age = dplyr::na_if(as_num(age), 99),
      age_month = dplyr::na_if(as_num(age_month), 99),
      age_month = dplyr::if_else(!is.na(age) & age >= 5, NA_real_, age_month),
      d_age0_5 = dplyr::if_else(dplyr::between(age, 0, 5), 1, NA_real_),
      d_age6_12 = dplyr::if_else(dplyr::between(age, 6, 12), 1, NA_real_),
      d_age13_18 = dplyr::if_else(dplyr::between(age, 13, 18), 1, NA_real_),
      d_age19_54_male = dplyr::if_else(dplyr::between(age, 19, 54) & male == 1, 1, NA_real_),
      d_age19_54_female = dplyr::if_else(dplyr::between(age, 19, 54) & male == 0, 1, NA_real_),
      d_age55_98 = dplyr::if_else(dplyr::between(age, 55, 98), 1, NA_real_),
      d_age0_14 = dplyr::if_else(dplyr::between(age, 0, 14), 1, NA_real_),
      d_age65_98 = dplyr::if_else(dplyr::between(age, 65, 98), 1, NA_real_),
      d_age0_11 = dplyr::if_else(dplyr::between(age, 0, 11), 1, NA_real_)
    )
  
  personal$ae <- NA_real_
  personal$ae[true_idx(!is.na(personal$age_month) & personal$age_month < 6 & personal$age == 0)] <- 0.26
  personal$ae[true_idx(!is.na(personal$age_month) & dplyr::between(personal$age_month, 6, 11))] <- 0.34
  personal$ae[true_idx(dplyr::between(personal$age, 1, 3))] <- 0.52
  personal$ae[true_idx(dplyr::between(personal$age, 4, 10))] <- 0.76
  personal$ae[true_idx(dplyr::between(personal$age, 11, 50) & personal$male == 1)] <- 1.12
  personal$ae[true_idx(dplyr::between(personal$age, 11, 50) & personal$male == 0)] <- 0.88
  personal$ae[true_idx(dplyr::between(personal$age, 51, 98))] <- 0.84
  
  personal <- personal |>
    dplyr::mutate(
      jefe = as.integer(!is.na(rel_a_jefe) & as_num(rel_a_jefe) == 1),
      no_insur = as.integer(!is.na(s26a) & as_num(s26a) == 2),
      leer = dplyr::case_when(as_num(leer) %in% c(9, 6) ~ NA_real_, as_num(leer) == 2 ~ 0, TRUE ~ as_num(leer)),
      grado = dplyr::case_when(as_num(grado) %in% c(8, 9) | (!is.na(age) & age < 6) ~ NA_real_, TRUE ~ as_num(grado)),
      nivel = dplyr::case_when(as_num(nivel) == 9 | (!is.na(age) & age < 6) ~ NA_real_, TRUE ~ as_num(nivel))
    )
  personal$educ <- NA_real_
  idx <- true_idx(personal$nivel %in% c(0, 1)); personal$educ[idx] <- 0
  idx <- true_idx(personal$nivel == 2); personal$educ[idx] <- personal$grado[idx]
  idx <- true_idx(personal$nivel == 3); personal$educ[idx] <- personal$grado[idx] + 6
  idx <- true_idx(personal$nivel == 4); personal$educ[idx] <- personal$grado[idx] + 9
  idx <- true_idx(personal$nivel == 4 & personal$grado == 5); personal$educ[idx] <- 12
  idx <- true_idx(dplyr::between(personal$nivel, 5, 7)); personal$educ[idx] <- personal$grado[idx] + 12
  idx <- true_idx(personal$nivel == 8); personal$educ[idx] <- personal$grado[idx] + 16
  
  personal <- personal |>
    dplyr::mutate(
      attend = dplyr::case_when(as_num(attend) %in% c(0, 9) | (!is.na(age) & age < 8) ~ NA_real_, as_num(attend) == 2 ~ 0, TRUE ~ as_num(attend)),
      d_age5_15 = dplyr::if_else(dplyr::between(age, 5, 15), 1, NA_real_),
      d_age5_15_noschool = dplyr::if_else(d_age5_15 == 1 & (is.na(attend) | attend != 1), 1, NA_real_),
      indig = dplyr::case_when(as_num(indig) == 0 ~ NA_real_, as_num(indig) == 2 ~ 0, TRUE ~ as_num(indig)),
      civ_stat = dplyr::if_else(as_num(civ_stat) %in% c(0, 9), NA_real_, as_num(civ_stat)),
      married = dplyr::if_else(!is.na(civ_stat) & dplyr::between(civ_stat, 1, 6), as.integer(civ_stat %in% c(1, 5)), NA_integer_),
      act = dplyr::if_else(as_num(act) == 9, NA_real_, as_num(act)),
      worked = dplyr::if_else(!is.na(act), as.integer(act == 1), NA_integer_),
      act2 = dplyr::case_when(as_num(act2) == 2 ~ NA_real_, as_num(act2) %in% c(8, 9, 0) ~ 8, TRUE ~ as_num(act2)),
      work_h = dplyr::if_else(as_num(work_h) %in% c(0, 88), NA_real_, as_num(work_h)),
      d_age12_15 = dplyr::if_else(dplyr::between(age, 12, 15), 1, NA_real_),
      d_age12_15_worked = dplyr::if_else(d_age12_15 == 1 & worked == 1, 1, NA_real_)
    )
  
  # RDA nutrient lookup: rows are the eight age/sex groups; Stata used columns 4:n.
  rda <- read_csv_safe(raw_support("est_avg_rec_nutrients.csv"))
  personal$grp <- dplyr::case_when(
    dplyr::between(personal$age, 0, 3) ~ 1,
    dplyr::between(personal$age, 4, 8) ~ 2,
    dplyr::between(personal$age, 9, 13) & personal$male == 1 ~ 3,
    dplyr::between(personal$age, 14, 18) & personal$male == 1 ~ 4,
    dplyr::between(personal$age, 19, 98) & personal$male == 1 ~ 5,
    dplyr::between(personal$age, 9, 13) & personal$male == 0 ~ 6,
    dplyr::between(personal$age, 14, 18) & personal$male == 0 ~ 7,
    dplyr::between(personal$age, 19, 98) & personal$male == 0 ~ 8,
    TRUE ~ NA_real_
  )
  if (ncol(rda) >= 4) {
    for (nm in names(rda)[4:ncol(rda)]) {
      vals <- suppressWarnings(as.numeric(rda[[nm]]))
      personal[[paste0("rda_", nm)]] <- ifelse(!is.na(personal$grp), vals[personal$grp], NA_real_)
    }
  }
  personal <- personal |>
    dplyr::select(-dplyr::matches("^s2"), -grp) |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # Section 3 - Health
  # ---------------------------------------------------------------------------
  bh <- read_dta_safe(baseline_raw("s0305")) |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::inner_join(b_p_sample, by = "pid", suffix = c("", ".sample")) |>
    dplyr::select(dplyr::matches("^s3"), id_1, pid, etapa, linea) |>
    dplyr::select(-dplyr::matches("resp$"), -dplyr::any_of("s321"))
  fh <- read_dta_safe(followup_raw("s302_2.dta")) |>
    dplyr::inner_join(f_p_sample, by = "cve_res", suffix = c("", ".sample"))
  for (i in 1:11) if (paste0("s3", i, "_2") %in% names(fh)) names(fh)[names(fh) == paste0("s3", i, "_2")] <- paste0("s3", i)
  fh <- fh |>
    dplyr::select(dplyr::matches("^s3"), dplyr::any_of(c("cve_viv", "cve_res", "linea", "pid", "id_1", "etapa", "tapoyo", "tipo_ind")))
  health <- dplyr::bind_rows(fh, bh)
  health <- rename_existing(health, c(
    s31 = "sick_days", s32 = "incap_days", s33 = "cant_walk", s34 = "eff_lgt", s35 = "eff_med",
    s36 = "eff_hvy", s37 = "dist_walk", s38 = "diabetes", s39 = "diabetes_t", s310 = "hyp_tens", s311 = "hyp_tens_t"
  ))
  health <- health |>
    dplyr::mutate(
      sick_days = dplyr::case_when(as_num(sick_days) %in% c(30, 29) ~ 28, as_num(sick_days) == 99 ~ NA_real_, TRUE ~ as_num(sick_days)),
      sick = dplyr::if_else(!is.na(sick_days), as.integer(sick_days >= 1), NA_integer_),
      incap_days = dplyr::case_when(as_num(incap_days) %in% c(30, 29) ~ 28, as_num(incap_days) == 99 ~ NA_real_, TRUE ~ as_num(incap_days)),
      incap = dplyr::if_else(!is.na(incap_days), as.integer(incap_days >= 1), NA_integer_),
      cant_walk = dplyr::if_else(as_num(cant_walk) == 2, 0, as_num(cant_walk)),
      eff_lgt = dplyr::if_else(as_num(eff_lgt) == 9, NA_real_, as_num(eff_lgt)),
      eff_med = dplyr::if_else(as_num(eff_med) == 9, NA_real_, as_num(eff_med)),
      eff_hvy = dplyr::if_else(as_num(eff_hvy) == 9, NA_real_, as_num(eff_hvy)),
      dist_walk = dplyr::if_else(dplyr::between(as_num(dist_walk), 99, 999999), NA_real_, as_num(dist_walk)),
      diabetes = dplyr::case_when(as_num(diabetes) == 9 ~ NA_real_, as_num(diabetes) == 2 ~ 0, TRUE ~ as_num(diabetes)),
      hyp_tens = dplyr::case_when(as_num(hyp_tens) == 9 ~ NA_real_, as_num(hyp_tens) == 2 ~ 0, TRUE ~ as_num(hyp_tens))
    ) |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # Section 10 - Alcohol & tobacco
  # ---------------------------------------------------------------------------
  bt <- read_dta_safe(baseline_raw("s1005")) |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::inner_join(b_p_sample, by = "pid", suffix = c("", ".sample")) |>
    dplyr::select(dplyr::matches("^s10"), id_1, pid, etapa, linea) |>
    dplyr::select(-dplyr::any_of("s1021"), -dplyr::matches("resp$"))
  ft <- read_dta_safe(followup_raw("s1002_2.dta")) |>
    dplyr::inner_join(f_p_sample, by = "cve_res", suffix = c("", ".sample")) |>
    dplyr::select(dplyr::matches("^s10"), dplyr::any_of(c("cve_viv", "cve_res", "linea", "pid", "id_1", "etapa", "tapoyo", "tipo_ind")))
  tobacco <- dplyr::bind_rows(ft, bt)
  tobacco <- rename_existing(tobacco, c(s101 = "smoke", s102 = "n_smoke", s103 = "drink", s104 = "n_drink_sf", s105 = "n_drink_hd"))
  tobacco <- tobacco |>
    dplyr::mutate(
      smoke = recode_binary_2_0(smoke), drink = recode_binary_2_0(drink),
      n_smoke = dplyr::na_if(as_num(n_smoke), 99),
      n_drink_sf = dplyr::na_if(as_num(n_drink_sf), 99),
      n_drink_hd = dplyr::na_if(as_num(n_drink_hd), 99),
      n_drink = n_drink_sf + n_drink_hd,
      n_drink = dplyr::if_else(n_drink %in% c(110, 134, 196), 98, n_drink)
    ) |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # Section 11 - Anthropometrics
  # ---------------------------------------------------------------------------
  ba <- dplyr::bind_rows(read_dta_safe(baseline_raw("s1105_mujeres.dta")), read_dta_safe(baseline_raw("s1105ninios.dta"))) |>
    dplyr::mutate(pid = paste0(id_1, linea)) |>
    dplyr::inner_join(b_p_sample, by = "pid", suffix = c("", ".sample")) |>
    dplyr::select(dplyr::any_of(c("s112a", "s112b", "s113", "s114", "s115", "s116", "s117", "id_1", "pid", "etapa", "linea")))
  ba <- rename_existing(ba, c(s112a = "s111", s112b = "s112"))
  fa <- dplyr::bind_rows(read_dta_safe(followup_raw("s1102_2_mujeres")), read_dta_safe(followup_raw("s1102_2_ninios.dta"))) |>
    dplyr::inner_join(f_p_sample, by = "cve_res", suffix = c("", ".sample"))
  for (nm in grep("^s11.*_2$", names(fa), value = TRUE)) names(fa)[names(fa) == nm] <- sub("_2$", "", nm)
  fa <- fa |>
    dplyr::select(dplyr::matches("^s11"), dplyr::any_of(c("cve_viv", "cve_res", "linea", "pid", "id_1", "etapa", "tipo_ind")))
  anthro <- dplyr::bind_rows(fa, ba) |>
    dplyr::mutate(
      s111 = dplyr::if_else(dplyr::between(as_num(s111), 222.1, 222.3), NA_real_, as_num(s111)),
      s112 = dplyr::if_else(as_num(s112) == 222.2, NA_real_, as_num(s112)),
      peso = rowMeans(cbind(s111, s112), na.rm = TRUE),
      peso = dplyr::if_else(is.nan(peso), NA_real_, peso),
      height = dplyr::if_else(as_num(s114) == 222.2, NA_real_, as_num(s114)),
      emb_pecho = dplyr::case_when(as_num(s116) %in% c(-9, 9, 0) ~ 4, TRUE ~ as_num(s116)),
      emb = as.integer(emb_pecho %in% c(1, 3))
    ) |>
    dplyr::select(dplyr::any_of(c("peso", "height", "emb", "emb_pecho", "cve_viv", "cve_res", "etapa", "id_1", "pid", "tipo_ind"))) |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # 24-hour food recall for mothers and young children
  # ---------------------------------------------------------------------------
  br <- dplyr::bind_rows(read_dta_safe(baseline_raw("sr2406_mujer")), read_dta_safe(baseline_raw("sr2406_ninios"))) |>
    dplyr::mutate(pid = paste0(id_1, ident)) |>
    dplyr::inner_join(b_p_sample, by = "pid", suffix = c("", ".sample"))
  br <- drop_existing(br, c("entidad", "locali", "ident", "eanos", "edadm", "edadmes", "sexo", "e_fisio", "e_fisio2", "edadcat", "tipo_r", "tipo_c", "tipoloc", "ef", "lactan"))
  br <- dplyr::select(br, -dplyr::starts_with("teta"), -dplyr::any_of("._merge"))
  
  fr <- dplyr::bind_rows(read_dta_safe(followup_raw("s1402_mujeres")), read_dta_safe(followup_raw("s1402_ninios"))) |>
    dplyr::inner_join(f_p_sample, by = "cve_res", suffix = c("", ".sample"))
  fr <- drop_existing(fr, c("ident", "sexo", "ef", "entidad", "edad_cat", "tipo_c", "idloc", "tapoyo"))
  fr <- dplyr::select(fr, -dplyr::starts_with("tx_"), -dplyr::any_of("._merge"))
  if (all(c("c_adecu", "edad_esc") %in% names(fr))) fr <- dplyr::select(fr, -(c_adecu:edad_esc))
  
  recall <- dplyr::bind_rows(fr, br)
  recall <- rename_existing(recall, c(energia = "cal", enfermo = "sick_ayer", proteina = "prot", folato = "folate", vit_c = "vitc", v_b12 = "vitb12", vit_a = "vita"))
  recall$edad <- floor(as_num(recall$edada))
  for (a in 0:6) {
    idx <- which(recall$edad == a & !is.na(recall$cal))
    if (length(idx)) {
      lo <- stata_pctile(as_num(recall$cal[idx]), 1); hi <- stata_pctile(as_num(recall$cal[idx]), 99)
      recall$cal[idx[as_num(recall$cal[idx]) < lo | as_num(recall$cal[idx]) > hi]] <- NA_real_
    }
  }
  recall <- recall |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # Blood samples (follow-up only)
  # ---------------------------------------------------------------------------
  anemia <- read_dta_safe(followup_raw("s1602_2_anemia")) |>
    dplyr::select(dplyr::matches("^cve"), linea, dplyr::matches("^s16|^hb|^pc|^anemia"))
  gluc <- read_dta_safe(followup_raw("s1602_2_glucosa")) |>
    dplyr::select(dplyr::matches("^cve"), linea, dplyr::matches("^s16|^ayuno|^diab"))
  blood <- stata_merge(gluc, anemia, by = "cve_res")
  blood$._merge <- NULL
  blood$etapa <- 2
  hb_cols <- grep("^hb_adj_", names(blood), value = TRUE)
  an_cols <- grep("^anemia_", names(blood), value = TRUE)
  blood$hb_adj <- row_total(blood, hb_cols)
  blood$hb_adj[row_missing(blood, hb_cols) == length(hb_cols)] <- NA_real_
  blood$anemia <- row_total(blood, an_cols)
  blood$anemia[row_missing(blood, an_cols) == length(an_cols)] <- NA_real_
  blood <- drop_existing(blood, c("hb"))
  blood <- dplyr::select(blood, -dplyr::matches("^s16(1|2|3|4|5).*_2$|^diab_ayuno|^hb_adj_[pem]$|^pc_adj_[pem]$|^anemia_[pem]$"))
  blood <- rename_existing(blood, c(s166_2 = "glucose", s167_2 = "hb", ayuno2 = "t_fast", ayuno9 = "d_fast"))
  blood <- dplyr::inner_join(blood, f_p_sample, by = "cve_res", suffix = c("", ".sample")) |>
    dplyr::arrange(pid, etapa)
  
  # ---------------------------------------------------------------------------
  # Merge individual modules
  # ---------------------------------------------------------------------------
  individual <- personal
  for (mod in list(health, tobacco, anthro, recall, blood)) {
    m <- stata_merge(individual, mod, by = c("pid", "etapa"), update = FALSE)
    m$._merge <- NULL
    individual <- m
  }
  individual <- fill_within(individual, "id_1", intersect(c("cve_viv"), names(individual)), order = "etapa")
  idx <- true_idx(!is.na(individual$cve_viv) & individual$cve_viv == "")
  individual$cve_viv[idx] <- individual$id_1[idx]
  individual <- fill_within(individual, "pid", intersect(c("cve_res"), names(individual)), order = "etapa")
  idx <- true_idx(!is.na(individual$cve_res) & individual$cve_res == "")
  individual$cve_res[idx] <- individual$pid[idx]
  
  # PAL recipient line comes from household transfer data constructed in analysis.do.
  if (!is.null(transfers) && all(c("cve_viv", "etapa", "pal_bene_linea") %in% names(transfers))) {
    pal <- transfers |>
      dplyr::select(cve_viv, etapa, pal_bene_linea) |>
      dplyr::filter(etapa == 2, dplyr::between(as_num(pal_bene_linea), 1, 13))
    individual <- stata_merge(pal, individual, by = c("cve_viv", "etapa"), update = FALSE)
    individual$._merge <- NULL
    individual <- individual |>
      dplyr::mutate(
        pal_bene_fem_i = dplyr::case_when(
          etapa == 2 & as_num(pal_bene_linea) == as_num(linea) & male == 0 ~ 1,
          etapa == 2 & as_num(pal_bene_linea) == as_num(linea) & male == 1 ~ 0,
          TRUE ~ NA_real_
        )
      ) |>
      dplyr::group_by(cve_viv) |>
      dplyr::mutate(pal_bene_fem = if (all(is.na(pal_bene_fem_i))) NA_real_ else sum(pal_bene_fem_i, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::select(-pal_bene_fem_i)
    individual <- fill_within(individual, "cve_viv", "pal_bene_fem", order = "etapa")
  }
  
  # Drop new household members older than two, as in individual.do.
  if ("tipo_ind" %in% names(individual)) {
    drop_new_old <- as_num(individual$tipo_ind) %in% c(2, 3) &
      !is.na(individual$age) & dplyr::between(individual$age, 3, 98)
    individual <- individual[!drop_new_old, , drop = FALSE] |>
      dplyr::select(-tipo_ind)
  }
  
  individual <- individual |>
    dplyr::mutate(p = as.integer(factor(cve_res, levels = unique(cve_res)))) |>
    dplyr::arrange(cve_res, etapa)
  
  # individual.do does not impose uniqueness of pid + etapa before saving.
  # Legacy Stata merge permits duplicate person-wave keys and pairs them
  # sequentially; keep those observations and record duplicates diagnostically.
  checkpoint(
    "06_individual",
    individual,
    key = c("pid", "etapa"),
    both_waves = TRUE,
    require_unique = FALSE
  )
  
  if (save) save_processed(individual, "individual")
  individual
}