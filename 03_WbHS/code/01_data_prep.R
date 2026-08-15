# Translate 01_DataPrep.do: construct the analysis dataset from six raw source files.

build_wbhs_dataset <- function(root) {
  raw_dir <- file.path(root, "data", "source", "raw")
  processed_dir <- ensure_dir(file.path(root, "data", "processed"))

  files <- file.path(raw_dir, c(
    "random_anon.dta", "BL_anon.dta", "PICTURES_anon.dta",
    "EXAM_anon.dta", "FU1_anon.dta", "FU2_anon.dta"
  ))
  require_files(files)

  random <- read_stata(files[1])
  bl <- read_stata(files[2])
  pics <- read_stata(files[3])
  exam <- read_stata(files[4])
  fu1 <- read_stata(files[5])
  fu2 <- read_stata(files[6])

  id <- "ID_Participante"
  x <- random
  x <- stata_merge_1to1(x, bl, id)$data
  x <- stata_merge_1to1(x, pics, id)$data

  m <- stata_merge_1to1(x, exam, id)
  x <- m$data
  x$Encuestado_EXAM <- as.numeric(m$merge == 3L)

  m <- stata_merge_1to1(x, fu1, id)
  x <- m$data
  x$Encuestado_SEG1 <- as.numeric(m$merge == 3L)

  m <- stata_merge_1to1(x, fu2, id)
  x <- m$data
  x$Encuestado_SEG2 <- as.numeric(m$merge == 3L)

  x <- x[!is.na(x[[id]]) & as.character(x[[id]]) != "." & as.character(x[[id]]) != "", , drop = FALSE]

  # Rosenberg score: baseline.
  require_cols(x, c(
    "lb_d3_autoestima", "lb_d4_autoestima", "lb_d5_autoestima", "lb_d6_autoestima",
    "lb_d7_autoestima", "lb_d8_autoestima", "lb_d9_autoestima", "lb_d10_autoestima",
    "lb_d11_autoestima", "lb_d12_autoestima"
  ), "merged WBHS data")
  for (v in c("lb_d3_autoestima", "lb_d8_autoestima")) x[[v]][x[[v]] == 99] <- 1
  for (v in c("lb_d3_autoestima", "lb_d4_autoestima", "lb_d6_autoestima", "lb_d8_autoestima", "lb_d9_autoestima")) {
    x[[paste0(v, "_aux")]] <- x[[v]] - 1
  }
  for (v in c("lb_d5_autoestima", "lb_d7_autoestima", "lb_d10_autoestima", "lb_d11_autoestima", "lb_d12_autoestima")) {
    z <- x[[v]]
    x[[paste0(v, "_aux")]] <- dplyr::case_when(
      z == 4 ~ 0, z == 3 ~ 1, z == 2 ~ 2, z == 1 ~ 3, TRUE ~ as.numeric(z)
    )
  }
  x$lb_rosenberg_score <- additive_sum(x, paste0(c(
    "lb_d3_autoestima", "lb_d4_autoestima", "lb_d5_autoestima", "lb_d6_autoestima",
    "lb_d7_autoestima", "lb_d8_autoestima", "lb_d9_autoestima", "lb_d10_autoestima",
    "lb_d11_autoestima", "lb_d12_autoestima"
  ), "_aux"))

  # Rosenberg score: first follow-up. Note that the original sum uses c3-c12 (ten items).
  for (v in c("c1_autoestima", "c3_autoestima", "c4_autoestima", "c6_autoestima", "c8_autoestima", "c9_autoestima")) {
    require_cols(x, v)
    z <- x[[v]]
    x[[paste0(v, "_aux")]] <- dplyr::case_when(
      z == 4 ~ 0, z == 3 ~ 1, z == 2 ~ 2, z == 1 ~ 3, TRUE ~ z - 1
    )
  }
  for (v in c("c2_autoestima", "c5_autoestima", "c7_autoestima", "c10_autoestima", "c11_autoestima", "c12_autoestima")) {
    require_cols(x, v)
    x[[paste0(v, "_aux")]] <- x[[v]] - 1
  }
  x$seg_rosenberg_score <- additive_sum(x, paste0(c(
    "c3_autoestima", "c4_autoestima", "c5_autoestima", "c6_autoestima", "c7_autoestima",
    "c8_autoestima", "c9_autoestima", "c10_autoestima", "c11_autoestima", "c12_autoestima"
  ), "_aux"))

  # Rosenberg score: second follow-up. The Stata index uses b1-b10.
  for (v in c("SEG2_b3", "SEG2_b8", "SEG2_b9", "SEG2_b10", "SEG2_b12")) {
    require_cols(x, v); x[[paste0(v, "_aux")]] <- x[[v]] - 1
  }
  for (v in c("SEG2_b1", "SEG2_b2", "SEG2_b4", "SEG2_b5", "SEG2_b6", "SEG2_b7", "SEG2_b11")) {
    require_cols(x, v)
    z <- x[[v]]
    x[[paste0(v, "_aux")]] <- dplyr::case_when(
      z == 4 ~ 0, z == 3 ~ 1, z == 2 ~ 2, z == 1 ~ 3, TRUE ~ z - 1
    )
  }
  x$SEG2_rosenberg_score <- additive_sum(x, paste0(paste0("SEG2_b", 1:10), "_aux"))

  c_rosen <- safe_mean(x$seg_rosenberg_score[x$treatment == 0])
  d_rosen <- safe_sd(x$seg_rosenberg_score[x$treatment == 0])
  x$c_seg_rosenberg_score <- c_rosen
  x$d_seg_rosenberg_score <- d_rosen
  x$new_sd_rosen0 <- (x$lb_rosenberg_score - c_rosen) / d_rosen
  x$new_sd_rosen1 <- (x$seg_rosenberg_score - c_rosen) / d_rosen
  x$new_sd_rosen2 <- (x$SEG2_rosenberg_score - c_rosen) / d_rosen

  # OHIP.
  for (v in paste0("lb_d", 12:13, "_ohip")) { require_cols(x, v); x[[v]][x[[v]] == 99] <- 1 }
  for (v in paste0("lb_d", 1:15, "_ohip")) { require_cols(x, v); x[[paste0(v, "_aux")]] <- x[[v]] - 1 }
  x$lb_ohip14 <- stata_row_sum(x, paste0("lb_d", 1:14, "_ohip_aux"))

  for (v in paste0("g", 1:15, "_ohip")) { require_cols(x, v); x[[paste0(v, "_aux")]] <- x[[v]] - 1 }
  x$seg_ohip14 <- ifelse(x$Encuestado_SEG1 == 1, stata_row_sum(x, paste0("g", 1:14, "_ohip_aux")), NA_real_)

  for (v in paste0("SEG2_f", 1:14)) { require_cols(x, v); x[[paste0(v, "_aux")]] <- x[[v]] - 1 }
  x$SEG2_OHIP14 <- ifelse(x$Encuestado_SEG2 == 1, stata_row_sum(x, paste0("SEG2_f", 1:14, "_aux")), NA_real_)

  c_ohip <- safe_mean(x$seg_ohip14[x$treatment == 0])
  d_ohip <- safe_sd(x$seg_ohip14[x$treatment == 0])
  x$c_seg_ohip14 <- c_ohip; x$d_seg_ohip14 <- d_ohip
  x$new_sd_ohip0 <- -(x$lb_ohip14 - c_ohip) / d_ohip
  x$new_sd_ohip1 <- -(x$seg_ohip14 - c_ohip) / d_ohip
  x$new_sd_ohip2 <- -(x$SEG2_OHIP14 - c_ohip) / d_ohip

  # Employment and labor income.
  require_cols(x, c("lb_trabajo_ingreso", "b1", "b2", "b3", "income_work", "b27"))
  x$ltotal_bl <- ifelse(!is.na(x$lb_trabajo_ingreso) & !x$lb_trabajo_ingreso %in% c(77, 88), log(x$lb_trabajo_ingreso), NA_real_)
  x$employed <- ifelse(x$Encuestado_SEG1 == 1, 0, NA_real_)
  x$employed[(!is.na(x$b1) & x$b1 == 1) | (!is.na(x$b2) & x$b2 == 1) | (!is.na(x$b3) & x$b3 == 1)] <- 1
  x$income1 <- x$income_work
  x$income1[x$employed == 0] <- 0
  x$income1[!is.na(x$income_work) & x$income_work == 0 & x$employed == 1] <- NA_real_
  x$income_extra <- NA_real_
  x$income_extra[x$Encuestado_SEG1 == 1 & !is.na(x$income1)] <- 0
  ok_extra <- !is.na(x$b27) & x$b27 > 0 & x$b27 != 99
  x$income_extra[ok_extra] <- x$b27[ok_extra]
  x$income_total <- ifelse(x$Encuestado_SEG1 == 1, x$income1 + x$income_extra, NA_real_)
  x$ltotal <- log(x$income_total + 1)

  seg2_emp_vars <- c("SEG2_a1", paste0("SEG2_a2_", 1:7), "SEG2_a3")
  require_cols(x, c(seg2_emp_vars, "SEG2_a30", "SEG2_income_work_ppal"))
  x$SEG2_employed <- ifelse(x$Encuestado_SEG2 == 1, 0, NA_real_)
  any_emp <- Reduce(`|`, lapply(seg2_emp_vars, function(v) !is.na(x[[v]]) & x[[v]] == 1))
  x$SEG2_employed[any_emp] <- 1
  x$SEG2_income_work_sec <- x$SEG2_a30
  x$SEG2_income_work_sec[is.na(x$SEG2_income_work_sec) & x$Encuestado_SEG2 == 1] <- 0
  x$SEG2_income_work_total <- x$SEG2_income_work_ppal + x$SEG2_income_work_sec
  x$ltotal_2 <- log(x$SEG2_income_work_total + 1)

  # Ratings of photographs.
  pic_groups <- list(cs = paste0("cs", 1:3, "_mea"), hi = paste0("hi", 1:3, "_mea"),
                     ap = paste0("ap", 1:3, "_mea"), sr = paste0("sr", 1:3, "_mea"))
  for (nm in names(pic_groups)) {
    require_cols(x, pic_groups[[nm]])
    x[[paste0(nm, "4_mean")]] <- additive_sum(x, pic_groups[[nm]]) / 3
  }
  x$index_photos <- (x$cs4_mean + x$hi4_mean + x$ap4_mean + x$sr4_mean) / 4
  for (v in c("cs4_mean", "hi4_mean", "ap4_mean", "sr4_mean")) {
    mu <- safe_mean(x[[v]][x$treatment == 0]); sig <- safe_sd(x[[v]][x$treatment == 0])
    x[[paste0("c_", v)]] <- mu; x[[paste0("d_", v)]] <- sig
    x[[paste0("sd_", v)]] <- (x[[v]] - mu) / sig
  }
  mu <- safe_mean(x$index_photos[x$treatment == 0]); sig <- safe_sd(x$index_photos[x$treatment == 0])
  x$c_index_photos <- mu; x$d_index_photos <- sig
  x$sd_index_photos <- (x$index_photos - mu) / sig
  require_cols(x, "ev2_mean"); x$smiles <- 1 - x$ev2_mean

  # Objective dental health: reproduce the tooth renaming map explicitly.
  crown_map <- c(setNames(18:11, paste0("form_ed_", 24:31)),
                 setNames(21:28, paste0("form_ed_", 32:39)),
                 setNames(48:41, paste0("form_ed_", 72:79)),
                 setNames(31:38, paste0("form_ed_", 80:87)))
  treat_map <- c(setNames(18:11, paste0("form_ed_", 56:63)),
                 setNames(21:28, paste0("form_ed_", 64:71)),
                 setNames(48:41, paste0("form_ed_", 104:111)),
                 setNames(31:38, paste0("form_ed_", 112:119)))
  require_cols(x, c(names(crown_map), names(treat_map)))
  for (old in names(crown_map)) names(x)[names(x) == old] <- paste0("corona_", crown_map[[old]])
  for (old in names(treat_map)) names(x)[names(x) == old] <- paste0("tratamiento_", treat_map[[old]])

  tooth_ids <- c(11:18, 21:28, 31:38, 41:48)
  for (i in tooth_ids) {
    cv <- paste0("corona_", i); tv <- paste0("tratamiento_", i)
    require_cols(x, c(cv, tv))
    car <- ifelse(x$Encuestado_EXAM == 1, 0, NA_real_)
    car[x$Encuestado_EXAM == 1 & !is.na(x[[cv]]) & as.character(x[[cv]]) == "1"] <- 1
    x[[paste0("aux_", cv, "_car")]] <- car
    tr <- ifelse(x$Encuestado_EXAM == 1, 0, NA_real_)
    tr[x$Encuestado_EXAM == 1 & !is.na(x[[tv]]) & as.character(x[[tv]]) == "0"] <- 1
    x[[paste0("aux_", i, "_trat")]] <- tr
  }
  x$seg_indice_cariados <- ifelse(x$Encuestado_EXAM == 1,
    stata_row_sum(x, paste0("aux_corona_", tooth_ids, "_car")), NA_real_)
  x$seg_indice_sintrat <- ifelse(x$Encuestado_EXAM == 1,
    stata_row_sum(x, paste0("aux_", tooth_ids, "_trat")), NA_real_)
  x$needtreat <- 32 - x$seg_indice_sintrat

  require_cols(x, c("e9", "f1", "SEG2_h7", "form_ep_120", "form_ep_121", "SEG2_i1", "SEG2_i5_1"))
  x$completed <- ifelse(!is.na(x$treatment), 0, NA_real_)
  x$completed[!is.na(x$e9) & x$e9 == 1] <- 1
  x$dental_service <- 0
  x$dental_service[!is.na(x$e9) & x$e9 == 1] <- 1
  x$dental_service[!is.na(x$f1) & x$f1 == 1 & x$treatment == 0] <- 1
  x$dental_serviceFU2 <- ifelse(x$Encuestado_SEG2 == 1, 0, NA_real_)
  x$dental_serviceFU2[x$Encuestado_SEG2 == 1 & !is.na(x$SEG2_h7) & x$SEG2_h7 <= 5] <- 1
  x$dental_serviceFU2 <- ifelse(is.na(x$dental_serviceFU2), x$dental_service,
                                pmax(x$dental_service, x$dental_serviceFU2, na.rm = TRUE))

  x$Sin_protesis_Sup_SEG1 <- ifelse(!is.na(x$form_ep_120), 0, NA_real_)
  x$Sin_protesis_Sup_SEG1[!is.na(x$form_ep_120) & x$form_ep_120 == 0] <- 1
  x$Sin_protesis_Inf_SEG1 <- ifelse(!is.na(x$form_ep_121), 0, NA_real_)
  x$Sin_protesis_Inf_SEG1[!is.na(x$form_ep_121) & x$form_ep_121 == 0] <- 1
  x$Con_protesis_Sup_SEG1 <- ifelse(is.na(x$Sin_protesis_Sup_SEG1), NA_real_, 1 - x$Sin_protesis_Sup_SEG1)
  x$Con_protesis_Inf_SEG1 <- ifelse(is.na(x$Sin_protesis_Inf_SEG1), NA_real_, 1 - x$Sin_protesis_Inf_SEG1)
  x$Protesis_SEG1 <- ifelse(!is.na(x$Con_protesis_Inf_SEG1) | !is.na(x$Con_protesis_Sup_SEG1), 0, NA_real_)
  x$Protesis_SEG1[x$Con_protesis_Inf_SEG1 == 1 | x$Con_protesis_Sup_SEG1 == 1] <- 1
  x$Protesis_SEG2 <- x$SEG2_i1
  x$lb_protesis <- ifelse(x$Encuestado_SEG2 == 1, 0, NA_real_)
  x$lb_protesis[not_blank(x$SEG2_i5_1) & x$Encuestado_SEG2 == 1] <- 1
  x$kkk1 <- x$Protesis_SEG1; x$kkk1[x$lb_protesis == 1] <- 0
  x$kkk2 <- x$Protesis_SEG2; x$kkk2[x$lb_protesis == 1] <- 0
  x$prosthesis1 <- x$kkk1; x$prosthesis2 <- x$kkk2
  x$FirstFU <- x$Encuestado_SEG1; x$SecondFU <- x$Encuestado_SEG2

  # Interactions with partner.
  require_cols(x, c("SEG2_d8", paste0("SEG2_d8_", 1:7)))
  for (i in 1:7) {
    z <- ifelse(not_blank(x$SEG2_d8), 0, NA_real_)
    z[!is.na(x[[paste0("SEG2_d8_", i)]]) & x[[paste0("SEG2_d8_", i)]] == 1] <- 1
    x[[paste0("Qd_act_", i)]] <- z
  }
  for (i in 4:7) x[[paste0("Qnd_act_", i)]] <- 1 - x[[paste0("Qd_act_", i)]]
  x$index_interactions <- additive_sum(x, c(paste0("Qd_act_", 1:3), paste0("Qnd_act_", 4:7)))

  effort_vars <- paste0("SEG2_e1_", 1:9)
  require_cols(x, effort_vars)
  x$efforts <- additive_sum(x, effort_vars)

  # SF-12 scoring exactly as coded in Stata.
  score_map <- function(z, map) {
    out <- ifelse(!is.na(z), 0, NA_real_)
    for (k in names(map)) out[!is.na(z) & z == as.numeric(k)] <- unname(map[[k]])
    out
  }
  maps <- list(
    ind1 = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind2a = c(`1`=0,`2`=50,`3`=100), ind2b = c(`1`=0,`2`=50,`3`=100),
    ind3a = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind3b = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind4a = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind4b = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind5 = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0,`6`=100),
    ind6a = c(`1`=0,`2`=25,`3`=50,`4`=50,`5`=100),
    ind6b = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0),
    ind7 = c(`1`=0,`2`=25,`3`=50,`4`=50,`5`=100),
    ind8 = c(`1`=100,`2`=75,`3`=50,`4`=25,`5`=0)
  )
  sf_vars <- c(ind1="SEG2_sf1", ind2a="SEG2_sf2a", ind2b="SEG2_sf2b", ind3a="SEG2_sf3a", ind3b="SEG2_sf3b",
               ind4a="SEG2_sf4a", ind4b="SEG2_sf4b", ind5="SEG2_sf5", ind6a="SEG2_sf6a", ind6b="SEG2_sf6b",
               ind7="SEG2_sf7", ind8="SEG2_sf8")
  require_cols(x, unname(sf_vars))
  for (nm in names(sf_vars)) x[[nm]] <- score_map(x[[sf_vars[[nm]]]], maps[[nm]])
  x$sf12_ph <- stata_row_mean(x, c("ind1","ind2a","ind2b","ind3a","ind3b","ind5"))
  x$sf12_mh <- stata_row_mean(x, c("ind4a","ind4b","ind6a","ind6b","ind7","ind8"))
  mu <- safe_mean(x$sf12_ph[x$treatment == 0]); sig <- safe_sd(x$sf12_ph[x$treatment == 0])
  x$c_sf12_ph <- mu; x$d_sf12_ph <- sig; x$sf12_ph_sd <- (x$sf12_ph - mu) / sig
  mu <- safe_mean(x$sf12_mh[x$treatment == 0]); sig <- safe_sd(x$sf12_mh[x$treatment == 0])
  x$c_sf12_mh <- mu; x$d_sf12_mh <- sig; x$sf12_mh_sd <- (x$sf12_mh - mu) / sig

  # Control variables.
  require_cols(x, c("lb_residentes_adulto_mayor", "lb_fecha_de_nacimiento", "lb_employ", "lb_trabajo_contrato",
                    "lb_dientes_faltantes_totales", "lb_dientes_faltantes_frontales"))
  x$ad_mayor <- x$lb_residentes_adulto_mayor
  x$edad <- 2013 - stata_year(x$lb_fecha_de_nacimiento)
  x$lb_ingreso_th <- x$lb_trabajo_ingreso / 1000
  x$lb_cont_formal <- ifelse(!is.na(x$lb_employ), 0, NA_real_)
  x$lb_cont_formal[x$lb_employ == 1 & x$lb_trabajo_contrato == 1] <- 1
  x$lb_cont_informal <- ifelse(!is.na(x$lb_employ), 0, NA_real_)
  x$lb_cont_informal[x$lb_employ == 1 & x$lb_cont_formal == 0] <- 1
  x$lb_dientes_falt_tot <- x$lb_dientes_faltantes_totales
  x$lb_dientes_falt_front <- x$lb_dientes_faltantes_frontales
  x$sd_ohip0 <- x$new_sd_ohip0; x$sd_ohip1 <- x$new_sd_ohip1; x$sd_ohip2 <- x$new_sd_ohip2
  x$sd_rosen0 <- x$new_sd_rosen0; x$sd_rosen1 <- x$new_sd_rosen1; x$sd_rosen2 <- x$new_sd_rosen2

  saveRDS(x, file.path(processed_dir, "main_dataset.rds"))
  haven::write_dta(x, file.path(processed_dir, "main_dataset.dta"), version = 14)

  diag <- data.frame(
    file = basename(files), rows = c(nrow(random), nrow(bl), nrow(pics), nrow(exam), nrow(fu1), nrow(fu2)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(diag, file.path(root, "output", "diagnostics", "01_source_row_counts.csv"), row.names = FALSE)
  message("Built data/processed/main_dataset.rds (", nrow(x), " rows, ", ncol(x), " columns).")
  invisible(x)
}
