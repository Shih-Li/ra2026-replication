# 05_build_household_modules.R
# Household raw-data cleaning modules corresponding to Sections 1, 4-9, 13,
# and the household food-recall sections in analysis*.do.
#
# Every function reads only from data/source/raw and returns an R-generated
# intermediate object. No source file is modified.

food_codes <- c(101:117, 201:216, 301:312, 401:416)

carry_fu_to_baseline <- function(df, group = "id_1", cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(df)
  
  # Stata string missing is ""; numeric missing is NA.
  # Use byte length instead of trimws()/regex so strings with legacy
  # non-UTF-8 bytes (e.g. pickup_s) can be carried without conversion.
  # This test does not modify or re-encode the underlying survey text.
  is_stata_missing <- function(v) {
    if (!is.character(v)) return(is.na(v))
    
    n_bytes <- nchar(v, type = "bytes", allowNA = TRUE, keepNA = TRUE)
    is.na(v) | (!is.na(n_bytes) & n_bytes == 0L)
  }
  
  df <- dplyr::arrange(df, .data[[group]], etapa)
  df <- dplyr::group_by(df, .data[[group]])
  df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), ~ {
    v <- .x
    miss <- is_stata_missing(v)
    fu_pos <- which(etapa == 2 & !miss)
    
    if (length(fu_pos)) {
      fu <- v[fu_pos[[1]]]
      idx <- etapa == 1 & miss
      idx[is.na(idx)] <- FALSE
      v[idx] <- fu
    }
    v
  }))
  dplyr::ungroup(df)
}

# Warning-free equivalent of Stata's rowmax(): all-missing rows stay missing.

row_max_no_warn <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  z <- as.data.frame(lapply(df[cols], as_num))
  ok <- rowSums(!is.na(z)) > 0
  out <- rep(NA_real_, nrow(z))
  if (any(ok)) out[ok] <- apply(z[ok, , drop = FALSE], 1, max, na.rm = TRUE)
  out
}

# Collapse redundant locality-level rows before attaching them to household data.
#
# Legacy Stata `merge key using ...` is not a Cartesian many-to-many join. For
# the locality support files used here, the intended data structure is one
# record per merge key. We therefore collapse duplicate keys only when their
# non-missing information is mutually consistent. If two duplicate rows carry
# conflicting non-missing values, stop and report the source/key rather than
# silently multiplying household observations.
collapse_consistent_key <- function(df, key, stage) {
  missing_key <- setdiff(key, names(df))
  if (length(missing_key)) {
    stop(
      stage, ": missing key column(s): ",
      paste(missing_key, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(df)) return(df)
  
  key_txt <- lapply(df[key], function(v) {
    z <- as.character(v)
    z[stata_missing(v)] <- "<STATA_MISSING>"
    z
  })
  sig <- do.call(paste, c(key_txt, sep = "\r"))
  groups <- split(seq_len(nrow(df)), sig, drop = TRUE)
  
  if (all(lengths(groups) == 1L)) return(df)
  
  nonkey <- setdiff(names(df), key)
  out <- vector("list", length(groups))
  j <- 0L
  
  for (ii in groups) {
    j <- j + 1L
    row <- df[ii[[1]], , drop = FALSE]
    
    if (length(ii) > 1L && length(nonkey)) {
      for (nm in nonkey) {
        v <- df[[nm]][ii]
        good <- which(!stata_missing(v))
        if (!length(good)) next
        
        vals <- v[good]
        # Compare through character representation only for consistency
        # checking; assignment below preserves the original vector type.
        u <- unique(as.character(vals))
        if (length(u) > 1L) {
          key_desc <- paste(
            paste0(key, "=", vapply(row[key], as.character, character(1))),
            collapse = ", "
          )
          stop(
            stage, ": conflicting duplicate locality records (",
            key_desc, ") in column `", nm, "`. Values: ",
            paste(utils::head(u, 6), collapse = " | "),
            call. = FALSE
          )
        }
        row[[nm]] <- vals[[1]]
      }
    }
    out[[j]] <- row
  }
  
  dplyr::bind_rows(out)
}

build_location <- function(b_hh, f_hh) {
  # -------------------------------------------------------------------------
  # Baseline location/interview month
  # -------------------------------------------------------------------------
  b <- read_dta_safe(baseline_raw("s0105")) |>
    dplyr::select(id_1, mm1, entidad) |>
    dplyr::inner_join(
      b_hh,
      by = "id_1",
      suffix = c("", ".sample"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      b_mm_ent = dplyr::case_when(
        dplyr::between(as_num(mm1), 0, 1) ~ 1,
        dplyr::between(as_num(mm1), 2, 5) ~ 3,
        dplyr::between(as_num(mm1), 12, 15) ~ 12,
        TRUE ~ NA_real_
      ),
      b_mm_ent_org = as_num(mm1)
    ) |>
    dplyr::select(-mm1)
  
  assert_unique_key(b, c("id_1", "etapa"), "location_baseline")
  
  # -------------------------------------------------------------------------
  # Follow-up location rows. f_hh is already unique by cve_viv and id_1.
  # Original Stata: merge cve_viv using f_hh_sample, update replace;
  #                 drop if _m==1
  # -------------------------------------------------------------------------
  fraw <- read_dta_safe(followup_raw("s102_2"))
  geo_cols <- grep("^(lati..|long..)$", names(fraw), value = TRUE)
  fu0 <- fraw |>
    dplyr::select(dplyr::any_of(c("id_1", "etapa", "cve_viv", geo_cols)))
  
  # Keep only raw follow-up rows that are in the empirical f_hh sample. Using
  # sample values take precedence for overlapping identifiers, matching
  # Stata's `merge, update replace` here without invoking a many-to-many join.
  fu <- fu0 |>
    dplyr::inner_join(
      f_hh,
      by = "cve_viv",
      suffix = c(".raw", ""),
      relationship = "one-to-one"
    )
  
  # Resolve the only overlaps intentionally: id_1 and etapa from f_hh_sample.
  if ("id_1.raw" %in% names(fu)) fu$id_1.raw <- NULL
  if ("etapa.raw" %in% names(fu)) fu$etapa.raw <- NULL
  if ("entidad" %in% names(fu)) fu$entidad <- as.character(fu$entidad)
  
  assert_unique_key(fu, c("id_1", "etapa"), "location_followup")
  
  # Stata: append using baseline temp, then fill latitude/longitude/b_mm_ent
  # within id_1 across waves.
  loc <- dplyr::bind_rows(fu, b)
  fill_cols <- grep("^(lat|lon)|^b_mm_ent$", names(loc), value = TRUE)
  loc <- fill_within(loc, "id_1", fill_cols, order = "etapa")
  assert_unique_key(loc, c("id_1", "etapa"), "location_after_append")
  
  # -------------------------------------------------------------------------
  # Locality support files. Each is made unambiguous on its Stata merge key
  # before being attached to household rows. This prevents a locality-level
  # duplicate from multiplying every household in that locality.
  # -------------------------------------------------------------------------
  inegi <- read_dta_safe(raw_support("inegi_geodata.dta")) |>
    collapse_consistent_key("id_loc", "inegi_geodata")
  loc <- stata_merge(loc, inegi, by = "id_loc") |>
    dplyr::filter(._merge != 2) |>
    dplyr::select(-._merge) |>
    dplyr::mutate(
      loc_alt_ln = log(as_num(loc_alt)),
      loc_pop2 = as_num(loc_pop)^2,
      km_to_munhead2 = as_num(km_to_munhead)^2
    )
  assert_unique_key(loc, c("id_1", "etapa"), "location_after_inegi")
  
  diconsa <- read_dta_safe(raw_support("diconsa_dir_2002_2005_pal.dta")) |>
    collapse_consistent_key(c("id_loc", "etapa"), "diconsa_dir_2002_2005_pal")
  loc <- stata_merge(loc, diconsa, by = c("id_loc", "etapa")) |>
    dplyr::filter(._merge != 2) |>
    dplyr::select(-._merge)
  assert_unique_key(loc, c("id_1", "etapa"), "location_after_diconsa")
  
  bnotes <- read_dta_safe(baseline_raw("field notes - Baseline - edit")) |>
    collapse_consistent_key("id_loc", "baseline_field_notes")
  loc <- stata_merge(loc, bnotes, by = "id_loc") |>
    dplyr::filter(._merge != 2) |>
    dplyr::select(-._merge)
  assert_unique_key(loc, c("id_1", "etapa"), "location_after_baseline_notes")
  
  # Original Stata makes follow-up field notes the master, but only
  # comment_code_ffn is used from that file. Reduce it to one consistent row
  # per locality, then attach it to the household location data. This has the
  # same intended result without legacy duplicate-key merge side effects.
  fnotes <- read_dta_safe(followup_raw("field notes - 1st followup - edit")) |>
    dplyr::select(dplyr::any_of(c("id_loc", "comment_code_ffn"))) |>
    collapse_consistent_key("id_loc", "followup_field_notes")
  
  loc <- stata_merge(loc, fnotes, by = "id_loc", update = TRUE) |>
    dplyr::filter(._merge != 2) |>
    dplyr::select(-._merge) |>
    dplyr::arrange(id_1, etapa)
  
  assert_unique_key(loc, c("id_1", "etapa"), "module_location")
  loc
}

build_transfers <- function(b_hh, f_hh) {
  b_names <- c("tortilla", "liconsa", "opor_din", "opor_pap", "dif", "desay_esc", "opor_beca", "beca_otra", "beca_trans", "ini", "probecat", "ali_campo", "apoyo_viv", "procampo", "cred_pala", "pet", "fonaes", "micro_emp", "otro_est", "otro_muni", "seg_pop", "otro", "indig")
  b <- read_dta_safe(baseline_raw("s0405"))
  b <- stata_merge(b, b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge)
  for (i in seq_along(b_names)) {
    p_old <- paste0("s41", i); t_old <- paste0("s42", i)
    if (p_old %in% names(b)) {
      b[[paste0("p_", b_names[[i]])]] <- dplyr::case_when(as_num(b[[p_old]]) == 2 ~ 0, as_num(b[[p_old]]) == 9 ~ NA_real_, TRUE ~ as_num(b[[p_old]]))
    }
    if (t_old %in% names(b)) {
      z <- as_num(b[[t_old]]); z[true_idx(dplyr::between(z, 6, 9))] <- NA_real_; b[[paste0("t_", b_names[[i]])]] <- z
    }
  }
  b <- b |>
    dplyr::select(id_1, etapa, dplyr::starts_with("p_"), dplyr::starts_with("t_"))
  
  f <- read_dta_safe(followup_raw("s402a_2")) |>
    dplyr::select(-dplyr::any_of(c("resul_final", "tipo_c", "idloc", "tprog_2")))
  value_cols <- intersect(c("s41_2", "s42_2"), names(f))
  f <- tidyr::pivot_wider(f, id_cols = cve_viv, names_from = nprog_2, values_from = dplyr::all_of(value_cols), names_glue = "{.value}{nprog_2}")
  f402b <- read_dta_safe(followup_raw("s402b_2"))
  f <- stata_merge(f, f402b, by = "cve_viv", update = TRUE) |>
    dplyr::select(-._merge)
  f <- stata_merge(f, f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge)
  
  f_names <- c("tortilla", "liconsa", "opor_din", "opor_pap", "dif", "desay_esc", "opor_beca", "beca_otra", "beca_trans", "ini", "probecat", "ali_campo", "apoyo_viv", "procampo", "cred_pala", "pet", "fonaes", "micro_emp", "seg_pop", "indig", "otro_muni", "otro", "otro_est", "pal")
  for (i in seq_along(f_names)) {
    p_old <- paste0("s41_2", i); t_old <- paste0("s42_2", i)
    if (p_old %in% names(f)) f[[paste0("p_", f_names[[i]])]] <- dplyr::case_when(as_num(f[[p_old]]) == 2 ~ 0, as_num(f[[p_old]]) == 9 ~ NA_real_, TRUE ~ as_num(f[[p_old]]))
    if (t_old %in% names(f)) {
      z <- as_num(f[[t_old]]); z[true_idx(dplyr::between(z, 6, 9))] <- NA_real_; f[[paste0("t_", f_names[[i]])]] <- z
    }
  }
  f <- rename_existing(f, c(f_cred_2 = "pal_id", s44b2 = "pal_bene_linea", linea_2 = "pal_bene_linea_r", npal2 = "npal_ask"))
  if ("pal_id" %in% names(f)) f$pal_id[true_idx(f$pal_id == "9999999999999999")] <- ""
  if ("npal_ask" %in% names(f)) f$npal_ask <- recode_missing(f$npal_ask, c(66, 99))
  
  month_a <- grep("^...0.a2$", names(f), value = TRUE)
  month_b <- grep("^...0.b2$", names(f), value = TRUE)
  for (nm in month_a) f[[nm]] <- recode_missing(f[[nm]], c(6, 9))
  if (length(month_a)) {
    f$first_mn <- apply(f[month_a], 1, function(x) { z <- which(as_num(x) == 1); if (length(z)) z[[1]] else NA_real_ })
    f$last_pal <- apply(f[month_a], 1, function(x) { z <- which(as_num(x) == 1); if (length(z)) tail(z, 1) else NA_real_ })
    if ("mm_ent" %in% names(f)) {
      f$last_pal <- dplyr::case_when(as_num(f$mm_ent) == 12 ~ 25 - f$last_pal, as_num(f$mm_ent) == 11 ~ 24 - f$last_pal, as_num(f$mm_ent) == 10 ~ 23 - f$last_pal, TRUE ~ f$last_pal)
    }
    if ("p_pal" %in% names(f)) f$p_pal[true_idx(as_num(f$p_pal) == 1 & f$last_pal > 4)] <- 0
    ma_mat <- as.data.frame(lapply(f[month_a], as_num))
    f$npal_calc <- rowSums(ma_mat == 1, na.rm = TRUE)
    f$npal_calc_mis <- rowSums(is.na(ma_mat))
    f$npal_calc[true_idx(f$npal_calc == 0 & f$npal_calc_mis == length(month_a))] <- NA_real_
  } else {
    f$first_mn <- f$last_pal <- f$npal_calc <- NA_real_
  }
  for (nm in month_b) f[[nm]] <- recode_missing(f[[nm]], c(6, 8))
  if (length(month_b)) {
    mb_mat <- as.data.frame(lapply(f[month_b], as_num))
    f$npal_pckg <- rowSums(mb_mat, na.rm = TRUE)
    miss <- rowSums(is.na(mb_mat))
    f$npal_pckg[true_idx(f$npal_pckg == 0 & miss == length(month_b))] <- NA_real_
  } else f$npal_pckg <- NA_real_
  
  f <- rename_existing(f, c(s47_2 = "on_time", s48a_2 = "pickup", s48b_2 = "pickup_s"))
  if ("on_time" %in% names(f)) f$on_time <- dplyr::case_when(as_num(f$on_time) == 2 ~ 0, as_num(f$on_time) %in% c(3, 9) ~ NA_real_, TRUE ~ as_num(f$on_time))
  if (all(c("s49a_2", "s49b_2") %in% names(f))) f$pickup_time <- dplyr::if_else(as_num(f$s49a_2) <= 3, as_num(f$s49b_2) + as_num(f$s49a_2) * 60, NA_real_)
  if ("mm_ent" %in% names(f)) {
    f$npal_pckg_exp <- dplyr::case_when(as_num(f$mm_ent) == 12 ~ 26 - f$first_mn, as_num(f$mm_ent) == 11 ~ 25 - f$first_mn, as_num(f$mm_ent) == 10 ~ 24 - f$first_mn, TRUE ~ NA_real_)
    f$coverage <- f$npal_pckg / f$npal_pckg_exp
    idx <- !is.na(f$coverage) & f$coverage > 1
    idx <- true_idx(idx)
    f$npal_pckg[idx] <- f$npal_pckg_exp[idx]
    f$coverage[true_idx(f$coverage >= 1 & f$coverage <= 5)] <- 1
  }
  if ("p_pal" %in% names(f)) {
    bad <- f$npal_calc == 0 | f$npal_ask == 0 | f$npal_pckg == 0 | is.na(f$npal_calc) | is.na(f$npal_pckg)
    f$p_pal[true_idx(as_num(f$p_pal) == 1 & bad)] <- 0
  }
  
  # PAL classes. Normalize variables that sometimes carry a _2 suffix.
  for (k in 1:5) {
    a <- paste0("s413a", k, "_2"); bnm <- paste0("s413a", k)
    if (a %in% names(f) && !bnm %in% names(f)) names(f)[names(f) == a] <- bnm
  }
  f <- rename_existing(f, c(s410_2 = "class", s411c_2 = "class_yr", s411b_2 = "class_mn", s412_2 = "n_class"))
  if ("class" %in% names(f)) f$class <- dplyr::case_when(as_num(f$class) %in% c(2, 9) ~ 0, TRUE ~ as_num(f$class))
  if ("n_class" %in% names(f)) {
    f$n_class <- recode_missing(f$n_class, c(66, 99))
    
    # Stata's replace ... if condition treats missing comparisons as false.
    # R logical subscripts may contain NA, which is not allowed for assignment,
    # so make each assignment index explicitly TRUE/FALSE.
    idx_nclass_zero <- is.na(f$n_class) & !is.na(f$class) & f$class == 0
    f$n_class[idx_nclass_zero] <- 0
    
    idx_class_zero <- !is.na(f$class) & f$class == 1 &
      (is.na(f$n_class) | (!is.na(f$n_class) & f$n_class == 0))
    f$class[idx_class_zero] <- 0
  }
  class_vars <- paste0("s413a", 1:4)
  if (all(class_vars %in% names(f))) {
    cls <- as.data.frame(lapply(f[class_vars], as_num))
    any_nonmiss <- rowSums(!is.na(cls)) > 0
    mk <- function(code) ifelse(any_nonmiss, as.integer(apply(cls, 1, function(x) any(x == code, na.rm = TRUE))), NA_integer_)
    f$class_o <- mk(1); f$class_n <- mk(2); f$class_s <- mk(3); f$class_h <- mk(4); f$class_t <- mk(5)
    # Match Stata's (x==1 | ...) behavior: missing values do not create an NA result.
    f$class_educ <- as.integer(
      f$class_n %in% 1 | f$class_s %in% 1 | f$class_h %in% 1
    )
    f$class_n_types <- row_total(f, intersect(c("class_o", "class_n", "class_s", "class_h", "class_t"), names(f)))
    
    idx_impute_nclass <- !is.na(f$class) & f$class == 1 & is.na(f$n_class)
    f$n_class[idx_impute_nclass] <- f$class_n_types[idx_impute_nclass]
    
    idx_class_zero2 <- !is.na(f$n_class) & f$n_class == 0
    f$class[idx_class_zero2] <- 0
  }
  
  keep <- unique(c("id_1", "id_loc", "cve_viv", "etapa", "tapoyo", "mm_ent", grep("^(p_|t_)", names(f), value = TRUE),
                   "pal_id", "pal_bene_linea", "pal_bene_linea_r", "npal_ask", "npal_calc", "npal_pckg", "npal_pckg_exp", "coverage", "on_time", "pickup", "pickup_s", "pickup_time", "first_mn", "last_pal", "class", "class_yr", "class_mn", "n_class", "class_o", "class_n", "class_s", "class_h", "class_t", "class_educ", "class_n_types"))
  f <- dplyr::select(f, dplyr::any_of(keep))
  
  transfers <- dplyr::bind_rows(f, b)
  drop_programs <- c("tortilla", "beca_trans", "ini", "probecat", "ali_campo", "pet", "fonaes", "indig", "micro_emp", "cred_pala", "apoyo_viv")
  transfers <- drop_existing(transfers, unlist(lapply(drop_programs, function(x) c(paste0("p_", x), paste0("t_", x)))))
  names_recode <- c("liconsa", "opor_din", "opor_pap", "dif", "desay_esc", "opor_beca", "beca_otra", "procampo", "seg_pop", "otro_muni", "otro", "otro_est", "pal")
  for (nm in names_recode) {
    p <- paste0("p_", nm); t <- paste0("t_", nm)
    if (all(c(p, t) %in% names(transfers))) transfers[[p]][true_idx(is.na(transfers[[t]]) & as_num(transfers[[p]]) == 1)] <- 0
  }
  if (all(c("p_opor_din", "p_opor_pap", "p_opor_beca") %in% names(transfers))) transfers$p_opor_any <- as.integer(transfers$p_opor_din == 1 | transfers$p_opor_pap == 1 | transfers$p_opor_beca == 1)
  if ("t_pal" %in% names(transfers)) {
    transfers$t_pal[true_idx(as_num(transfers$p_pal) == 0 | as_num(transfers$tapoyo) == 1)] <- NA_real_
    transfers$t_pal[true_idx(as_num(transfers$t_pal) == 5)] <- 4
    for (i in 1:4) transfers[[paste0("t_pal_", i)]] <- as.integer(transfers$t_pal == i)
    transfers <- transfers |>
      dplyr::group_by(id_loc) |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("t_pal_"), ~ ifelse(etapa == 2, mean(.x[etapa == 2], na.rm = TRUE), NA_real_), .names = "pct_{.col}")) |>
      dplyr::ungroup()
    pct_cols <- paste0("pct_t_pal_", 1:4)
    transfers$pct_max <- row_max_no_warn(transfers, pct_cols)
    transfers$t_pal_vil <- NA_real_
    for (i in 1:4) {
      idx <- transfers$etapa == 2 & is.na(transfers$t_pal_vil) & !is.na(transfers$pct_max) & transfers$pct_max == transfers[[pct_cols[[i]]]]
      transfers$t_pal_vil[true_idx(idx)] <- i
    }
    transfers$t_pal_vil[true_idx(as_num(transfers$tapoyo) == 1)] <- 0
    transfers <- drop_existing(transfers, c(paste0("t_pal_", 1:4), pct_cols, "pct_max"))
  }
  fill_vars <- intersect(c("p_pal", "t_pal", "t_pal_vil", "pal_bene_linea", "pickup", "class", "n_class", "pal_bene_linea_r", "npal_calc", "npal_pckg", "npal_pckg_exp", "coverage", "on_time", "pickup_time", "p_opor_any", "pal_id", "pickup_s"), names(transfers))
  transfers <- carry_fu_to_baseline(transfers, "id_1", fill_vars) |>
    dplyr::arrange(id_1, etapa)
  save_intermediate(transfers, "transfers")
  transfers
}

build_vivienda <- function(b_hh, f_hh) {
  b <- read_dta_safe(baseline_raw("s0505")) |>
    dplyr::distinct(id_1, .keep_all = TRUE)
  b <- stata_merge(b, b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge) |>
    dplyr::select(dplyr::matches("^s5"), id_1, id_loc, etapa)
  
  f <- read_dta_safe(followup_raw("s502_2"))
  f <- stata_merge(f, f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge)
  f <- dplyr::select(f, -dplyr::matches("^tapoyo[1-9]$|^s511.*1_2$"), -dplyr::any_of(c("tipo_c", "idloc", "resul_final")))
  f <- rename_existing(f, c(s58e_2 = "s58o"))
  for (i in 1:10) if (paste0("s5", i, "_2") %in% names(f)) names(f)[names(f) == paste0("s5", i, "_2")] <- paste0("s5", i)
  for (y in c("t", "f")) for (x in letters[1:11]) {
    old <- paste0("s511", y, x, "_2"); new <- paste0("s511", y, x)
    if (old %in% names(f)) names(f)[names(f) == old] <- new
  }
  v <- dplyr::bind_rows(f, b)
  v <- rename_existing(v, c(s51 = "dirt_flr", s54 = "no_kitchen", s56 = "n_rooms", s57 = "n_bedroom", s58 = "agua_tube", s510 = "lights"))
  v <- v |>
    dplyr::mutate(
      dirt_flr = dplyr::case_when(as_num(dirt_flr) %in% c(2, 3) ~ 0, as_num(dirt_flr) == 9 ~ NA_real_, TRUE ~ as_num(dirt_flr)),
      temp_wall_roof = dplyr::if_else(!is.na(s52) & !is.na(s53) & !as_num(s52) %in% 9 & !as_num(s53) %in% 9, as.integer(as_num(s52) %in% 1:3 | as_num(s53) %in% 1:2), NA_integer_),
      no_kitchen = dplyr::case_when(as_num(no_kitchen) %in% c(-9, 9) ~ NA_real_, as_num(no_kitchen) == 2 ~ 1, as_num(no_kitchen) == 1 ~ 0, TRUE ~ as_num(no_kitchen)),
      no_kitchen = dplyr::if_else(as_num(s55) == 1, 1, no_kitchen),
      n_rooms = dplyr::case_when(dplyr::between(as_num(n_rooms), 7, 11) ~ 6, as_num(n_rooms) %in% c(-9, 99) ~ NA_real_, TRUE ~ as_num(n_rooms)),
      n_bedroom = dplyr::case_when(as_num(n_bedroom) == 7 ~ 6, as_num(n_bedroom) %in% c(-9, 99) ~ NA_real_, TRUE ~ as_num(n_bedroom)),
      agua_tube = dplyr::case_when(as_num(agua_tube) %in% 1:2 ~ 1, dplyr::between(as_num(agua_tube), 3, 7) ~ 0, as_num(agua_tube) == -9 ~ NA_real_, TRUE ~ as_num(agua_tube)),
      s59 = dplyr::if_else(as_num(s59) %in% c(-9, 9), NA_real_, as_num(s59)),
      bath_in = dplyr::if_else(!is.na(s59), as.integer(s59 %in% 1:2), NA_integer_),
      bath_out = dplyr::if_else(!is.na(s59), as.integer(s59 %in% 3:4), NA_integer_),
      bath_none = dplyr::if_else(!is.na(s59), as.integer(s59 == 5), NA_integer_),
      lights = dplyr::case_when(as_num(lights) %in% c(-9, 9) ~ NA_real_, as_num(lights) == 2 ~ 0, TRUE ~ as_num(lights))
    )
  if (all(c("s511ta", "s511tb") %in% names(v))) v$radio_tv <- ifelse(is.na(v$s511ta) | is.na(v$s511tb) | as_num(v$s511ta) == 9 | as_num(v$s511tb) == 9, NA, as.integer(as_num(v$s511ta) == 1 | as_num(v$s511tb) == 1))
  src <- c(s511tf = "fridge", s511th = "gas_stove", s511tg = "washer", s511tc = "vcr")
  for (old in names(src)) if (old %in% names(v)) {
    z <- as_num(v[[old]]); z[z %in% c(-9, 9)] <- NA; z[z == 2] <- 0; v[[src[[old]]]] <- z
  }
  if (all(c("s511tj", "s511tk") %in% names(v))) {
    a <- recode_missing(v$s511tj, c(-9, 9)); b2 <- recode_missing(v$s511tk, c(-9, 9))
    v$car_moto <- ifelse(is.na(a) & is.na(b2), NA, as.integer(a == 1 | b2 == 1))
  }
  v |>
    dplyr::select(-dplyr::matches("^s5")) |>
    dplyr::arrange(id_1, etapa)
}

convert_food_units <- function(df) {
  # Stata's `replace ... if inrange(...)` silently ignores missing values.
  # Do all food-unit edits on plain numeric vectors and use integer positions
  # (`which`) so NA can never enter a subscript assignment. This also removes
  # haven_labelled/vctrs classes before modification.
  veg_factors <- c(.106, .278, .211, .086, .350, .227, .170, .170, .056,
                   .113, .113, .170, .232, .118, .165, .088, .227)
  
  for (t in c(2, 3, 5)) {
    mult <- function(code, units, factor) {
      cvar <- paste0("s6", t, "c", code)
      uvar <- paste0("s6", t, "u", code)
      if (!all(c(cvar, uvar) %in% names(df))) return(invisible(NULL))
      
      qty  <- as_num(df[[cvar]])
      unit <- as_num(df[[uvar]])
      idx <- which(!is.na(unit) & unit %in% units)
      if (length(idx)) qty[idx] <- qty[idx] * factor
      
      df[[cvar]] <<- qty
      df[[uvar]] <<- unit
      invisible(NULL)
    }
    
    miss <- function(code, units) {
      cvar <- paste0("s6", t, "c", code)
      uvar <- paste0("s6", t, "u", code)
      if (!all(c(cvar, uvar) %in% names(df))) return(invisible(NULL))
      
      qty  <- as_num(df[[cvar]])
      unit <- as_num(df[[uvar]])
      idx <- which(!is.na(unit) & unit %in% units)
      if (length(idx)) qty[idx] <- NA_real_
      
      df[[cvar]] <<- qty
      df[[uvar]] <<- unit
      invisible(NULL)
    }
    
    recode_unit_if_qty <- function(code, from_unit, lo, hi, to_unit = 3) {
      cvar <- paste0("s6", t, "c", code)
      uvar <- paste0("s6", t, "u", code)
      if (!all(c(cvar, uvar) %in% names(df))) return(invisible(NULL))
      
      qty  <- as_num(df[[cvar]])
      unit <- as_num(df[[uvar]])
      idx <- which(!is.na(unit) & !is.na(qty) &
                     unit == from_unit & qty >= lo & qty <= hi)
      if (length(idx)) unit[idx] <- to_unit
      
      df[[cvar]] <<- qty
      df[[uvar]] <<- unit
      invisible(NULL)
    }
    
    # Vegetables: Stata lines 288-298.
    for (j in seq_along(veg_factors)) {
      mult(100 + j, 3:5, veg_factors[[j]])
    }
    
    # Grains: Stata lines 299-328.
    mult(201, 3, .035)
    mult(201, 4:5, .5)
    mult(202, 3:5, .1)
    mult(203, 3:5, .1)
    for (code in 204:205) {
      recode_unit_if_qty(code, 1, 11, 98, 3)
      mult(code, 3:5, .07)
    }
    mult(206, 3:5, .680)
    miss(207, 3)
    mult(208, 2:5, .2)
    mult(209, 4:5, .5)
    mult(209, 3, .041)
    mult(210, 3:5, .2)
    mult(211, 3:5, .15)
    for (code in 212:216) mult(code, 3:5, .1)
    
    # Animal products: Stata lines 329-357.
    mult(301, 3:5, .188)
    mult(302, 3:5, .15)
    mult(303, 3:5, .15)
    mult(304, 3:5, .185)
    mult(305, 3:5, .4)
    recode_unit_if_qty(306, 1, 10, 75, 3)
    mult(306, 3, .062)
    mult(306, 4:5, .744)
    mult(307, 2, .13)
    mult(307, 3:4, .1)
    mult(307, 5, .25)
    mult(308, 3:5, .15)
    for (code in 309:311) mult(code, 3:5, .25)
    mult(312, 2, .13)
    mult(312, 3:4, .1)
    mult(312, 5, .25)
    
    # Processed foods: Stata lines 358-390.
    mult(401, 3:5, .12)
    mult(402, 3:5, .5)
    miss(403, 5)
    mult(403, 3:4, .1)
    mult(404, 2:5, .1)
    miss(405, 3:5)
    miss(406, 3:5)
    mult(407, 3:4, .009)
    mult(407, 5, .045)
    mult(408, 3:5, .09)
    mult(409, 3:5, .09)
    mult(410, 3:5, .1)
    mult(411, 3:5, .25)
    mult(412, 3:5, .05)
    mult(413, 3:5, .25)
    mult(414, 3:5, .25)
    mult(415, 3:5, .25)
    mult(416, 3:5, .2)
  }
  
  df
}

build_food_consumption <- function(b_hh, f_hh) {
  b <- read_dta_safe(baseline_raw("s0606_basal_nva version_w.dta"))
  b <- stata_merge(b, b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge)
  for (code in food_codes) {
    for (pair in list(c(paste0("s61c", code), paste0("s61", code)), c(paste0("s64c", code), paste0("s64", code)), c(paste0("s66c", code), paste0("s66", code)), c(paste0("s66o", code), paste0("s660", code)))) {
      if (pair[[1]] %in% names(b)) names(b)[names(b) == pair[[1]]] <- pair[[2]]
    }
  }
  
  f <- read_dta_safe(followup_raw("s602_2.dta")) |>
    dplyr::filter(!is.na(alimento), alimento != "")
  f$s6na_2 <- as.character(as_num(f$s6na_2)); f$s6na_2 <- ifelse(nchar(f$s6na_2) == 1, paste0("0", f$s6na_2), f$s6na_2)
  f$food <- as.numeric(paste0(as.character(as_num(f$tipo)), f$s6na_2))
  f <- drop_existing(f, c("tipo_c", "tapoyo", "alimento", "s6na", "tipo", "idloc"))
  for (v in c("1", "2c", "2u", "3c", "3u", "4", "5c", "5u", "6", "60")) {
    old <- paste0("s6", v, "_2"); new <- paste0("s6", v)
    if (old %in% names(f)) names(f)[names(f) == old] <- new
  }
  vals <- grep("^s6", names(f), value = TRUE); vals <- setdiff(vals, c("s6na_2"))
  f <- tidyr::pivot_wider(f, id_cols = cve_viv, names_from = food, values_from = dplyr::all_of(vals), names_glue = "{.value}{food}")
  f <- stata_merge(f, f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge, -dplyr::matches("417$"))
  
  x <- dplyr::bind_rows(f, b)
  x <- convert_food_units(x)
  for (code in food_codes) {
    old <- c(paste0("s61", code), paste0("s62c", code), paste0("s62u", code), paste0("s63c", code), paste0("s63u", code), paste0("s64", code), paste0("s65c", code), paste0("s65u", code), paste0("s66", code), paste0("s660", code))
    new <- c(paste0("f", code, "days"), paste0("f", code, "ate"), paste0("f", code, "ate_u"), paste0("f", code, "bought"), paste0("f", code, "bought_u"), paste0("f", code, "spent"), paste0("f", code, "auto"), paste0("f", code, "auto_u"), paste0("f", code, "reason"), paste0("f", code, "reason_oth"))
    for (j in seq_along(old)) if (old[[j]] %in% names(x)) names(x)[names(x) == old[[j]]] <- new[[j]]
    days <- paste0("f", code, "days"); ate <- paste0("f", code, "ate"); au <- paste0("f", code, "ate_u"); bought <- paste0("f", code, "bought"); bu <- paste0("f", code, "bought_u"); spent <- paste0("f", code, "spent"); auto <- paste0("f", code, "auto"); autou <- paste0("f", code, "auto_u")
    if (days %in% names(x)) {
      z <- as_num(x[[days]])
      idx8 <- which(!is.na(z) & z == 8)
      if (length(idx8)) z[idx8] <- NA_real_
      x[[days]] <- z
    }
    if (all(c(ate, au) %in% names(x))) {
      qty <- as_num(x[[ate]])
      unit <- as_num(x[[au]])
      bad <- which(is.na(unit) | unit >= 6)
      if (length(bad)) qty[bad] <- NA_real_
      x[[ate]] <- recode_missing(qty, c(0, 66, 66.66, 88.88, 99, 99.99))
      x[[au]] <- unit
    }
    if (all(c(bought, bu) %in% names(x))) {
      qty <- as_num(x[[bought]])
      unit <- as_num(x[[bu]])
      bad <- which(is.na(unit) | unit >= 6)
      if (length(bad)) qty[bad] <- NA_real_
      x[[bought]] <- recode_missing(qty, c(0, 66, 66.66, 88.88, 99, 99.99))
      x[[bu]] <- unit
    }
    if (spent %in% names(x)) {
      x[[spent]] <- recode_missing(x[[spent]], c(0, 666.66, 988.88, 999.99))
    }
    if (all(c(auto, autou) %in% names(x))) {
      qty <- as_num(x[[auto]])
      unit <- as_num(x[[autou]])
      bad <- which(is.na(unit) | unit >= 6)
      if (length(bad)) qty[bad] <- NA_real_
      x[[auto]] <- recode_missing(qty, c(0, 66, 66.66, 88.88, 99, 99.99))
      x[[autou]] <- unit
    }
    for (nm in intersect(c(days, ate, bought, spent, auto), names(x))) {
      x[[nm]] <- as_num(x[[nm]]) * 4.35
    }
    if (days %in% names(x)) {
      dayv <- as_num(x[[days]])
      zero_day <- !is.na(dayv) & dayv == 0
      for (nm in intersect(c(ate, bought, spent, auto), names(x))) {
        z <- as_num(x[[nm]])
        idx0 <- which(zero_day & is.na(z))
        if (length(idx0)) z[idx0] <- 0
        x[[nm]] <- z
      }
    }
  }
  x <- dplyr::select(x, -dplyr::matches("_u$|reason")) |>
    dplyr::arrange(id_1, etapa)
  x
}

build_food_extra <- function(b_hh, f_hh) {
  b <- read_dta_safe(baseline_raw("s06e05"))
  svars <- grep("^s", names(b), value = TRUE); names(b)[match(svars, names(b))] <- paste0(svars, "_2")
  b <- drop_existing(b, c("id_loc", "tipoloc", "entidad", "tipo_r", "tipo_c")) |>
    dplyr::distinct()
  b <- stata_merge(b, b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge)
  f <- read_dta_safe(followup_raw("s602e_2.dta")) |> drop_existing(c("._merge"))
  f <- stata_merge(f, f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(-._merge, -dplyr::any_of(c("seg_alim", "idloc", "tipo_c", "resul_final", "split")))
  x <- dplyr::bind_rows(f, b)
  for (nm in grep("^s.*_2$", names(x), value = TRUE)) names(x)[names(x) == nm] <- sub("_2$", "", nm)
  x <- rename_existing(x, c(s625 = "special_event", s626 = "n_pers_eat_away", s627 = "n_meals_eat_away", s628 = "n_guests_eat", s629 = "n_guests_meals", s630 = "exp_food_away", s631 = "exp_food_subj", s632 = "exp_total_subj", s633 = "veg_loc", s635 = "mercado_transport", s637 = "lack_food_ever"))
  x$special_event <- recode_missing(x$special_event, 6)
  for (nm in intersect(c("n_pers_eat_away", "n_meals_eat_away", "n_guests_eat", "n_guests_meals"), names(x))) x[[nm]] <- recode_missing(x[[nm]], 66)
  for (nm in intersect(c("exp_food_away", "exp_food_subj", "exp_total_subj"), names(x))) { x[[nm]] <- recode_missing(x[[nm]], c(6666, 9999)); x[[nm]][is.na(x[[nm]])] <- 0; x[[nm]] <- x[[nm]] * 4.35 }
  if ("veg_loc" %in% names(x)) x$veg_loc <- dplyr::case_when(as_num(x$veg_loc) == 2 ~ 0, as_num(x$veg_loc) == 6 ~ NA_real_, TRUE ~ as_num(x$veg_loc))
  if (all(c("s634e1", "s634e2", "s634e3") %in% names(x))) {
    e1 <- recode_missing(x$s634e1, c(62, 99, 999)); e2 <- recode_missing(x$s634e2, c(99, 999)); e3 <- recode_missing(x$s634e3, c(99, 999)); x$mercado_lat <- e1 + e2 / 60 + e3 / 3600
  }
  if (all(c("s634f1", "s634f2", "s634f3") %in% names(x))) {
    f1 <- as_num(x$s634f1); f1[true_idx(dplyr::between(f1, 10, 47) | f1 == 999)] <- NA_real_; f2 <- recode_missing(x$s634f2, c(99, 102, 134, 560, 999)); f3 <- recode_missing(x$s634f3, c(99, 102, 134, 560, 999)); x$mercado_lon <- f1 + f2 / 60 + f3 / 3600
  }
  if ("mercado_transport" %in% names(x)) x$mercado_transport <- recode_missing(x$mercado_transport, 9)
  x$mercado_min <- ifelse(x$veg_loc == 1, 0, NA_real_)
  if (all(c("s636m", "s636h") %in% names(x))) {
    s636m <- as_num(x$s636m)
    s636h <- as_num(x$s636h)
    idx <- true_idx(x$veg_loc == 0 & s636m != 99 & s636h != 99)
    if (length(idx)) x$mercado_min[idx] <- s636m[idx] + s636h[idx] * 60
  }
  if ("lack_food_ever" %in% names(x)) x$lack_food_ever <- recode_binary_2_0(x$lack_food_ever)
  lack <- grep("^s638", names(x), value = TRUE)
  for (nm in lack) { new <- paste0("lack_food_", substring(nm, 5, 6)); names(x)[names(x) == nm] <- new; x[[new]] <- recode_missing(x[[new]], 6) }
  lack2 <- grep("^lack_food_..$", names(x), value = TRUE)
  x$lack_food_n_months <- ifelse(x$lack_food_ever == 1, row_total(x, lack2), NA_real_)
  x <- x |>
    dplyr::group_by(id_loc) |>
    dplyr::mutate(veg_loc_v = ifelse(etapa == 2, mean(veg_loc[etapa == 2], na.rm = TRUE), NA_real_), mercado_min_v = ifelse(etapa == 2, mean(mercado_min[etapa == 2], na.rm = TRUE), NA_real_)) |>
    dplyr::ungroup()
  for (nm in intersect(c("exp_food_away", "exp_food_subj", "exp_total_subj"), names(x))) { idx <- true_idx(as_num(x$etapa) == 2); z <- as_num(x[[nm]]); z[idx] <- z[idx] * (100 / 109.4); x[[nm]] <- z }
  x |>
    dplyr::select(-dplyr::matches("^s6")) |>
    dplyr::arrange(id_1, etapa)
}

build_food24_hh <- function(b_hh, f_hh) {
  b <- stata_merge(read_dta_safe(baseline_raw("sr2406_hogar")), b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge == 3) |>
    dplyr::select(energia, id_1, etapa)
  f <- stata_merge(read_dta_safe(followup_raw("s1402_hogar")), f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge == 3) |>
    dplyr::select(energia, id_1, cve_viv, etapa)
  dplyr::bind_rows(f, b) |>
    dplyr::rename(cal_24 = energia) |>
    dplyr::arrange(id_1, etapa)
}

build_nonfood <- function(b_hh, f_hh) {
  b <- stata_merge(read_dta_safe(baseline_raw("s0705")), b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(etapa, id_1, id_loc, dplyr::matches("^s7"))
  f <- stata_merge(read_dta_safe(followup_raw("s702_2")), f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::select(dplyr::any_of(c("id_1", "cve_viv", "id_loc", "etapa", "split")), dplyr::matches("^s7"))
  x <- dplyr::bind_rows(f, b)
  for (i in 1:3) {
    old <- paste0("s71", letters[i]); new <- paste0("n10", i, "exp")
    if (old %in% names(x)) { z <- recode_missing(x[[old]], 999); x[[new]] <- z * 4.35 }
  }
  if ("s72h" %in% names(x)) x$s72h <- recode_missing(x$s72h, 9999)
  for (i in 1:8) { old <- paste0("s72", letters[i]); new <- paste0("n20", i, "exp"); if (old %in% names(x)) x[[new]] <- recode_missing(x[[old]], 999) }
  if ("s73o" %in% names(x)) x$s73o <- recode_missing(x$s73o, 99999)
  for (i in 1:9) { old <- paste0("s73", letters[i]); new <- paste0("n30", i, "exp"); if (old %in% names(x)) x[[new]] <- recode_missing(x[[old]], 9999) / 6 }
  if ("s73j" %in% names(x)) x$n310exp <- recode_missing(x$s73j, 9999) / 6
  for (i in 1:5) { old <- paste0("s73", letters[10 + i]); new <- paste0("n31", i, "exp"); if (old %in% names(x)) x[[new]] <- recode_missing(x[[old]], 9999) / 6 }
  for (nm in grep("^n...exp$", names(x), value = TRUE)) { idx <- true_idx(as_num(x$etapa) == 2); z <- as_num(x[[nm]]); z[idx] <- z[idx] * (100 / 109.4); x[[nm]] <- z }
  x |>
    dplyr::arrange(id_1, etapa)
}

build_credit <- function(b_hh, f_hh) {
  b <- stata_merge(read_dta_safe(baseline_raw("s0805")), b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(dplyr::matches("^s8"), etapa, id_1, id_loc)
  f <- stata_merge(read_dta_safe(followup_raw("s802_2")), f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::select(dplyr::any_of(c("id_1", "id_loc", "cve_viv", "etapa", "split")), dplyr::matches("^s8")) |>
    dplyr::select(-dplyr::any_of("s82_2_tran"))
  for (i in 1:3) if (paste0("s8", i, "_2") %in% names(f)) names(f)[names(f) == paste0("s8", i, "_2")] <- paste0("s8", i)
  x <- rename_existing(dplyr::bind_rows(f, b), c(s81 = "debt", s82 = "debt_a", s83 = "debt_p"))
  x$debt <- recode_binary_2_0(x$debt); x$debt_a <- recode_missing(x$debt_a, c(98888, 99999)); x$debt_p <- recode_missing(x$debt_p, c(98880, 98888))
  idx <- true_idx(as_num(x$etapa) == 2); x$debt_a <- as_num(x$debt_a); x$debt_p <- as_num(x$debt_p); x$debt_a[idx] <- x$debt_a[idx] * (100 / 109.4); x$debt_p[idx] <- x$debt_p[idx] * (100 / 109.4)
  dplyr::arrange(x, id_1, etapa)
}

build_income <- function(b_hh, f_hh) {
  b <- stata_merge(read_dta_safe(baseline_raw("s0905")), b_hh, by = "id_1", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge != 1) |>
    dplyr::select(id_1, id_loc, etapa, dplyr::matches("^s9")) |>
    dplyr::select(-dplyr::any_of(c("s99b5e_c", "s99ge_ca")), -dplyr::matches("^s99b..r$"))
  f <- read_dta_safe(followup_raw("s902_2")) |> dplyr::distinct()
  f <- stata_merge(f, f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::select(dplyr::any_of(c("id_1", "id_loc", "cve_viv", "etapa", "split")), dplyr::matches("^s9")) |>
    dplyr::select(-dplyr::matches("^s99b4.*_2t$|^s99f.*_2t$"))
  for (letter in c("c", "d", "e", "f", "g")) {
    if (paste0("s99", letter, "b") %in% names(f)) names(f)[names(f) == paste0("s99", letter, "b")] <- paste0("s99", letter, "a")
    if (paste0("s99", letter, "c") %in% names(f)) names(f)[names(f) == paste0("s99", letter, "c")] <- paste0("s99", letter, "b")
  }
  x <- dplyr::bind_rows(f, b)
  x <- rename_existing(x, c(s91 = "own_home", s92 = "own_oth_home", s93 = "own_land", s95 = "val_hm_lnd", s96 = "farm", s97 = "farm_cost", s98 = "farm_profit", s99a = "has_trees", s99ac = "trees_v", s99b5e = "des_ani_5", s99ge = "des_capital_g"))
  for (nm in c("own_home", "own_oth_home", "own_land")) if (nm %in% names(x)) x[[nm]] <- recode_binary_2_0(x[[nm]])
  if (all(c("s94c", "s94u") %in% names(x))) {
    q <- recode_missing(x$s94c, c(9999.99, 999999)); u <- as_num(x$s94u)
    x$amt_land <- dplyr::case_when(u == 2 ~ q, u == 1 ~ q / 10000, u == 3 ~ q * 10000, TRUE ~ NA_real_); x$amt_land[!is.na(x$amt_land) & x$amt_land > 200] <- 200
  }
  x$val_hm_lnd <- recode_missing(x$val_hm_lnd, c(98888, 99999)); x$farm <- dplyr::case_when(as_num(x$farm) == 2 ~ 0, as_num(x$farm) %in% c(8, 9) ~ NA_real_, TRUE ~ as_num(x$farm)); x$farm_cost <- recode_missing(x$farm_cost, c(98888, 99999)); x$farm_profit <- recode_missing(x$farm_profit, c(98888, 99999)); x$farm_profit[true_idx(x$farm == 0)] <- NA_real_
  x$has_trees <- dplyr::case_when(as_num(x$has_trees) == 2 ~ 0, as_num(x$has_trees) %in% c(8, 9) ~ NA_real_, TRUE ~ as_num(x$has_trees)); x$trees_v <- recode_missing(x$trees_v, c(98888, 99922, 99999))
  for (i in 1:5) {
    a <- paste0("s99b", i, "a"); bnm <- paste0("s99b", i, "b"); c <- paste0("s99b", i, "c")
    if (a %in% names(x)) x[[paste0("num_animal_", i)]] <- recode_missing(x[[a]], 99)
    if (bnm %in% names(x)) { z <- as_num(x[[bnm]]); z[z == 2] <- 0; z[z %in% c(8, 9)] <- NA; x[[paste0("has_animal_", i)]] <- z }
    if (c %in% names(x)) x[[paste0("val_animal_", i)]] <- recode_missing(x[[c]], c(98888, 99999))
  }
  for (letter in c("c", "d", "e", "f", "g")) {
    a <- paste0("s99", letter, "a"); bnm <- paste0("s99", letter, "b")
    if (a %in% names(x)) { z <- as_num(x[[a]]); z[z == 2] <- 0; z[z %in% c(8, 9)] <- NA; x[[paste0("has_capital_", letter)]] <- z }
    if (bnm %in% names(x)) x[[paste0("val_capital_", letter)]] <- recode_missing(x[[bnm]], c(98888, 99999))
  }
  money <- grep("^(val_hm_lnd|farm_cost|farm_profit|trees_v|val_animal|val_capital)", names(x), value = TRUE)
  for (nm in money) { idx <- true_idx(as_num(x$etapa) == 2); z <- as_num(x[[nm]]); z[idx] <- z[idx] * (100 / 109.4); x[[nm]] <- z }
  x |>
    dplyr::select(-dplyr::matches("^s9")) |>
    dplyr::arrange(id_1, etapa)
}

build_inventory <- function(f_hh) {
  x <- stata_merge(read_dta_safe(followup_raw("s1302_2")), f_hh, by = "cve_viv", update = TRUE, replace = TRUE) |>
    dplyr::filter(._merge == 3) |>
    dplyr::select(-._merge)
  x$inv_cal <- as_num(x$kcaladeq) * as_num(x$adultoeq)
  if (all(c("gramos7", "gramos13") %in% names(x))) x$inv_fish <- as_num(x$gramos7) + as_num(x$gramos13)
  x <- drop_existing(x, c("gramos3", "gramos7", "gramos12", "gramos13", "gramos14"))
  x <- rename_existing(x, c(num_ali = "inv_n", gramos1 = "inv_rice", gramos2 = "inv_cookie", gramos4 = "inv_lentil", gramos5 = "inv_milk", gramos6 = "inv_oil", gramos8 = "inv_cereal", gramos9 = "inv_beans", gramos10 = "inv_crnflr", gramos11 = "inv_pasta"))
  x |>
    dplyr::select(dplyr::any_of(c("id_1", "id_loc", "cve_viv", "etapa", "split", "tapoyo")), dplyr::starts_with("inv_")) |>
    dplyr::arrange(id_1, etapa)
}


build_household_modules <- function(samples, save = TRUE) {
  b_hh <- samples$b
  f_hh <- samples$f
  
  modules <- list(
    location = build_location(b_hh, f_hh),
    transfers = build_transfers(b_hh, f_hh),
    vivienda = build_vivienda(b_hh, f_hh),
    credit = build_credit(b_hh, f_hh),
    food = build_food_consumption(b_hh, f_hh),
    food_extra = build_food_extra(b_hh, f_hh),
    nfood = build_nonfood(b_hh, f_hh),
    food24 = build_food24_hh(b_hh, f_hh),
    income = build_income(b_hh, f_hh),
    inventory = build_inventory(f_hh)
  )
  
  both_wave_modules <- setdiff(names(modules), "inventory")
  
  for (nm in names(modules)) {
    z <- modules[[nm]]
    
    # The authors later use legacy `merge id_1 etapa using ...`.  Some raw
    # modules (notably follow-up food_extra) legitimately contain repeated
    # id_1-etapa keys.  Record those duplicates in the checkpoint, but do not
    # delete them or force artificial uniqueness here.
    checkpoint(
      paste0("05_module_", nm),
      z,
      key = c("id_1", "etapa"),
      both_waves = nm %in% both_wave_modules,
      require_unique = FALSE
    )
    
    if (save) {
      save_intermediate(z, paste0("module_", nm))
    }
  }
  
  modules
}
