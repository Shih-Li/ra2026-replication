# 04_build_samples.R
# Build the baseline/follow-up household samples used by all later modules.
#
# Direct translation of the first part of analysis*.do.
# The split-household construction is implemented explicitly rather than
# through a generic many-to-many merge.

make_hh_samples <- function(save = TRUE) {
  # ---------------------------------------------------------------------------
  # Baseline household sample
  # ---------------------------------------------------------------------------
  b0 <- read_dta_safe(baseline_raw("s0105"))

  drop_b <- (
    as.character(b0$tipo_r) == "2" |
    (!is.na(as_num(b0$tipoloc)) & as_num(b0$tipoloc) == 0) |
    b0$id_1 == "0706511591601811111"
  )

  b <- stata_drop_rows(b0, drop_b) |>
    dplyr::transmute(
      id_1 = as.character(id_1),
      id_loc = as.character(id_loc),
      etapa = as_num(etapa)
    ) |>
    dplyr::arrange(id_1)

  assert_unique_key(b, "id_1", "b_hh_sample")
  checkpoint("04a_b_hh_sample", b, key = "id_1")

  # ---------------------------------------------------------------------------
  # First follow-up household sample
  # ---------------------------------------------------------------------------
  f0 <- read_dta_safe(followup_raw("s102_2"))

  # Original Stata logic:
  #   1. use cveviv2 to find the parent household's id_1 via cve_viv
  #   2. attach that parent id_1 to split-off rows
  #   3. mark matched split-off rows split==2 and drop them
  parent_lookup <- f0 |>
    dplyr::filter(!stata_missing(cve_viv), !stata_missing(id_1)) |>
    dplyr::select(
      cve_viv,
      parent_id_1 = id_1
    ) |>
    dplyr::distinct(cve_viv, .keep_all = TRUE)

  f <- f0 |>
    dplyr::left_join(
      parent_lookup,
      by = c("cveviv2" = "cve_viv")
    )

  split_match <- (
    !stata_missing(f$cveviv2) &
    !stata_missing(f$parent_id_1)
  )

  # For matched split-off households, the intended baseline household key is
  # the parent's id_1.
  idx_split <- true_idx(split_match)
  if (length(idx_split)) {
    f$id_1[idx_split] <- f$parent_id_1[idx_split]
  }

  f$._split_match <- split_match
  f <- f[true_idx(!stata_missing(f$id_1)), , drop = FALSE]

  # Stata: by id_1: egen split=count(_N); recode 2/3=1 else=0;
  #        replace split=2 if _m==3
  group_n <- ave(
    rep.int(1L, nrow(f)),
    f$id_1,
    FUN = length
  )

  f$split <- ifelse(group_n %in% c(2L, 3L), 1, 0)
  f$split[true_idx(f$._split_match)] <- 2

  # Stata drop if resul_final>=3; missing resul_final is retained.
  f <- stata_drop_rows(
    f,
    !is.na(as_num(f$resul_final)) & as_num(f$resul_final) >= 3
  )

  # Drop split-off rows; retain the original household.
  f <- stata_drop_rows(f, !is.na(f$split) & f$split == 2)

  f <- rename_existing(
    f,
    c(
      idloc = "id_loc",
      muni = "cve_mun",
      locali = "cve_loc"
    )
  )

  f <- f |>
    dplyr::transmute(
      cve_viv = as.character(cve_viv),
      id_1 = as.character(id_1),
      id_loc = as.character(id_loc),
      entidad,
      nom_ent,
      cve_mun,
      nom_mun,
      cve_loc,
      nom_loc,
      etapa = as_num(etapa),
      split = as_num(split),
      tapoyo = as_num(tapoyo)
    ) |>
    dplyr::arrange(id_1)

  assert_unique_key(f, "id_1", "f_hh_sample")
  assert_unique_key(f, "cve_viv", "f_hh_sample")

  checkpoint("04b_f_hh_sample", f, key = "id_1")

  if (save) {
    save_intermediate(b, "b_hh_sample")
    save_intermediate(f, "f_hh_sample")
  }

  list(b = b, f = f)
}
