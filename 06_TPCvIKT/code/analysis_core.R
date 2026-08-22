# Shared analysis translation for analysis.do, analysis_revised.do, and
# analysis_hhattrit_fixed.do.
#
# The data construction is performed by build_household.R / individual.R.
# This file contains analysis functions and writes tables/figures only to
# output/ and paper/. It never modifies source data.

source(file.path(if (dir.exists(file.path(getwd(), "code"))) getwd() else dirname(getwd()), "code", "_helpers.R"), local = FALSE)

existing <- function(data, vars) intersect(vars, names(data))

safe_mean <- function(x) if (any(is.finite(x), na.rm = TRUE)) mean(x, na.rm = TRUE) else NA_real_
safe_sd <- function(x) if (sum(is.finite(x), na.rm = TRUE) > 1) stats::sd(x, na.rm = TRUE) else NA_real_
safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, na.rm = TRUE, type = 2))
}

prepare_hh_analysis <- function(hh) {
  hh |>
    dplyr::mutate(
      ctrl = as.integer(group == 1),
      ik = as.integer(group == 2),
      cash = as.integer(group == 3),
      fu = as.integer(etapa == 2),
      ik_fu = ik * fu,
      cash_fu = cash * fu,
      ctrl_fu = ctrl * fu,
      ik_educ = as.integer(o_group == 3),
      ik_no_educ = as.integer(o_group == 2),
      ik_educ_fu = ik_educ * fu,
      ik_no_educ_fu = ik_no_educ * fu,
      pc_exp_n_inkind = pc_exp_food - pc_exp_inkind
    )
}

pal_item_spec <- function() {
  data.frame(
    item = c("corn_fl", "rice", "beans", "pasta", "oil", "milk_pwd", "cookie", "lentil", "can_fish", "cereal"),
    qty = c(3, 2, 2, 1.2, 1, 1.92, 1, 1, .6, .2),
    kcal_kg = c(3630, 3367.5, 3610, 3745, 8808.6, 4940, 4516.7, 1060, 2037.5, 3715.4),
    stringsAsFactors = FALSE
  )
}

pal_summary <- function(hh) {
  spec <- pal_item_spec()
  tag <- hh |>
    dplyr::filter(etapa == 1) |>
    dplyr::arrange(id_loc) |>
    dplyr::distinct(id_loc, .keep_all = TRUE)

  rows <- lapply(seq_len(nrow(spec)), function(j) {
    nm <- spec$item[j]
    pvar <- paste0("b_pr_", nm)
    if (!pvar %in% names(tag)) return(NULL)
    v <- as_num(tag[[pvar]]) * spec$qty[j]
    data.frame(
      item = nm,
      mean_value = safe_mean(v),
      cv = safe_sd(v) / safe_mean(v),
      p25 = safe_quantile(v, .25),
      p75 = safe_quantile(v, .75),
      calorie_share = spec$qty[j] * spec$kcal_kg[j] / sum(spec$qty * spec$kcal_kg)
    )
  })
  out <- dplyr::bind_rows(rows)
  if ("pr_ik" %in% names(tag)) {
    v <- as_num(tag$pr_ik)
    out <- dplyr::bind_rows(out, data.frame(
      item = "PAL basket total",
      mean_value = safe_mean(v),
      cv = safe_sd(v) / safe_mean(v),
      p25 = safe_quantile(v, .25),
      p75 = safe_quantile(v, .75),
      calorie_share = 1
    ))
  }
  write_table_xlsx(out, "pal_food_summary.xlsx", "PAL foods")
  out
}

plot_pal_cdfs <- function(hh) {
  spec <- pal_item_spec()
  d <- hh |> dplyr::filter(ctrl == 0, etapa == 2)
  parts <- list()
  lines <- list()
  for (j in seq_len(nrow(spec))) {
    nm <- spec$item[j]
    v <- paste0("ate_", nm)
    if (!v %in% names(d)) next
    cap <- safe_quantile(as_num(d[[v]]), .95)
    z <- d |>
      dplyr::filter(!is.na(.data[[v]]), .data[[v]] < cap) |>
      dplyr::transmute(value = as_num(.data[[v]]), treatment = factor(ik, levels = c(0, 1), labels = c("Cash", "In-kind")), item = nm)
    parts[[length(parts) + 1]] <- z
    lines[[length(lines) + 1]] <- data.frame(item = nm, transfer = spec$qty[j])
  }
  dat <- dplyr::bind_rows(parts)
  vlines <- dplyr::bind_rows(lines)
  if (!nrow(dat)) return(invisible(NULL))
  p <- ggplot2::ggplot(dat, ggplot2::aes(value, linetype = treatment)) +
    ggplot2::stat_ecdf(geom = "step", na.rm = TRUE) +
    ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = transfer), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~item, scales = "free_x") +
    ggplot2::labs(x = NULL, y = "Empirical CDF", linetype = NULL,
                  caption = "Vertical lines denote in-kind transfer quantities. Treated follow-up households; each distribution is truncated at its 95th percentile.") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(PAPER_DIR, "fig_em_nb.pdf"), p, width = 11, height = 8.5)
  invisible(p)
}

classify_extra_marginal <- function(hh) {
  # Restart-safe equivalent of the revised Stata EM block. On a fresh dataset it
  # gives the same definitions as analysis.do.
  spec <- pal_item_spec()
  old <- grep("(_em$|_emq$|_em_pct$|_emv$)|^(em_N|em_V|b_em_V|pc_em_V|em_V_nomilk_pwd|em_V_pct)$", names(hh), value = TRUE)
  hh <- drop_existing(hh, old)

  em <- character(); emv <- character()
  for (j in seq_len(nrow(spec))) {
    nm <- spec$item[j]
    ate <- paste0("ate_", nm)
    price <- paste0("b_pr_", nm)
    if (!all(c(ate, price) %in% names(hh))) next
    q <- spec$qty[j]
    emn <- paste0(ate, "_em"); emqn <- paste0(ate, "_emq")
    emp <- paste0(ate, "_em_pct"); emvn <- paste0(ate, "_emv")
    hh[[emn]] <- ifelse(!is.na(hh[[ate]]), as.integer(hh[[ate]] < q), NA_integer_)
    hh[[emqn]] <- ifelse(hh[[emn]] == 1, q - hh[[ate]], ifelse(hh[[emn]] == 0, 0, NA_real_))
    hh[[emp]] <- ifelse(hh[[emn]] == 1, hh[[emqn]] / q, NA_real_)
    hh[[emvn]] <- ifelse(hh[[emn]] == 1, hh[[emqn]] * hh[[price]], ifelse(hh[[emn]] == 0, 0, NA_real_))
    em <- c(em, emn); emv <- c(emv, emvn)
  }
  hh$em_N <- row_total(hh, em)
  hh$em_V <- row_total(hh, emv)
  hh$b_em_V <- ifelse(hh$etapa == 1, hh$em_V, NA_real_)
  hh <- fill_within(hh, "cve_viv", "b_em_V", order = "etapa")
  hh$pc_em_V <- hh$em_V / hh$pc_hh
  hh$em_V_nomilk_pwd <- hh$em_V - (hh$ate_milk_pwd_emv %||% 0)
  hh$em_V_pct <- hh$em_V / hh$pr_ik
  hh
}

run_extra_marginal <- function(hh) {
  hh <- classify_extra_marginal(hh)
  spec <- pal_item_spec()
  d <- hh |> dplyr::filter(ctrl == 0, etapa == 2)
  emvars <- existing(d, paste0("ate_", spec$item, "_em"))
  pctvars <- existing(d, paste0("ate_", spec$item, "_em_pct"))

  em_summary <- d |>
    dplyr::group_by(ik) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(emvars), safe_mean, .names = "mean_{.col}"),
      dplyr::across(dplyr::all_of(pctvars), safe_mean, .names = "mean_{.col}"),
      mean_em_N = safe_mean(em_N),
      mean_em_V_pct = safe_mean(em_V_pct),
      .groups = "drop"
    )
  write_table_xlsx(em_summary, "em_nb_summary.xlsx", "EM-NB")

  means <- d |>
    dplyr::group_by(treatment = dplyr::case_when(cash == 1 ~ "Extra-marginal value", ik == 1 ~ "Non-binding value", TRUE ~ "Other")) |>
    dplyr::summarise(mean = safe_mean(em_V_pct), .groups = "drop")
  p <- ggplot2::ggplot(d |> dplyr::filter(cash == 1 | ik == 1), ggplot2::aes(em_V_pct, linetype = factor(ik))) +
    ggplot2::geom_density(na.rm = TRUE, adjust = 1) +
    ggplot2::geom_vline(data = means |> dplyr::filter(treatment != "Other"),
                       ggplot2::aes(xintercept = mean), inherit.aes = FALSE) +
    ggplot2::labs(x = "Percent", y = "Density", linetype = NULL,
                  caption = "Extra-marginal value: cash households. Non-binding value: in-kind households.") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(PAPER_DIR, "fig_em_nb_value.pdf"), p, width = 7, height = 5)

  if (nrow(d) && all(c("em_V_pct", "ik", "id_loc") %in% names(d))) {
    m <- fit_clustered(d, "em_V_pct", "ik", cluster = "id_loc")
    dist <- data.frame(
      cash_mean = safe_mean(d$em_V_pct[d$cash == 1]),
      ik_mean = safe_mean(d$em_V_pct[d$ik == 1]),
      difference = safe_mean(d$em_V_pct[d$cash == 1]) - safe_mean(d$em_V_pct[d$ik == 1]),
      ik_coefficient = if ("ik" %in% names(stats::coef(m))) unname(stats::coef(m)[["ik"]]) else NA_real_,
      mean_pal_basket = safe_mean(hh$pr_ik[hh$group != 1])
    )
    dist$distortion_pesos <- -dist$mean_pal_basket * dist$ik_coefficient
    append_table_sheet(dist, "em_nb_summary.xlsx", "Distortion")
  }
  hh
}

hh_regression_row <- function(model, outcome, pr_ik_cash = NA_real_, pc_hh_ik = NA_real_, pc_hh_cash = NA_real_, pr_ik_ik = NA_real_, label = NULL) {
  tab <- model_rows(model, keep = intersect(c("ik", "cash", "fu", "ik_fu", "cash_fu"), names(stats::coef(model))),
                    model_name = label %||% outcome)
  diff <- lincomb(model, c(ik_fu = 1, cash_fu = -1))
  equal_p <- wald_equal(model, "ik_fu", "cash_fu")
  scale <- pr_ik_cash / 150
  cash_eq <- lincomb(model, c(cash_fu = scale))
  diff_eq <- lincomb(model, c(ik_fu = 1, cash_fu = -scale))
  p_eq <- unname(diff_eq[["p"]])
  mult_ik <- if (is.finite(pc_hh_ik) && is.finite(pr_ik_ik) && pr_ik_ik != 0) lincomb(model, c(ik_fu = pc_hh_ik / pr_ik_ik)) else c(estimate=NA,se=NA,p=NA)
  mult_c <- if (is.finite(pc_hh_cash)) lincomb(model, c(cash_fu = pc_hh_cash / 150)) else c(estimate=NA,se=NA,p=NA)
  extras <- data.frame(
    outcome = outcome,
    att_ik_minus_cash = unname(diff["estimate"]),
    p_ik_eq_cash = equal_p,
    att_eq_cash = unname(cash_eq["estimate"]),
    att_eq_cash_se = unname(cash_eq["se"]),
    att_ik_minus_eq_cash = unname(diff_eq["estimate"]),
    p_ik_eq_eq_cash = p_eq,
    ik_multiplier = unname(mult_ik["estimate"]),
    cash_multiplier = unname(mult_c["estimate"])
  )
  list(coef = tab, stats = extras)
}

run_consumption_analysis <- function(hh) {
  ctrls <- existing(hh, "diconsa")
  if ("mm_ent" %in% names(hh)) ctrls <- c(ctrls, "i(mm_ent)")
  good <- !is.na(hh$special_etapa1) & hh$special_etapa1 != 1
  # In Stata, missing special_etapa1 is not equal to 1 and remains included.
  good <- is.na(hh$special_etapa1) | hh$special_etapa1 != 1

  pr_ik_ik <- safe_mean(hh$pr_ik[hh$etapa == 1 & hh$ik == 1 & good])
  pr_ik_cash <- safe_mean(hh$pr_ik[hh$etapa == 1 & hh$cash == 1 & good])
  pc_hh_ik <- safe_mean(hh$pc_hh[hh$etapa == 2 & hh$ik == 1 & good])
  pc_hh_cash <- safe_mean(hh$pc_hh[hh$etapa == 2 & hh$cash == 1 & good])

  outcomes <- c("exp_total", "exp_food", "exp_inkind", "exp_n_inkind", "exp_nfood")
  all_coef <- list(); all_stats <- list()
  for (v in outcomes) {
    y <- paste0("pc_", v)
    if (!y %in% names(hh)) next
    m <- fit_clustered(hh, y, c("ik", "cash", "fu", "ik_fu", "cash_fu", ctrls), subset = good)
    r <- hh_regression_row(m, v, pr_ik_cash, pc_hh_ik, pc_hh_cash, pr_ik_ik)
    all_coef[[v]] <- r$coef; all_stats[[v]] <- r$stats
  }
  write_table_xlsx(dplyr::bind_rows(all_coef), "cons_agg.xlsx", "coefficients")
  append_table_sheet(dplyr::bind_rows(all_stats), "cons_agg.xlsx", "tests")

  # Robustness: omit no-class education arm (o_group == 2).
  rob_coef <- list(); rob_stats <- list()
  for (v in outcomes) {
    y <- paste0("pc_", v); if (!y %in% names(hh)) next
    sub <- good & (is.na(hh$o_group) | hh$o_group != 2)
    m <- fit_clustered(hh, y, c("ik", "cash", "fu", "ik_fu", "cash_fu", ctrls), subset = sub)
    r <- hh_regression_row(m, v, pr_ik_cash, pc_hh_ik, pc_hh_cash, pr_ik_ik, "o_group != 2")
    rob_coef[[v]] <- r$coef; rob_stats[[v]] <- r$stats
  }
  write_table_xlsx(dplyr::bind_rows(rob_coef), "cons_agg_robust.xlsx", "coefficients")
  append_table_sheet(dplyr::bind_rows(rob_stats), "cons_agg_robust.xlsx", "tests")

  # Robustness: no village controls.
  nc_coef <- list(); nc_stats <- list()
  for (v in outcomes) {
    y <- paste0("pc_", v); if (!y %in% names(hh)) next
    m <- fit_clustered(hh, y, c("ik", "cash", "fu", "ik_fu", "cash_fu"), subset = good)
    r <- hh_regression_row(m, v, pr_ik_cash, pc_hh_ik, pc_hh_cash, pr_ik_ik)
    nc_coef[[v]] <- r$coef; nc_stats[[v]] <- r$stats
  }
  write_table_xlsx(dplyr::bind_rows(nc_coef), "cons_agg_no_ctrls.xlsx", "coefficients")
  append_table_sheet(dplyr::bind_rows(nc_stats), "cons_agg_no_ctrls.xlsx", "tests")

  # Disaggregated categories.
  if (all(c("pc_exp_grn", "pc_exp_pulse") %in% names(hh))) hh$pc_exp_grn_pulse <- hh$pc_exp_grn + hh$pc_exp_pulse
  if (all(c("pc_exp_meat", "pc_exp_dairy") %in% names(hh))) hh$pc_exp_dairy_meat <- hh$pc_exp_meat + hh$pc_exp_dairy
  cats <- c("frvg", "grn_pulse", "corn_fl", "corn_oth", "rice", "pasta", "cookie", "cereal", "oth_wht", "beans", "lentil",
            "dairy_meat", "milk_pwd", "milk_lq", "dairy_oth", "chicken", "beef_pork", "seafood", "can_fish",
            "cookfat", "oil", "cookfat_oth", "junk", "alcohol", "tbc", "sch", "med_hyg", "oth_trans", "clh", "hh_items")
  dis <- list()
  for (nm in cats) {
    y <- paste0("pc_exp_", nm); if (!y %in% names(hh)) next
    m <- fit_clustered(hh, y, c("ik", "cash", "fu", "ik_fu", "cash_fu", ctrls), subset = good)
    scale <- pr_ik_cash / 150
    ceq <- lincomb(m, c(cash_fu = scale))
    dis[[nm]] <- dplyr::bind_cols(
      model_rows(m, keep = intersect("ik_fu", names(stats::coef(m))), model_name = nm),
      data.frame(att_eq_cash = unname(ceq["estimate"]), att_eq_cash_se = unname(ceq["se"]),
                 p_ik_eq_eq_cash = unname(lincomb(m, c(ik_fu = 1, cash_fu = -scale))[["p"]]))
    )
  }
  write_table_xlsx(dplyr::bind_rows(dis), "cons_disagg_new.xlsx", "results")
  invisible(list(pr_ik_ik = pr_ik_ik, pr_ik_cash = pr_ik_cash, pc_hh_ik = pc_hh_ik, pc_hh_cash = pc_hh_cash))
}

run_hh_balance_classes_receipt <- function(hh) {
  good <- is.na(hh$special_etapa1) | hh$special_etapa1 != 1
  bvars <- existing(hh, c("pc_hh", "jefe_educ", "dirt_flr", "indig_hh", "farm", "diconsa", "pr_ik",
                          "pc_exp_food", "pc_exp_nfood", "pc_exp_food_away", "pc_exp_inkind"))
  bal <- list()
  for (v in bvars) {
    m <- fit_clustered(hh, v, c("ctrl", "ik", "cash"), subset = hh$etapa == 1 & good, no_intercept = TRUE)
    z <- model_rows(m, keep = intersect(c("ctrl","ik","cash"), names(stats::coef(m))), model_name = v)
    z$p_ctrl_eq_ik <- wald_equal(m, "ctrl", "ik")
    z$p_ctrl_eq_cash <- wald_equal(m, "ctrl", "cash")
    z$p_ik_eq_cash <- wald_equal(m, "ik", "cash")
    bal[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(bal), "baseline_means.xlsx", "balance")

  hh$noclasses_ik <- as.integer(hh$o_group == 2)
  hh$classes_ik <- as.integer(hh$o_group == 3)
  hh$classes_cash <- as.integer(hh$o_group == 4)
  if ("n_class" %in% names(hh)) {
    hh$n_class_noORG <- hh$n_class
    if ("class_o" %in% names(hh)) hh$n_class_noORG[hh$class_o == 1] <- hh$n_class_noORG[hh$class_o == 1] - 1
    hh$class_noORG <- as.integer(!is.na(hh$n_class_noORG) & hh$n_class_noORG > 1)
  }
  class_vars <- existing(hh, c("class", "class_noORG", "n_class", "n_class_noORG"))
  cl <- list()
  for (v in class_vars) {
    sub <- hh$etapa == 1 & hh$ctrl == 0 & hh$p_pal_f == 1
    m <- fit_clustered(hh, v, c("noclasses_ik", "classes_ik", "classes_cash"), subset = sub, no_intercept = TRUE)
    z <- model_rows(m, keep = intersect(c("noclasses_ik","classes_ik","classes_cash"), names(stats::coef(m))), model_name = v)
    z$p_no_eq_ikclasses <- wald_equal(m, "noclasses_ik", "classes_ik")
    z$p_no_eq_cashclasses <- wald_equal(m, "noclasses_ik", "classes_cash")
    z$p_ikclasses_eq_cashclasses <- wald_equal(m, "classes_ik", "classes_cash")
    cl[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(cl), "classes.xlsx", "classes")

  rec <- list()
  if ("p_pal_f" %in% names(hh)) {
    m <- fit_clustered(hh, "p_pal_f", c("ik","cash"), subset = hh$etapa == 2, no_intercept = TRUE)
    rec[["p_pal_f"]] <- model_rows(m, keep = intersect(c("ik","cash"), names(stats::coef(m))), model_name = "p_pal_f") |>
      dplyr::mutate(p_ik_eq_cash = wald_equal(m, "ik", "cash"))
  }
  for (v in existing(hh, c("npal_pckg", "npal_pckg_exp", "coverage", "on_time"))) {
    m <- fit_clustered(hh, v, c("ik","cash"), subset = hh$p_pal_f == 1 & hh$etapa == 2 & hh$ctrl == 0, no_intercept = TRUE)
    rec[[v]] <- model_rows(m, keep = intersect(c("ik","cash"), names(stats::coef(m))), model_name = v) |>
      dplyr::mutate(p_ik_eq_cash = wald_equal(m, "ik", "cash"))
  }
  write_table_xlsx(dplyr::bind_rows(rec), "pal_receipt.xlsx", "receipt")
  invisible(hh)
}

prepare_individual_analysis <- function(individual, hh) {
  keep_hh <- existing(hh, c("cve_viv", "etapa", "group", "id_loc", "p_pal_f", "special_etapa1", "pr_ik", "diconsa", "cve_ent", "mm_ent"))
  hhs <- hh |> dplyr::select(dplyr::all_of(keep_hh)) |> dplyr::distinct(cve_viv, etapa, .keep_all = TRUE)
  d <- dplyr::inner_join(individual, hhs, by = c("cve_viv", "etapa"), suffix = c("", ".hh"))
  for (nm in c("group","id_loc","p_pal_f","special_etapa1","pr_ik","diconsa","cve_ent","mm_ent")) {
    rhs <- paste0(nm, ".hh")
    if (rhs %in% names(d)) {
      if (!nm %in% names(d)) d[[nm]] <- d[[rhs]]
      else {
        if (is.character(d[[nm]])) {
          z <- d[[nm]]; z[z == ""] <- NA_character_
          u <- d[[rhs]]; if (is.character(u)) u[u==""] <- NA_character_
          d[[nm]] <- dplyr::coalesce(z, u)
        } else d[[nm]] <- dplyr::coalesce(d[[nm]], d[[rhs]])
      }
    }
  }

  # Drop persons whose panel age difference is not 1 or 2 years.
  d <- d |> dplyr::arrange(cve_res, etapa) |>
    dplyr::group_by(cve_res) |>
    dplyr::mutate(age_dif = dplyr::if_else(etapa == 2, age - dplyr::lag(age), NA_real_),
                  bad_age = any(!is.na(age_dif) & (age_dif < 1 | age_dif > 2))) |>
    dplyr::ungroup() |>
    dplyr::filter(!bad_age) |>
    dplyr::select(-age_dif, -bad_age)

  for (v in existing(d, c("vitc","vita","vitb12","ca","prot","fe","zn"))) d[[v]][d$age >= 0 & d$age <= 7 & (d$cal < 200 | d$cal > 2000)] <- NA_real_
  if ("cal" %in% names(d)) d$cal[d$age >= 0 & d$age <= 7 & (d$cal < 200 | d$cal > 2000)] <- NA_real_

  d <- d |> dplyr::arrange(cve_res, etapa) |>
    dplyr::group_by(cve_res) |>
    dplyr::mutate(height_dif = dplyr::if_else(etapa == 2 & dplyr::between(age, 0, 6), height - dplyr::lag(height), NA_real_),
                  height_dif = ifelse(is.na(height_dif), dplyr::last(height_dif), height_dif)) |>
    dplyr::ungroup()
  bad_h <- !is.na(d$height_dif) & d$height_dif < 0 & dplyr::between(d$age, 0, 6)
  if ("height" %in% names(d)) d$height[bad_h] <- NA_real_
  if ("peso" %in% names(d)) d$peso[bad_h] <- NA_real_
  idx <- d$age == 3 & !is.na(d$peso) & abs(d$peso - 140.25) < 1e-8
  d$height[idx] <- NA_real_; d$peso[idx] <- NA_real_
  d$peso[d$age == 0 & dplyr::between(d$peso, 48, 49)] <- NA_real_
  d$peso[d$age == 0 & dplyr::between(d$peso, 80, 81)] <- NA_real_

  d <- d |>
    dplyr::mutate(
      ctrl = as.integer(group == 1), ik = as.integer(group == 2), cash = as.integer(group == 3),
      fu = as.integer(etapa == 2), ctrl_fu = ctrl * fu, ik_fu = ik * fu, cash_fu = cash * fu
    )

  # RDA ratios.
  for (v in existing(d, c("vitc","vita","vitb12","ca","prot","fe","zn"))) {
    rv <- paste0("rda_", v)
    if (rv %in% names(d)) {
      d[[paste0("pct_rda_", v)]] <- d[[v]] / d[[rv]]
      d[[paste0("d_rda_", v)]] <- ifelse(!is.na(d[[paste0("pct_rda_", v)]]), as.integer(d[[paste0("pct_rda_", v)]] > 1), NA_integer_)
    }
  }
  d$pct_rda_cal <- NA_real_
  d$pct_rda_cal[d$age == 0 & dplyr::between(d$age_month, 6, 7)] <- d$cal[d$age == 0 & dplyr::between(d$age_month, 6, 7)] / 650
  d$pct_rda_cal[d$age == 0 & dplyr::between(d$age_month, 8, 11)] <- d$cal[d$age == 0 & dplyr::between(d$age_month, 8, 11)] / 850
  d$pct_rda_cal[dplyr::between(d$age, 1, 3)] <- d$cal[dplyr::between(d$age, 1, 3)] / 1300
  d$pct_rda_cal[dplyr::between(d$age, 4, 6)] <- d$cal[dplyr::between(d$age, 4, 6)] / 1800
  d$pct_rda_cal[d$age >= 12] <- d$cal[d$age >= 12] / 2200
  d$d_rda_cal <- ifelse(!is.na(d$pct_rda_cal), as.integer(d$pct_rda_cal > 1), NA_integer_)

  # Anthropometrics, using CDC 2000 LMS references (US) to mirror zanthro(...,US).
  d$bmi <- d$peso / ((d$height / 100)^2)
  d$zbmi <- cdc_sds(d$bmi, d$age + ifelse(is.na(d$age_month), 0, d$age_month / 12), d$male, "bmi")
  d$bmi_pct <- cdc_bmi_percentile(d$bmi, d$age + ifelse(is.na(d$age_month), 0, d$age_month / 12), d$male)
  d$z_obese <- ifelse(!is.na(d$bmi_pct), as.integer(d$bmi_pct >= .95), NA_integer_)
  d$z_overweight <- ifelse(!is.na(d$bmi_pct), as.integer(d$bmi_pct >= .85), NA_integer_)
  age_dec <- d$age + ifelse(is.na(d$age_month), 0, d$age_month / 12)
  d$zw4a <- cdc_sds(d$peso, age_dec, d$male, "weight")
  d$zh4a <- cdc_sds(d$height, age_dec, d$male, "height")
  d$d_zw4a <- ifelse(!is.na(d$zw4a), as.integer(d$zw4a <= -2), NA_integer_)
  # Preserve the original Stata condition: d_zh4a is defined when zw4a is nonmissing.
  d$d_zh4a <- ifelse(!is.na(d$zw4a), as.integer(d$zh4a <= -2), NA_integer_)
  d$overweight <- ifelse(!is.na(d$zw4a), as.integer(d$zw4a >= 2), NA_integer_)

  bvars <- existing(d, c("age","cal","vitc","vita","vitb12","ca","prot","fe","zn",
                         grep("^d_rda_", names(d), value = TRUE),
                         "bmi","zbmi","z_obese","z_overweight","zw4a","zh4a","overweight"))
  for (v in unique(bvars)) {
    bn <- paste0("b_", v)
    d[[bn]] <- ifelse(d$etapa == 1, d[[v]], NA)
    d <- fill_within(d, "cve_res", bn, order = "etapa")
  }
  d
}

run_individual_balance <- function(d) {
  childvars <- existing(d, c("age","male","cal","vitc","fe","zn","peso","height","sick","d_rda_cal","d_rda_vitc","d_rda_fe","d_rda_zn","sick_days"))
  rows <- list()
  sub <- d$etapa == 1 & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & dplyr::between(d$age, 0, 6)
  for (v in childvars) {
    m <- fit_clustered(d, v, c("ctrl","ik","cash"), subset = sub, no_intercept = TRUE)
    z <- model_rows(m, keep = intersect(c("ctrl","ik","cash"), names(stats::coef(m))), model_name = v)
    z$p_ctrl_eq_ik <- wald_equal(m, "ctrl","ik"); z$p_ctrl_eq_cash <- wald_equal(m, "ctrl","cash"); z$p_ik_eq_cash <- wald_equal(m, "ik","cash")
    rows[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(rows), "baseline_indiv.xlsx", "children")

  wvars <- existing(d, c("age","cal","vitc","fe","zn","peso","height","bmi","sick_days","sick","d_rda_cal","d_rda_vitc","d_rda_fe","d_rda_zn"))
  rows <- list()
  sub <- d$etapa == 1 & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & dplyr::between(d$age, 12, 51) & d$male == 0
  for (v in wvars) {
    m <- fit_clustered(d, v, c("ctrl","ik","cash"), subset = sub, no_intercept = TRUE)
    z <- model_rows(m, keep = intersect(c("ctrl","ik","cash"), names(stats::coef(m))), model_name = v)
    z$p_ctrl_eq_ik <- wald_equal(m, "ctrl","ik"); z$p_ctrl_eq_cash <- wald_equal(m, "ctrl","cash"); z$p_ik_eq_cash <- wald_equal(m, "ik","cash")
    rows[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(rows), "baseline_women.xlsx", "women")
}

trim_by_age <- function(d, var, ages, female = FALSE) {
  out <- rep(NA_real_, nrow(d))
  for (a in ages) {
    base <- d[[var]][d$age == a & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & (!female | d$male == 0)]
    lo <- safe_quantile(base, .01); hi <- safe_quantile(base, .99)
    idx <- d$age == a & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & (!female | d$male == 0) &
      !is.na(d[[var]]) & d[[var]] >= lo & d[[var]] <= hi
    out[idx] <- d[[var]][idx]
  }
  out
}

run_panel_regressions <- function(d, outcomes, subset, filename, controls = TRUE, male_control = TRUE, age_fe = TRUE, pr_ik_cash = NA_real_) {
  ctrls <- character()
  if (male_control && "male" %in% names(d)) ctrls <- c(ctrls, "male")
  if (controls && "diconsa" %in% names(d)) ctrls <- c(ctrls, "diconsa")
  if (controls && "mm_ent" %in% names(d)) ctrls <- c(ctrls, "i(mm_ent)")
  rows <- list()
  for (v in existing(d, outcomes)) {
    rhs <- c("ik_fu","cash_fu","fu","ik","cash",ctrls)
    m <- fit_clustered(d, v, rhs, subset = subset, cluster = "id_loc", fe = if (age_fe) "age" else NULL)
    z <- model_rows(m, keep = intersect(c("ik_fu","cash_fu"), names(stats::coef(m))), model_name = v)
    z$p_ik_eq_cash <- wald_equal(m, "ik_fu","cash_fu")
    if (is.finite(pr_ik_cash)) {
      eq <- lincomb(m, c(cash_fu = pr_ik_cash / 150))
      z$att_eq_cash <- unname(eq["estimate"]); z$att_eq_cash_se <- unname(eq["se"])
      z$p_ik_eq_eq_cash <- unname(lincomb(m, c(ik_fu = 1, cash_fu = -(pr_ik_cash/150)))[["p"]])
    }
    rows[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(rows), filename, "results")
  invisible(rows)
}

plot_health_densities <- function(d) {
  d$peso_trim <- trim_by_age(d, "peso", 0:6)
  d$height_trim <- trim_by_age(d, "height", 0:6)
  sub <- d |> dplyr::filter(fu == 1, dplyr::between(age, 0, 6), ik == 1 | cash == 1 | ctrl == 1) |>
    dplyr::mutate(treatment = factor(group, levels = c(1,2,3), labels = c("Control","In-kind","Cash"))) |>
    dplyr::select(treatment, age, zbmi, peso_trim, height_trim) |>
    tidyr::pivot_longer(c(zbmi,peso_trim,height_trim), names_to = "measure", values_to = "value")
  if (nrow(sub)) {
    p <- ggplot2::ggplot(sub, ggplot2::aes(value, linetype = treatment)) +
      ggplot2::geom_density(na.rm = TRUE) + ggplot2::facet_wrap(~measure, scales = "free") +
      ggplot2::labs(x = NULL, y = "Density", linetype = NULL) + ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(PAPER_DIR, "fig_health_kdensities.pdf"), p, width = 10, height = 4)
  }

  d$peso_trim_w <- trim_by_age(d, "peso", 12:54, female = TRUE)
  d$height_trim_w <- trim_by_age(d, "height", 12:54, female = TRUE)
  subw <- d |> dplyr::filter(fu == 1, dplyr::between(age, 12, 54), male == 0, ik == 1 | cash == 1 | ctrl == 1) |>
    dplyr::mutate(treatment = factor(group, levels = c(1,2,3), labels = c("Control","In-kind","Cash"))) |>
    dplyr::select(treatment, bmi, peso_trim_w, height_trim_w) |>
    tidyr::pivot_longer(c(bmi,peso_trim_w,height_trim_w), names_to = "measure", values_to = "value")
  if (nrow(subw)) {
    p <- ggplot2::ggplot(subw, ggplot2::aes(value, linetype = treatment)) +
      ggplot2::geom_density(na.rm = TRUE) + ggplot2::facet_wrap(~measure, scales = "free") +
      ggplot2::labs(x = NULL, y = "Density", linetype = NULL) + ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(PAPER_DIR, "fig_health_kdensities_women.pdf"), p, width = 10, height = 4)
  }
  d
}

run_individual_analysis <- function(individual, hh) {
  d <- prepare_individual_analysis(individual, hh)
  run_individual_balance(d)
  pr_ik_cash <- {
    tag <- d |> dplyr::filter(etapa == 1, group == 3) |> dplyr::distinct(cve_viv, .keep_all = TRUE)
    safe_mean(tag$pr_ik)
  }

  include_child <- ((dplyr::between(d$age,1,4) & d$etapa==1) | (dplyr::between(d$age,2,6) & d$etapa==2)) &
    (is.na(d$special_etapa1) | d$special_etapa1 != 1)
  run_panel_regressions(d, c("cal","vitc","fe","zn","d_rda_cal","d_rda_vitc","d_rda_fe","d_rda_zn"),
                        include_child, "nutrition.xlsx", pr_ik_cash = pr_ik_cash)

  # Below RDA at baseline.
  below <- list()
  for (v in c("cal","vitc","fe","zn")) {
    bv <- paste0("b_d_rda_", v)
    if (!all(c(v,bv) %in% names(d))) next
    sub <- include_child & d[[bv]] == 0
    rhs <- c("ik_fu","cash_fu","fu","ik","cash","male", existing(d,"diconsa"), if ("mm_ent"%in%names(d)) "i(mm_ent)" else character())
    m <- fit_clustered(d, v, rhs, subset = sub, fe = "age")
    z <- model_rows(m, keep = intersect(c("ik_fu","cash_fu"), names(stats::coef(m))), model_name = v)
    z$p_ik_eq_cash <- wald_equal(m,"ik_fu","cash_fu")
    below[[v]] <- z
  }
  write_table_xlsx(dplyr::bind_rows(below), "nutrition_below_rda.xlsx", "results")

  d <- plot_health_densities(d)
  for (amax in c(6,1)) {
    sub <- (is.na(d$special_etapa1) | d$special_etapa1 != 1) & dplyr::between(d$age,0,amax)
    run_panel_regressions(d, c("peso","height","sick","anemia","zw4a","zh4a","zbmi"), sub,
                          paste0("health_age0_", amax, ".xlsx"), pr_ik_cash = NA_real_)
  }

  # Child health heterogeneity by baseline BMI.
  hetero <- list()
  defs <- list(
    baseline_overweight = !is.na(d$b_z_overweight) & d$b_z_overweight == 1,
    baseline_not_overweight = !is.na(d$b_z_overweight) & d$b_z_overweight == 0,
    baseline_zbmi_pos = !is.na(d$b_zbmi) & d$b_zbmi > 0,
    baseline_zbmi_neg = !is.na(d$b_zbmi) & d$b_zbmi < 0
  )
  for (dn in names(defs)) {
    for (v in existing(d,c("cal","peso","bmi","zw4a"))) {
      sub <- defs[[dn]] & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & dplyr::between(d$age,0,6)
      rhs <- c("ik_fu","cash_fu","fu","ik","cash","male",existing(d,"diconsa"),if("mm_ent"%in%names(d))"i(mm_ent)" else character())
      m <- fit_clustered(d,v,rhs,subset=sub,fe="age")
      z <- model_rows(m,keep=intersect(c("ik_fu","cash_fu"),names(stats::coef(m))),model_name=paste(dn,v,sep=":"))
      z$p_ik_eq_cash <- wald_equal(m,"ik_fu","cash_fu")
      hetero[[paste(dn,v)]] <- z
    }
  }
  write_table_xlsx(dplyr::bind_rows(hetero), "health_hetero_by_bmi.xlsx", "results")

  # Women.
  include_w <- dplyr::between(d$age,12,54) & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & d$male == 0
  run_panel_regressions(d, c("cal","vitc","fe","zn","d_rda_cal","d_rda_vitc","d_rda_fe","d_rda_zn"),
                        include_w, "nutrition_mothers.xlsx", male_control = FALSE, pr_ik_cash = pr_ik_cash)
  # Use women-specific trimmed variables generated above.
  run_panel_regressions(d, c("peso_trim_w","height_trim_w","bmi","sick","anemia","sick_days","hb_adj"),
                        include_w, "health_women.xlsx", male_control = FALSE)

  # Height 12-21 robustness.
  sub <- dplyr::between(d$age,12,21) & (is.na(d$special_etapa1) | d$special_etapa1 != 1) & d$male == 0
  run_panel_regressions(d, c("height_trim_w"), sub, "health_women_height_12_21.xlsx", male_control = FALSE)

  save_processed(d, "individual_analysis")
  invisible(d)
}

make_attrition_panel <- function(hh_attrit) {
  d <- prepare_hh_analysis(hh_attrit)
  counts <- d |> dplyr::count(cve_viv, name = "n_wave")
  d <- dplyr::left_join(d, counts, by = "cve_viv") |>
    dplyr::mutate(attriter = as.integer(n_wave == 1))
  fake <- d |> dplyr::filter(etapa == 1, attriter == 1)
  if (nrow(fake)) {
    fake$etapa <- 2
    fake$fu <- 1; fake$ik_fu <- fake$ik; fake$cash_fu <- fake$cash; fake$ctrl_fu <- fake$ctrl
    d <- dplyr::bind_rows(d, fake) |> dplyr::arrange(cve_viv, etapa)
  }
  for (v in c("exp_total","exp_food","exp_inkind","exp_n_inkind","exp_nfood")) {
    y <- paste0("pc_", v)
    if (!y %in% names(d)) next
    d[[y]][d$etapa == 2 & d$attriter == 1] <- NA_real_
    by <- paste0("b_", y)
    d[[by]] <- ifelse(d$etapa == 1, d[[y]], NA_real_)
    d <- fill_within(d, "cve_viv", by, order = "etapa")
    d[[paste0(y, "_dif")]] <- d[[y]] - d[[by]]
  }
  d
}

run_lee_bounds <- function(hh_attrit, reps_diff = 500, reps_level = 50) {
  d <- make_attrition_panel(hh_attrit)
  outcomes <- c("exp_total","exp_food","exp_inkind","exp_n_inkind","exp_nfood")
  comparisons <- list(
    "IK vs Cash" = list(sub = d$ctrl == 0 & d$etapa == 2, tr = "ik"),
    "IK vs Ctrl" = list(sub = d$cash == 0 & d$etapa == 2, tr = "ik"),
    "Cash vs Ctrl" = list(sub = d$ik == 0 & d$etapa == 2, tr = "cash")
  )
  res <- list(); k <- 0L
  for (tight in list(c("dirt_flr","bath_in"), "diconsa")) {
    tight <- existing(d, tight)
    for (cmp in names(comparisons)) {
      cc <- comparisons[[cmp]]
      dd <- d[which(cc$sub %in% TRUE),,drop=FALSE]
      for (v in outcomes) {
        y <- paste0("pc_",v,"_dif"); if (!y %in% names(dd)) next
        k <- k+1L
        z <- lee_bounds_cluster_boot(dd,y,cc$tr,cluster="id_loc",tight_vars=tight,reps=reps_diff,seed=2012+k)
        z$outcome <- v; z$comparison <- cmp; z$form <- "difference"; z$tightening <- paste(tight,collapse="+")
        res[[k]] <- z
      }
    }
  }
  for (cmp in names(comparisons)) {
    cc <- comparisons[[cmp]]
    dd <- d[which(cc$sub %in% TRUE),,drop=FALSE]
    tight <- existing(dd,c("dirt_flr","bath_in"))
    for (v in outcomes) {
      y <- paste0("pc_",v); if (!y %in% names(dd)) next
      k <- k+1L
      z <- lee_bounds_cluster_boot(dd,y,cc$tr,cluster="id_loc",tight_vars=tight,reps=reps_level,seed=3012+k)
      z$outcome <- v; z$comparison <- cmp; z$form <- "level"; z$tightening <- paste(tight,collapse="+")
      res[[k]] <- z
    }
  }
  out <- dplyr::bind_rows(res)
  write_table_xlsx(out, "cons_agg_leebounds.xlsx", "Lee bounds")
  invisible(out)
}

run_household_analysis <- function(hh) {
  hh <- prepare_hh_analysis(hh)
  pal_summary(hh)
  plot_pal_cdfs(hh)
  hh <- run_extra_marginal(hh)
  run_consumption_analysis(hh)
  run_hh_balance_classes_receipt(hh)
  save_processed(hh, "hh_analysis")
  invisible(hh)
}
