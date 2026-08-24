# 15_cash_accounting.R
# Translation of 4.4.CashAccounting.do.
# SOURCE NOTE: the Stata accounting loop refers to undefined $cash_tx; the only
# cash-arm list defined in that source is $cashtx. This translation uses that
# defined list explicitly and records the interpretation here rather than silently.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

costs <- read_costs()
rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
tx <- existing_vars(d, c("treat_HD", "treat_GD_lower", "treat_GD_middle", "treat_GD_upper", "treat_GD_huge", "treat_combined"))
cashtx <- c("GD_lower", "GD_middle", "GD_upper", "GD_huge", "combined")

transfers <- c(GD_lower = 317.16, GD_middle = 410.65, GD_upper = 502.96, GD_huge = 750.30, combined = 410.65)
xrate <- 845
months <- 12
financials <- existing_vars(d, c(
  "hh_month_consumption_pc", "bn_tot_prod_assetval", "sav_stock", "debt_stock",
  "hh_livestock_wealth", "hh_lending_ihs", "hh_cashgifts_inflow_ihs",
  "hh_cashgifts_outflow_ihs", "bn_monthly_income"
))

models <- list(); rows <- list()
for (y in financials) {
  m <- fit_model(d, y, c(tx, nonempty_vars(d, paste0("L", y)), rhs, blocks), weight = "attr_wgt", cluster = "hhid", subset = d$round == 1)
  models[[y]] <- m
  e <- extract_terms(m, tx)
  e$outcome <- y; e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl]); e$n <- stats::nobs(m); e$r2 <- safe_r2(m)
  cget <- function(nm) cost_value(costs, sub("^treat_", "", nm))
  e$p_hd_vs_lower_per_dollar <- if (all(c("treat_HD", "treat_GD_lower") %in% tx)) linear_contrast(m, c(treat_HD = 1/cget("treat_HD"), treat_GD_lower = -1/cget("treat_GD_lower")))["p.value"] else NA
  e$p_lower_vs_large_per_dollar <- if (all(c("treat_GD_lower", "treat_GD_huge") %in% tx)) linear_contrast(m, c(treat_GD_lower = 1/cget("treat_GD_lower"), treat_GD_huge = -1/cget("treat_GD_huge")))["p.value"] else NA
  e$p_combined_vs_large_per_dollar <- if (all(c("treat_combined", "treat_GD_huge") %in% tx)) linear_contrast(m, c(treat_combined = 1/cget("treat_combined"), treat_GD_huge = -1/cget("treat_GD_huge")))["p.value"] else NA
  rows[[y]] <- e
}
res <- dplyr::bind_rows(rows)

# The source extracts columns 6:8, corresponding to lending, gifts received, gifts made.
transfer_outcomes <- intersect(c("hh_lending_ihs", "hh_cashgifts_inflow_ihs", "hh_cashgifts_outflow_ihs"), financials)
itt_transfers <- dplyr::filter(res, outcome %in% transfer_outcomes)
itt_transfers$q.value <- sharpened_qvalues(itt_transfers$p.value)
write_outputs(itt_transfers, "itt_transfers.tex")

# Levels versions for accounting.
for (v in financials) d[[paste0(v, "_levels")]] <- sinh(d[[v]]) / xrate
if ("bn_monthly_income_levels" %in% names(d)) d$bn_monthly_income_levels <- d$bn_monthly_income_levels * months
if ("hh_month_consumption_pc_levels" %in% names(d) && "hh_members_aes" %in% names(d)) {
  for (arm in c("control", cashtx)) {
    tvar <- paste0("treat_", arm)
    if (!tvar %in% names(d)) next
    sub <- d$round == 1 & d[[tvar]] == 1
    mhh <- mean(d$hh_members_aes[sub], na.rm = TRUE)
    d$hh_month_consumption_pc_levels[sub] <- d$hh_month_consumption_pc_levels[sub] * mhh * months
  }
}

account_rows <- c("control", cashtx)
accounts <- tibble::tibble(arm = account_rows)
accounts$cash_received <- c(0, unname(transfers[cashtx]))

account_vars <- intersect(c(
  "bn_monthly_income", "hh_cashgifts_inflow_ihs", "hh_month_consumption_pc",
  "bn_tot_prod_assetval", "sav_stock", "debt_stock", "hh_livestock_wealth",
  "hh_lending_ihs", "hh_cashgifts_outflow_ihs"
), financials)

for (y in account_vars) {
  level_var <- paste0(y, "_levels")
  ctrl_mean <- mean(d[[level_var]][d$round == 1 & d$treat_control == 1], na.rm = TRUE)
  vals <- rep(NA_real_, length(account_rows)); vals[1] <- ctrl_mean
  b <- stats::coef(models[[y]])
  for (i in seq_along(cashtx)) {
    term <- paste0("treat_", cashtx[i])
    vals[i + 1] <- if (term %in% names(b)) ctrl_mean * b[[term]] else NA_real_
  }
  accounts[[y]] <- vals
}

getcol <- function(nm) if (nm %in% names(accounts)) accounts[[nm]] else rep(0, nrow(accounts))
accounts$total_inflows <- accounts$cash_received + getcol("bn_monthly_income") + getcol("hh_cashgifts_inflow_ihs")
accounts$total_outflows <- getcol("hh_month_consumption_pc") + getcol("hh_livestock_wealth") + getcol("bn_tot_prod_assetval") +
  getcol("sav_stock") - getcol("debt_stock") + getcol("hh_lending_ihs") + getcol("hh_cashgifts_outflow_ihs")
accounts$share_accounted <- accounts$total_outflows / accounts$total_inflows
# ------------------------------------------------------------------
# Match original Stata table orientation
# ------------------------------------------------------------------

# Original only constructs totals/share for treatment arms.
accounts$total_inflows[
  accounts$arm == "control"
] <- NA_real_

accounts$total_outflows[
  accounts$arm == "control"
] <- NA_real_

accounts$share_accounted[
  accounts$arm == "control"
] <- NA_real_

account_order <- c(
  "cash_received",
  "bn_monthly_income",
  "hh_cashgifts_inflow_ihs",
  "total_inflows",
  "hh_month_consumption_pc",
  "hh_livestock_wealth",
  "bn_tot_prod_assetval",
  "sav_stock",
  "debt_stock",
  "hh_lending_ihs",
  "hh_cashgifts_outflow_ihs",
  "total_outflows",
  "share_accounted"
)

cash_table <- accounts |>
  dplyr::select(
    arm,
    dplyr::all_of(
      account_order
    )
  ) |>
  tidyr::pivot_longer(
    cols = -arm,
    names_to = "item",
    values_to = "value"
  ) |>
  dplyr::mutate(
    item = factor(
      item,
      levels = account_order
    ),
    arm = factor(
      arm,
      levels = c(
        "control",
        "GD_lower",
        "GD_middle",
        "GD_upper",
        "GD_huge",
        "combined"
      )
    )
  ) |>
  dplyr::arrange(
    item,
    arm
  ) |>
  tidyr::pivot_wider(
    names_from = arm,
    values_from = value
  ) |>
  dplyr::arrange(
    item
  ) |>
  dplyr::mutate(
    item = as.character(
      item
    )
  ) |>
  dplyr::rename(
    `Control mean` = control,
    Lower = GD_lower,
    Middle = GD_middle,
    Upper = GD_upper,
    Large = GD_huge,
    Combined = combined
  )

write_outputs(
  cash_table,
  "cash_accounting.tex",
  digits = 2
)
