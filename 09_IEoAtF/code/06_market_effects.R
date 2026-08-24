# 06_market_effects.R
# Translation of loan_main.do Table 6 using market.dta.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("prepare_market")) source("code/02_helpers.R")

mkt <- prepare_market()
require_cols(mkt, c("townid", "round", "survey_town", "survey_town_type"), "market.dta")

rows <- list()

# Market-panel fixed-effect outcomes.
for (y in existing_vars(mkt, c("lnmarketsales", "marketprofit", "lnmarketlabor",
                               "marketshut", "marketrenno", "marketnew"))) {
  rhs <- existing_vars(mkt, c("interpostm", "post"))
  fit <- fit_ols(mkt, y, rhs, cluster = "survey_town", fe = "townid")
  mean_round <- if (y %in% c("marketrenno", "marketnew")) 3 else 1
  z <- model_rows(
    fit, intersect("interpostm", rhs), paste0("Table6_", y), y,
    control = control_mean(mkt, y, mkt$survey_town_type == 0 & mkt$round == mean_round),
    fixed_effects = "Market FE + Post"
  )
  rows[[length(rows) + 1L]] <- z
}

# Weighted endline cross-section.
if ("marketeduc" %in% names(mkt)) {
  rhs <- existing_vars(mkt, c("post", "interpostm"))
  fit <- fit_ols(
    mkt, "marketeduc", rhs, subset = mkt$round == 3,
    cluster = "survey_town", weights = if ("townsize" %in% names(mkt)) "townsize" else NULL
  )
  rows[[length(rows) + 1L]] <- model_rows(
    fit, intersect("interpostm", rhs), "Table6_marketeduc", "marketeduc",
    control = control_mean(mkt, "marketeduc", mkt$survey_town_type == 0 & mkt$round == 3),
    fixed_effects = "No",
    notes = if ("townsize" %in% names(mkt)) "Analytic weights: townsize" else "townsize unavailable"
  )
}

# Endline cross-sectional price and satisfaction outcomes.
for (y in existing_vars(mkt, c("wmarketprice", "marketsat"))) {
  rhs <- existing_vars(mkt, c("interpostm", "post"))
  fit <- fit_ols(mkt, y, rhs, subset = mkt$round == 3, cluster = "survey_town")
  rows[[length(rows) + 1L]] <- model_rows(
    fit, intersect("interpostm", rhs), paste0("Table6_", y), y,
    control = control_mean(mkt, y, mkt$survey_town_type == 0 & mkt$round == 3),
    fixed_effects = "No"
  )
}

write_table(dplyr::bind_rows(rows), "Table6", "Table 6. Market effects")
