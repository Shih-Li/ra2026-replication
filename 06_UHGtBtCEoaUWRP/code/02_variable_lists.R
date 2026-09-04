# 02_variable_lists.R
# Direct translation of Scripts/do/VariableLists.do.

primary <- c(
  "bn_employed",
  "bn_productive_hrs",
  "bn_monthly_income",
  "bn_tot_prod_assetval",
  "hh_month_consumption_pc"
)

secondary_welfare <- c("bn_swb", "bn_mental_health", "bn_consumption")
secondary_wealth <- c("hh_net_wealth", "hh_livestock_wealth", "sav_stock", "debt_stock")
secondary_skills <- c(
  "bn_loc", "bn_alt_aspiration", "bn_big_five",
  "bn_business_knowledge", "bn_business_attitude"
)
secondary <- c(secondary_welfare, secondary_wealth, secondary_skills)

secondary_baseline <- c("sav_stock", "debt_stock", "bn_consumption", "hh_livestock_wealth")

balancevars <- c(
  "ubudehe1", "bn_female", "bn_age", "bn_edyrs", "hh_members",
  "bn_employed", "bn_productive_hrs", "bn_monthly_income",
  "bn_tot_prod_assetval", "hh_month_consumption_pc", "bn_consumption",
  "hh_net_wealth", "sav_stock", "debt_stock", "hh_livestock_wealth",
  "bn_busiknowledge"
)

covars_short <- c(
  "bn_female", "bn_age", "bn_educ", "bn_employed",
  "hh_members", "hh_asset_val", "bn_risk_aversion"
)

uptake <- c("HD_complier", "completed_WRN", "completed_BYOB", "completed_TT")
