# 04_table1_balance.R
# Stata Table 1 (original analysis lines ~78-355): randomization balance.

d <- read_source("complete_bl_decider")

vars <- c(
  "deci_male", "deci_age", "deci_edu_level", "hh_size", "wealth_ind1_bl",
  "own_house", "twostories", "num_rooms", "house_clean_always_bl", "mech_year", "manu_year"
)
labels <- c(
  "Respondent male", "Respondent age", "Respondent years of education", "Household size",
  "Wealth index", "Own their house", "House has two stories", "Number of rooms in house",
  "Courtyard looks clean", "Used mechanized in year before bl", "Used manual in year before bl"
)

left <- balance_panel(d, vars, labels, "subsidy_low", "subsidy_high", "spillover", absorb_cluster = TRUE)
right <- balance_panel(d, vars, labels, "neighbor_high46", "neighbor_high13", "neighbor_high79", absorb_cluster = FALSE)

# The final Stata row reports p-values from reverse regressions testing all balance covariates jointly.
left_joint <- data.frame(
  label = "$p$-value of joint F-test", mean = NA_real_, sd = NA_real_,
  b1 = reverse_balance_p(d, "subsidy_high", "subsidy_low", vars, TRUE), se1 = NA_real_,
  b2 = reverse_balance_p(d, "spillover", "subsidy_low", vars, TRUE), se2 = NA_real_,
  pjoint = NA_real_, N = NA_real_
)
right_joint <- data.frame(
  label = "$p$-value of joint F-test", mean = NA_real_, sd = NA_real_,
  b1 = reverse_balance_p(d, "neighbor_high13", "neighbor_high46", vars, FALSE), se1 = NA_real_,
  b2 = reverse_balance_p(d, "neighbor_high79", "neighbor_high46", vars, FALSE), se2 = NA_real_,
  pjoint = NA_real_, N = NA_real_
)

write_balance_tex(
  dplyr::bind_rows(left, left_joint), dplyr::bind_rows(right, right_joint),
  result_path("Table1_RandomizationBalance.tex", "table")
)
