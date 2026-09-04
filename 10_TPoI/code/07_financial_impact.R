# 07_financial_impact.R
# Financial-impact correlations in the control group (Table 6) and
# heterogeneous treatment effects by electricity expenditure (Table 7).

# -----------------------------------------------------------------------------
# Table 6
# -----------------------------------------------------------------------------

data <- data %>%
  mutate(
    highinc = ifelse(innt %in% c(5, 6), 1, 0),
    highelexp = ifelse(Q18 %in% c(7, 8), 1, 0),
    hhsize = personer
  )

data_control <- data %>% filter(gruppe == 4)

Priceresp <- MASS::polr(
  as.factor(support_price) ~ highelexp + hhsize + highinc,
  data = data_control,
  method = "logistic",
  Hess = TRUE
)

Lumpresp <- MASS::polr(
  as.factor(support_lumpsum) ~ highelexp + hhsize + highinc,
  data = data_control,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  Priceresp, Lumpresp,
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

stargazer::stargazer(
  Priceresp, Lumpresp,
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  out = result_file("tables", "Table6.html")
)

# -----------------------------------------------------------------------------
# Table 7
# -----------------------------------------------------------------------------

# Electricity expenditures treated as continuous using the source QMD midpoints.
data$elbill <- dplyr::case_when(
  data$Q18 == 1 ~ NA_real_,
  data$Q18 == 2 ~ 250,
  data$Q18 == 3 ~ 750,
  data$Q18 == 4 ~ 1500,
  data$Q18 == 5 ~ 2500,
  data$Q18 == 6 ~ 3500,
  data$Q18 == 7 ~ 4500,
  data$Q18 == 8 ~ 5500,
  data$Q18 == 9 ~ NA_real_
)

het_price_elbill <- MASS::polr(
  as.factor(support_price) ~ T1 * elbill + T2 * elbill + T3 * elbill,
  data = data,
  method = "logistic",
  Hess = TRUE
)

het_lump_elbill <- MASS::polr(
  as.factor(support_lumpsum) ~ T1 * elbill + T2 * elbill + T3 * elbill,
  data = data,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  het_price_elbill, het_lump_elbill,
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

stargazer::stargazer(
  het_price_elbill, het_lump_elbill,
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE,
  out = result_file("tables", "Table7.html")
)
