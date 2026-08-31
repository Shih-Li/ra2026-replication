# 06_heterogeneity_beliefs.R
# Heterogeneous treatment effects by prior beliefs: Table 5 and Table A6.

# -----------------------------------------------------------------------------
# Table 5: any incorrect prior belief
# -----------------------------------------------------------------------------

data <- data %>%
  mutate(
    price_incorrect_dist = ifelse(Q11_1 %in% c(1, 3, 4), 1, 0),
    lump_incorrect_dist = ifelse(Q11_2 %in% c(1, 2, 4), 1, 0),
    price_incorrect_el = ifelse(Q12_1 %in% c(2, 3, 4), 1, 0),
    lump_incorrect_el = ifelse(Q12_2 %in% c(1, 2, 4), 1, 0)
  ) %>%
  mutate(
    T1_price_incorrect_dist = T1 * price_incorrect_dist,
    T2_price_incorrect_el = T2 * price_incorrect_el,
    T3_price_incorrect_dist = T3 * price_incorrect_dist,
    T3_price_incorrect_el = T3 * price_incorrect_el,
    T1_lump_incorrect_dist = T1 * lump_incorrect_dist,
    T2_lump_incorrect_el = T2 * lump_incorrect_el,
    T3_lump_incorrect_dist = T3 * lump_incorrect_dist,
    T3_lump_incorrect_el = T3 * lump_incorrect_el
  )

het1_price <- MASS::polr(
  as.factor(support_price) ~
    T1 + T2 + T3 +
    T1_price_incorrect_dist + T2_price_incorrect_el +
    T3_price_incorrect_dist + T3_price_incorrect_el +
    price_incorrect_dist + price_incorrect_el,
  data = data,
  method = "logistic",
  Hess = TRUE
)

het1_lump <- MASS::polr(
  as.factor(support_lumpsum) ~
    T1 + T2 + T3 +
    T1_lump_incorrect_dist + T2_lump_incorrect_el +
    T3_lump_incorrect_dist + T3_lump_incorrect_el +
    lump_incorrect_dist + lump_incorrect_el,
  data = data,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  het1_price, het1_lump,
  type = "text",
  report = "vcp*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

stargazer::stargazer(
  het1_price, het1_lump,
  type = "html",
  report = "vcp*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE,
  out = result_file("tables", "Table5.html")
)

# -----------------------------------------------------------------------------
# Table A6: heterogeneous effects by type of incorrect belief
# -----------------------------------------------------------------------------

# Interaction variables for price-subsidy analyses.
data <- data %>%
  mutate(
    T1_price_poormore = T1 * price_poormore,
    T1_price_same = T1 * price_same,
    T1_price_dk_dist = T1 * price_dk_dist,
    T2_price_savemore = T2 * price_savemore,
    T2_price_savesame = T2 * price_savesame,
    T2_price_dk_save = T2 * price_dk_save,
    T3_price_poormore = T3 * price_poormore,
    T3_price_same = T3 * price_same,
    T3_price_dk_dist = T3 * price_dk_dist,
    T3_price_savemore = T3 * price_savemore,
    T3_price_savesame = T3 * price_savesame,
    T3_price_dk_save = T3 * price_dk_save
  )

# Interaction variables for lump-sum analyses.
data <- data %>%
  mutate(
    T1_lump_poormore = T1 * lump_poormore,
    T1_lump_richmore = T1 * lump_richmore,
    T1_lump_dk_dist = T1 * lump_dk_dist,
    T2_lump_savemore = T2 * lump_savemore,
    T2_lump_saveless = T2 * lump_saveless,
    T2_lump_dk_save = T2 * lump_dk_save,
    T3_lump_poormore = T3 * lump_poormore,
    T3_lump_richmore = T3 * lump_richmore,
    T3_lump_dk_dist = T3 * lump_dk_dist,
    T3_lump_savemore = T3 * lump_savemore,
    T3_lump_saveless = T3 * lump_saveless,
    T3_lump_dk_save = T3 * lump_dk_save
  )

het2_price <- MASS::polr(
  as.factor(support_price) ~
    T1 + T2 + T3 +
    T1_price_poormore + T1_price_same + T1_price_dk_dist +
    T2_price_savemore + T2_price_savesame + T2_price_dk_save +
    T3_price_poormore + T3_price_same + T3_price_dk_dist +
    T3_price_savemore + T3_price_savesame + T3_price_dk_save +
    price_poormore + price_same + price_dk_dist +
    price_savemore + price_savesame + price_dk_save,
  data = data,
  method = "logistic",
  Hess = TRUE
)

het2_lump <- MASS::polr(
  as.factor(support_lumpsum) ~
    T1 + T2 + T3 +
    T1_lump_poormore + T1_lump_richmore + T1_lump_dk_dist +
    T2_lump_savemore + T2_lump_saveless + T2_lump_dk_save +
    T3_lump_poormore + T3_lump_richmore + T3_lump_dk_dist +
    T3_lump_savemore + T3_lump_saveless + T3_lump_dk_save +
    lump_poormore + lump_richmore + lump_dk_dist +
    lump_savemore + lump_saveless + lump_dk_save,
  data = data,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  het2_price, het2_lump,
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

stargazer::stargazer(
  het2_price, het2_lump,
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE,
  out = result_file("tables", "TableA6.html")
)
