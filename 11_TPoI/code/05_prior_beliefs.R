# 05_prior_beliefs.R
# Prior-belief distributions (Table 3A/3B) and their association with policy
# support in the control group (Table 4).

# -----------------------------------------------------------------------------
# Table 3A: beliefs about distribution of compensation
# -----------------------------------------------------------------------------

category_labels <- c(
  "1" = "poor households will receive more",
  "2" = "rich households will receive more",
  "3" = "households of the same size will receive the same",
  "4" = "don't know"
)

get_labeled_distribution <- function(var, scheme_name) {
  counts <- table(factor(var, levels = 1:4))
  shares <- prop.table(counts) * 100
  df <- as.data.frame(shares)
  names(df) <- c("Category", "Share")
  df$Category <- factor(
    df$Category,
    levels = names(category_labels),
    labels = category_labels
  )
  df$Scheme <- scheme_name
  df
}

price_df <- get_labeled_distribution(data$Q11_1, "Price subsidy")
lump_df <- get_labeled_distribution(data$Q11_2, "Lump sum")
combined_df <- rbind(price_df, lump_df)
combined_df$Scheme <- factor(
  combined_df$Scheme,
  levels = c("Price subsidy", "Lump sum")
)

print(combined_df)
writexl::write_xlsx(combined_df, result_file("tables", "Table3A.xlsx"))

# -----------------------------------------------------------------------------
# Table 3B: beliefs about incentives for saving electricity
# -----------------------------------------------------------------------------

incentive_labels <- c(
  "1" = "households will save less electricity",
  "2" = "households will save more electricity",
  "3" = "households will save the same amount",
  "4" = "don't know"
)

get_incentive_distribution <- function(var, scheme_name) {
  counts <- table(factor(var, levels = 1:4))
  shares <- prop.table(counts) * 100
  df <- as.data.frame(shares)
  names(df) <- c("Category", "Share")
  df$Category <- factor(
    df$Category,
    levels = names(incentive_labels),
    labels = incentive_labels
  )
  df$Scheme <- scheme_name
  df
}

price_incentives <- get_incentive_distribution(data$Q12_1, "Price subsidy")
lump_incentives <- get_incentive_distribution(data$Q12_2, "Lump sum")
incentives_df <- rbind(price_incentives, lump_incentives)
incentives_df$Scheme <- factor(
  incentives_df$Scheme,
  levels = c("Price subsidy", "Lump sum")
)

print(incentives_df)
writexl::write_xlsx(incentives_df, result_file("tables", "Table3B.xlsx"))

# -----------------------------------------------------------------------------
# Table 4: policy support and prior beliefs in the control group
# -----------------------------------------------------------------------------

# Correct alternatives are the omitted categories in the source QMD.
data <- data %>%
  mutate(
    price_poormore = ifelse(Q11_1 == 1, 1, 0),
    price_same = ifelse(Q11_1 == 3, 1, 0),
    price_dk_dist = ifelse(Q11_1 == 4, 1, 0),
    lump_poormore = ifelse(Q11_2 == 1, 1, 0),
    lump_richmore = ifelse(Q11_2 == 2, 1, 0),
    lump_dk_dist = ifelse(Q11_2 == 4, 1, 0),
    price_savemore = ifelse(Q12_1 == 2, 1, 0),
    price_savesame = ifelse(Q12_1 == 3, 1, 0),
    price_dk_save = ifelse(Q12_1 == 4, 1, 0),
    lump_savemore = ifelse(Q12_2 == 1, 1, 0),
    lump_saveless = ifelse(Q12_2 == 2, 1, 0),
    lump_dk_save = ifelse(Q12_2 == 4, 1, 0)
  )

data_control <- data %>% filter(gruppe == 4)

Priceprior <- MASS::polr(
  as.factor(support_price) ~
    price_poormore + price_same + price_dk_dist +
    price_savemore + price_savesame + price_dk_save,
  data = data_control,
  method = "logistic",
  Hess = TRUE
)

Lumpprior <- MASS::polr(
  as.factor(support_lumpsum) ~
    lump_poormore + lump_richmore + lump_dk_dist +
    lump_savemore + lump_saveless + lump_dk_save,
  data = data_control,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  Priceprior, Lumpprior,
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001)
)

stargazer::stargazer(
  Priceprior, Lumpprior,
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  out = result_file("tables", "Table4.html")
)
