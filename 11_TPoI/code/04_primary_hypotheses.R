# 04_primary_hypotheses.R
# Primary ordered-logit tests, multiple-testing adjustment, LPM robustness,
# and Figure 2 panels.

# Support variables with "don't know" omitted, as coded in the source QMD.
data$support_price <- ifelse(data$Q13_1 == 6, NA, data$Q13_1)
data$support_lumpsum <- ifelse(data$Q13_2 == 6, NA, data$Q13_2)

# -----------------------------------------------------------------------------
# Table 2: ordered logistic regression
# -----------------------------------------------------------------------------

H1 <- MASS::polr(
  as.factor(support_price) ~ T1 + T2 + T3,
  data = data,
  method = "logistic",
  Hess = TRUE
)

H2 <- MASS::polr(
  as.factor(support_lumpsum) ~ T1 + T2 + T3,
  data = data,
  method = "logistic",
  Hess = TRUE
)

stargazer::stargazer(
  H1, H2,
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

stargazer::stargazer(
  H1, H2,
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE,
  out = result_file("tables", "Table2.html")
)

# -----------------------------------------------------------------------------
# Table A4: Benjamini-Hochberg adjustment across the six treatment tests
# -----------------------------------------------------------------------------

resH1 <- broom::tidy(H1) %>%
  mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE)) %>%
  filter(term %in% c("T1", "T2", "T3"))

resH2 <- broom::tidy(H2) %>%
  mutate(p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE)) %>%
  filter(term %in% c("T1", "T2", "T3"))

pvalues <- c(resH1$p.value, resH2$p.value)
fdrs <- p.adjust(pvalues, method = "BH")

comparison_table <- data.frame(
  Hypothesis = c("H1: T1", "H1: T2", "H1: T3", "H2: T1", "H2: T2", "H2: T3"),
  Original_Pvalue = round(pvalues, 4),
  Adjusted_Pvalue = round(fdrs, 4)
)

print(comparison_table)
writexl::write_xlsx(comparison_table, result_file("tables", "TableA4.xlsx"))

# -----------------------------------------------------------------------------
# Table A5: linear probability models with HC1 robust standard errors
# -----------------------------------------------------------------------------

binary_data <- data %>%
  mutate(
    support_priceB = case_when(
      Q13_1 %in% c(4, 5) ~ 1,
      Q13_1 %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    support_lumpsumB = case_when(
      Q13_2 %in% c(4, 5) ~ 1,
      Q13_2 %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

H1LPM <- lm(support_priceB ~ T1 + T2 + T3, data = binary_data)
robust_testH1LPM <- lmtest::coeftest(
  H1LPM,
  vcov = sandwich::vcovHC(H1LPM, type = "HC1")
)

# Preserve the coefficient-CI diagnostic calculated in the source QMD.
print(lmtest::coefci(H1LPM, vcov = sandwich::vcovHC(H1LPM, type = "HC1")))
resH1LPM <- broom::tidy(robust_testH1LPM)

H2LPM <- lm(support_lumpsumB ~ T1 + T2 + T3, data = binary_data)
robust_testH2LPM <- lmtest::coeftest(
  H2LPM,
  vcov = sandwich::vcovHC(H2LPM, type = "HC1")
)
resH2LPM <- broom::tidy(robust_testH2LPM)

robustseH1LPM <- resH1LPM$std.error
robustseH2LPM <- resH2LPM$std.error

stargazer::stargazer(
  H1LPM, H2LPM,
  se = list(robustseH1LPM, robustseH2LPM),
  type = "text",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE
)

stargazer::stargazer(
  H1LPM, H2LPM,
  se = list(robustseH1LPM, robustseH2LPM),
  type = "html",
  report = "vc*s",
  align = TRUE,
  p.auto = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE,
  out = result_file("tables", "TableA5.html")
)

# -----------------------------------------------------------------------------
# Figure 2A: price subsidy
# -----------------------------------------------------------------------------

binary_data <- binary_data %>%
  mutate(
    Gruppe = factor(
      case_when(
        gruppe == 1 ~ "Distribution",
        gruppe == 2 ~ "Incentives",
        gruppe == 3 ~ "Combo",
        gruppe == 4 ~ "No info"
      ),
      levels = c("No info", "Distribution", "Incentives", "Combo")
    )
  )

plot_data_price <- binary_data %>%
  filter(Q13_1 != 6) %>%
  mutate(
    Response = factor(
      Q13_1,
      levels = 1:5,
      labels = c(
        "Strongly opposed", "Partially opposed", "Neutral",
        "Partially in favor", "Strongly in favor"
      )
    )
  )

summary_data_price <- binary_data %>%
  group_by(Gruppe) %>%
  summarize(
    Proportion = mean(support_priceB, na.rm = TRUE),
    N = sum(!is.na(support_priceB)),
    SE = sqrt((Proportion * (1 - Proportion)) / N),
    CI_Lower = Proportion - 1.96 * SE,
    CI_Upper = Proportion + 1.96 * SE,
    .groups = "drop"
  )

fig2a <- ggplot(plot_data_price, aes(x = Gruppe, fill = Response)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "lightgreen", "darkgreen")) +
  geom_errorbar(
    data = summary_data_price,
    aes(x = Gruppe, ymin = CI_Lower, ymax = CI_Upper),
    inherit.aes = FALSE,
    color = "black",
    width = 0.15
  ) +
  labs(x = NULL, y = "Percentage", fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 14),
    # Match the authors' original Figure 2A, which suppresses the response legend.
    legend.position = "none"
  )

print(fig2a)
ggsave(
  result_file("figures", "Figure2A.png"),
  plot = fig2a,
  width = 8,
  height = 6,
  dpi = 300
)

# -----------------------------------------------------------------------------
# Figure 2B: lump sum
# -----------------------------------------------------------------------------

plot_data_ls <- binary_data %>%
  filter(Q13_2 != 6) %>%
  mutate(
    Response = factor(
      Q13_2,
      levels = 1:5,
      labels = c(
        "Strongly opposed", "Partially opposed", "Neutral",
        "Partially in favor", "Strongly in favor"
      )
    )
  )

summary_data_ls <- binary_data %>%
  group_by(Gruppe) %>%
  summarize(
    Proportion = mean(support_lumpsumB, na.rm = TRUE),
    N = sum(!is.na(support_lumpsumB)),
    SE = sqrt((Proportion * (1 - Proportion)) / N),
    CI_Lower = Proportion - 1.96 * SE,
    CI_Upper = Proportion + 1.96 * SE,
    .groups = "drop"
  )

fig2b <- ggplot(plot_data_ls, aes(x = Gruppe, fill = Response)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "lightgreen", "darkgreen")) +
  geom_errorbar(
    data = summary_data_ls,
    aes(x = Gruppe, ymin = CI_Lower, ymax = CI_Upper),
    inherit.aes = FALSE,
    color = "black",
    width = 0.15
  ) +
  labs(x = NULL, y = "Percentage", fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 14),
    legend.position = "right"
  )

print(fig2b)
ggsave(
  result_file("figures", "Figure2B.png"),
  plot = fig2b,
  width = 8,
  height = 6,
  dpi = 300
)
