# 02_data_prep_quality.R
# Data loading, treatment construction, attention checks, Table A2, and Table A3.

message("Loading author-prepared survey data")
full_data <- readRDS(paths$surveydata)

# Treatment dummies for the four randomized groups.
full_data <- full_data %>%
  mutate(
    C  = if_else(gruppe == 4, 1, 0),
    T1 = if_else(gruppe == 1, 1, 0),
    T2 = if_else(gruppe == 2, 1, 0),
    T3 = if_else(gruppe == 3, 1, 0)
  )

# Attention check and final analysis sample.
full_data <- full_data %>%
  mutate(att = if_else(Qattention == 2 & Qattention2 == 4, 1, 0))

data <- subset(full_data, att == 1)

# -----------------------------------------------------------------------------
# Table A2: representativity of the final sample
# -----------------------------------------------------------------------------

kjonn_shares <- data %>%
  group_by(kjonn) %>%
  summarize(
    Count = n(),
    Share = round(n() / nrow(data) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Share))

# The source QMD uses right = FALSE with breaks beginning at 18.
data <- data %>%
  mutate(
    age_group = cut(
      alder,
      breaks = c(18, 30, 40, 50, 60, 70, 150),
      labels = c("18–29", "30–39", "40–49", "50–59", "60–69", "70+"),
      right = FALSE
    )
  )

age_group_shares <- data %>%
  group_by(age_group) %>%
  summarize(
    Count = n(),
    Share = round(n() / nrow(data) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Share))

region_shares <- data %>%
  group_by(fylke) %>%
  summarize(
    Count = n(),
    Share = round(n() / nrow(data) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Share))

print(kjonn_shares)
print(age_group_shares)
print(region_shares)

# The authors' QMD prints Table A2 components but does not save them. The clean
# replication captures those same three components in a workbook for inspection.
writexl::write_xlsx(
  list(
    Gender = as.data.frame(kjonn_shares),
    Age = as.data.frame(age_group_shares),
    Region = as.data.frame(region_shares)
  ),
  result_file("tables", "TableA2.xlsx")
)

# -----------------------------------------------------------------------------
# Table A3: summary statistics and balance checks, full sample
# -----------------------------------------------------------------------------

summarydata <- full_data %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(male = ifelse(kjonn == "Mann", 1, 0))

balance <- summarydata %>%
  dplyr::select(
    gruppe, Q11_1, Q11_2, Q12_1, Q12_2, alder, male, landsdel,
    Q18, UTD, innt, att
  ) %>%
  gtsummary::tbl_summary(
    by = gruppe,
    label = list(
      # The source output displays this row as `male`. Keep that exact label
      # while targeting the selected derived variable, rather than the absent `kjonn`.
      male = "male",
      alder = "Age",
      landsdel = "Region",
      Q18 = "Electricity bill",
      UTD = "Education level",
      innt = "Household gross annual income"
    )
  ) %>%
  gtsummary::add_p() %>%
  gtsummary::modify_header(label = "**Summary statistics and balance checks**")

print(balance)

flextable::save_as_docx(
  gtsummary::as_flex_table(balance),
  path = result_file("tables", "TableA3.docx")
)