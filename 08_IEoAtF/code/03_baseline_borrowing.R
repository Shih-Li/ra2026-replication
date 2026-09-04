# 03_baseline_borrowing.R
# Translation of loan_main.do through Table A4 and Figure 2:
# Table 1, A1, A2, market-structure text statistics, Table 2, A4, Figure 2.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("prepare_loanmain")) source("code/02_helpers.R")

d <- prepare_loanmain()

# ------------------------------------------------------------------
# Table 1: baseline balance
# ------------------------------------------------------------------

treat_vars <- existing_vars(d, c("T50", "C50", "T80", "C80"))
table1_panels <- list(
  A = c(firmage = "Firm age", retail = "Sector-Retail (%)",
        labor = "Number of Employees", total_profit = "Profit (10,000 RMB)",
        part5revenue = "Sales (10,000 RMB)"),
  B = c(gender = "Gender (1=Male, 0=Female)", age = "Age",
        education_college = "Education-College",
        polconnection = "Political Connection (1=Yes, 0=No)"),
  C = c(bankloan = "Other Bank Loan (1=Yes, 0=No)",
        bankloan_amount = "Loan Size (10,000 RMB)",
        bankloan_interest = "Monthly Interest Rate"),
  D = c(num_clients = "Number of Clients", num_supplier = "Number of Suppliers"),
  E = c(flagatt = "Attrition", shutdownall = "Shutdown")
)

t1_rows <- list()
for (panel in names(table1_panels)) {
  vars <- table1_panels[[panel]]
  for (y in names(vars)) {
    if (!y %in% names(d)) next
    mask <- d$round == 1
    if (y %in% c("bankloan_amount", "bankloan_interest") && "bankloan" %in% names(d)) {
      mask <- mask & d$bankloan == 1
    }
    m <- fit_ols(d, y, treat_vars, subset = mask, cluster = "survey_town")
    wanted <- c("(Intercept)", treat_vars)
    z <- tidy_model(m, wanted, model_id = paste0("Panel_", panel), outcome = y)
    z$panel <- panel
    z$outcome_label <- unname(vars[[y]])
    z$column <- dplyr::recode(
      z$term,
      `(Intercept)` = "Pure Control",
      T50 = "Delta Treated 50% Market",
      C50 = "Delta Untreated 50% Market",
      T80 = "Delta Treated 80% Market",
      C80 = "Delta Untreated 80% Market",
      .default = z$term
    )
    z$row_type <- "estimate"
    t1_rows[[length(t1_rows) + 1L]] <- z
  }
}
table1 <- dplyr::bind_rows(t1_rows)

base_controls <- baseline_controls(d)
joint_rows <- lapply(treat_vars, function(y) {
  m <- fit_ols(d, y, base_controls, subset = d$round == 1, cluster = "survey_town")
  data.frame(
    model = "Joint significance", outcome = y, term = "joint_baseline_controls",
    estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
    p.value = joint_wald_p(m, base_controls), n = stats::nobs(m),
    panel = "Bottom", outcome_label = "P-val of Joint Significance of Vars in Panels A-D",
    column = y, row_type = "joint_p", stringsAsFactors = FALSE
  )
})
joint_rows <- dplyr::bind_rows(joint_rows)
joint_rows <- dplyr::bind_rows(
  data.frame(
    model = "Joint significance", outcome = "PureControl", term = "joint_baseline_controls",
    estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
    n = NA_integer_, panel = "Bottom",
    outcome_label = "P-val of Joint Significance of Vars in Panels A-D",
    column = "Pure Control", row_type = "joint_p", stringsAsFactors = FALSE
  ),
  joint_rows
)

count_defs <- list(
  "Pure Control" = d$round == 1 & rowSums(d[, treat_vars, drop = FALSE] == 1, na.rm = TRUE) == 0
)
for (v in treat_vars) count_defs[[v]] <- d$round == 1 & d[[v]] == 1
n_rows <- dplyr::bind_rows(lapply(names(count_defs), function(nm) {
  data.frame(
    model = "Observations", outcome = "N", term = "N", estimate = sum(count_defs[[nm]], na.rm = TRUE),
    std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, n = NA_integer_,
    panel = "Bottom", outcome_label = "Observations", column = nm, row_type = "N",
    stringsAsFactors = FALSE
  )
}))
write_table(dplyr::bind_rows(table1, joint_rows, n_rows), "Table1",
            "Table 1. Baseline balance")

# ------------------------------------------------------------------
# Table A1: market size and employment
# ------------------------------------------------------------------

a1_labor <- d |>
  dplyr::filter(.data$round == 1) |>
  dplyr::group_by(.data$marketcategory) |>
  dplyr::summarise(
    statistic = "Average number of employees",
    value = safe_mean(.data$avgnumlabor),
    n = sum(!is.na(.data$avgnumlabor)),
    .groups = "drop"
  )

a1_firms <- d |>
  dplyr::filter(.data$round == 1) |>
  dplyr::select(dplyr::all_of(c("survey_town", "marketsize", "marketcategory"))) |>
  dplyr::distinct() |>
  dplyr::group_by(.data$marketcategory) |>
  dplyr::summarise(
    statistic = "Average number of firms",
    value = safe_mean(.data$marketsize),
    n = dplyr::n(),
    .groups = "drop"
  )
write_table(dplyr::bind_rows(a1_labor, a1_firms), "TableA1", "Table A1")

# ------------------------------------------------------------------
# Table A2: selection/attrition robustness at baseline
# ------------------------------------------------------------------

a2_outcomes <- existing_vars(d, c("part5revenue", "total_profit", "labor", "num_clients"))
a2_samples <- list(
  retained_no_shutdown = d$shutdownall == 0 & d$flagatt == 0 & d$round == 1,
  observed_endline = d$endatt == 0 & d$round == 1
)

a2_desc <- dplyr::bind_rows(lapply(names(a2_samples), function(sn) {
  mask <- a2_samples[[sn]] & d$survey_town_type == 0
  dplyr::bind_rows(lapply(a2_outcomes, function(y) {
    x <- d[[y]][mask]
    data.frame(sample = sn, outcome = y, mean = safe_mean(x),
               sd = stats::sd(x, na.rm = TRUE), n = sum(!is.na(x)))
  }))
}))

a2_reg <- dplyr::bind_rows(lapply(names(a2_samples), function(sn) {
  mask <- a2_samples[[sn]]
  dplyr::bind_rows(lapply(a2_outcomes, function(y) {
    m <- fit_ols(d, y, treat_vars, subset = mask, cluster = "survey_town")
    model_rows(m, treat_vars, paste0(sn, "_", y), y)
  }))
}))

a2_joint <- dplyr::bind_rows(lapply(names(a2_samples), function(sn) {
  mask <- a2_samples[[sn]]
  dplyr::bind_rows(lapply(treat_vars, function(y) {
    m <- fit_ols(d, y, base_controls, subset = mask, cluster = "survey_town")
    data.frame(sample = sn, assignment = y,
               joint_p.value = joint_wald_p(m, base_controls), n = stats::nobs(m))
  }))
}))
write_table(a2_desc, "TableA2-descriptives", "Table A2 descriptives")
write_table(a2_reg, "TableA2-regressions", "Table A2 regressions")
write_table(a2_joint, "TableA2-joint-tests", "Table A2 joint significance tests")

# ------------------------------------------------------------------
# Text statistics: market structure
# ------------------------------------------------------------------

market_level <- d |>
  dplyr::filter(.data$round == 1) |>
  dplyr::select(dplyr::any_of(c(
    "survey_town", "industry", "townsize", "indsize", "invherf",
    "survey_town_type", "round"
  ))) |>
  dplyr::distinct()

if (nrow(market_level)) {
  market_level$type_market <- as.integer(market_level$survey_town_type != 0)
  market_level <- market_level |>
    dplyr::group_by(.data$survey_town, .data$round) |>
    dplyr::mutate(numind = dplyr::n()) |>
    dplyr::ungroup()

  text_stats <- dplyr::bind_rows(lapply(existing_vars(market_level, c("townsize", "numind", "indsize", "invherf")), function(y) {
    g0 <- market_level[[y]][market_level$type_market == 0]
    g1 <- market_level[[y]][market_level$type_market == 1]
    tt <- try(stats::t.test(g0, g1, var.equal = TRUE), silent = TRUE)
    data.frame(
      variable = y, control_mean = safe_mean(g0), treated_market_mean = safe_mean(g1),
      difference = safe_mean(g1) - safe_mean(g0),
      p.value = if (inherits(tt, "try-error")) NA_real_ else tt$p.value
    )
  }))
  write_table(text_stats, "TableText-market-structure", "Market structure statistics")
}

# ------------------------------------------------------------------
# Table 2: borrowing take-up
# ------------------------------------------------------------------

table2_specs <- list(
  list(id = "newloan_treated", y = "newloan", rhs = c("type")),
  list(id = "newloan_peer", y = "newloan", rhs = c("type", "inter_untpeer")),
  list(id = "newloan_saturation", y = "newloan", rhs = c("T50", "T80", "C50", "C80")),
  list(id = "otherloan_peer", y = "otherloan_end", rhs = c("type", "inter_untpeer")),
  list(id = "otherloan_saturation", y = "otherloan_end", rhs = c("T50", "T80", "C50", "C80")),
  list(id = "allloan_peer", y = "allloan_end", rhs = c("type", "inter_untpeer")),
  list(id = "allloan_saturation", y = "allloan_end", rhs = c("T50", "T80", "C50", "C80"))
)

t2 <- dplyr::bind_rows(lapply(table2_specs, function(s) {
  if (!s$y %in% names(d)) return(NULL)
  rhs <- existing_vars(d, s$rhs)
  m <- fit_ols(d, s$y, rhs, subset = d$round == 3, cluster = "survey_town")
  z <- model_rows(
    m, c(rhs, "(Intercept)"), s$id, s$y,
    control = control_mean(d, s$y, d$round == 3 & d$survey_town_type == 0)
  )
  z
}))
write_table(t2, "Table2", "Table 2. Borrowing")

# ------------------------------------------------------------------
# Table A4: borrowing amounts
# ------------------------------------------------------------------

a4_specs <- list(
  list(id = "RCC_treated", y = "RCC_amount", rhs = c("type")),
  list(id = "RCC_peer", y = "RCC_amount", rhs = c("type", "inter_untpeer")),
  list(id = "RCC_saturation", y = "RCC_amount", rhs = c("T50", "T80", "C50", "C80")),
  list(id = "other_amount_peer", y = "otherloan_amount", rhs = c("type", "inter_untpeer")),
  list(id = "other_amount_saturation", y = "otherloan_amount", rhs = c("T50", "T80", "C50", "C80")),
  list(id = "all_amount_peer", y = "aallloan_end", rhs = c("type", "inter_untpeer")),
  list(id = "all_amount_saturation", y = "aallloan_end", rhs = c("T50", "T80", "C50", "C80"))
)
a4 <- dplyr::bind_rows(lapply(a4_specs, function(s) {
  if (!s$y %in% names(d)) return(NULL)
  rhs <- existing_vars(d, s$rhs)
  m <- fit_ols(d, s$y, rhs, subset = d$round == 3, cluster = "survey_town")
  model_rows(m, rhs, s$id, s$y,
             control = control_mean(d, s$y, d$round == 3 & d$survey_town_type == 0))
}))
write_table(a4, "TableA4", "Table A4")

# ------------------------------------------------------------------
# Figure 2: baseline levels and post-baseline growth distributions
# ------------------------------------------------------------------

group_label <- function(data) {
  dplyr::case_when(
    data$type == 1 ~ "Treated",
    data$type == 0 & data$survey_town_type != 0 ~ "Untreated in Treated Markets",
    data$type == 0 & data$survey_town_type == 0 ~ "Untreated in Control Markets",
    TRUE ~ NA_character_
  )
}

fig2a <- d[d$round == 1 & !is.na(d$lnpart5revenue), , drop = FALSE]
fig2a$group <- group_label(fig2a)
p2a <- ggplot2::ggplot(fig2a, ggplot2::aes(x = .data$lnpart5revenue, linetype = .data$group)) +
  ggplot2::geom_density(na.rm = TRUE) +
  ggplot2::labs(x = "log Sales", y = "Density", linetype = NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

fig2b <- d[d$round > 1 & !is.na(d$avggrowthsales), , drop = FALSE]
fig2b$group <- group_label(fig2b)
p2b <- ggplot2::ggplot(fig2b, ggplot2::aes(x = .data$avggrowthsales, linetype = .data$group)) +
  ggplot2::geom_density(na.rm = TRUE) +
  ggplot2::labs(x = "log Sales Growth", y = "Density", linetype = NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

fig2 <- p2a + p2b + patchwork::plot_layout(ncol = 2, guides = "collect")
save_figure(fig2, "Figure2", width = 12, height = 5)
