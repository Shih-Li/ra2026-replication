# Figure 1 and Table A3: treatment take-up.
# Translation of analysis_takeup.do.

survey_takeup <- read_clean("survey_data.dta")


# ============================================================
# Resolve inconsistent variable names in original Stata files
# ============================================================

resolve_takeup_name <- function(data, target, aliases = character()) {
  
  if (target %in% names(data)) {
    return(data)
  }
  
  hit <- aliases[aliases %in% names(data)]
  
  if (length(hit) == 1L) {
    
    data[[target]] <- data[[hit]]
    
    message(
      "  using ", hit,
      " as ", target
    )
    
    return(data)
  }
  
  nearby <- grep(
    paste0("(?i)", gsub("_", ".*", target)),
    names(data),
    value = TRUE,
    perl = TRUE
  )
  
  stop(
    "survey_data.dta does not contain `",
    target,
    "` or its expected alias(es): ",
    paste(aliases, collapse = ", "),
    if (length(nearby)) {
      paste0(
        ". Similar names found: ",
        paste(nearby, collapse = ", ")
      )
    } else {
      ""
    }
  )
}


# account_noncompliance
survey_takeup <- resolve_takeup_name(
  survey_takeup,
  "account_noncompliance",
  "account_noncompliance_base"
)


# disburse_noncompliance
survey_takeup <- resolve_takeup_name(
  survey_takeup,
  "disburse_noncompliance",
  "disburse_noncompliance_base"
)


# ------------------------------------------------------------
# Original analysis_takeup.do calls this variable `disburse_no`,
# but no such variable is generated in the supplied cleaning
# workflow. The coding used in that block corresponds exactly
# to disburse_noncompliance.
# ------------------------------------------------------------

survey_takeup$disburse_no <-
  survey_takeup$disburse_noncompliance

message(
  "  using disburse_noncompliance as disburse_no"
)


# ============================================================
# Original analysis begins
# ============================================================

for (v in c(
  "above_m_median_base",
  "hh_bus_base"
)) {
  
  if (v %in% names(survey_takeup)) {
    
    survey_takeup[[v]][
      is.na(survey_takeup[[v]])
    ] <- 0
  }
}


for (x in c(
  "loan_amount_dis",
  "weekly_profit",
  "earn_business",
  "much_saved",
  "hh_income"
)) {
  
  v <- paste0(x, "_base")
  
  if (v %in% names(survey_takeup)) {
    survey_takeup[[v]] <-
      survey_takeup[[v]] / 1000
  }
}


survey_takeup <-
  survey_takeup |>
  dplyr::filter(disburse_base == 1)


survey_takeup <-
  survey_takeup |>
  dplyr::mutate(
    
    # Mobile Account take-up
    wanted_ma = dplyr::if_else(
      treatment_base == 1,
      0,
      NA_real_
    ),
    
    wanted_ma = dplyr::if_else(
      account_noncompliance == 0,
      1,
      wanted_ma
    ),
    
    # Mobile Disbursement willingness
    wanted_md = dplyr::if_else(
      treatment_base == 2,
      0,
      NA_real_
    ),
    
    wanted_md = dplyr::if_else(
      disburse_noncompliance %in% c(0, 3, 5),
      1,
      wanted_md
    ),
    
    # Compliance classification
    md_compliance = dplyr::case_when(
      
      disburse_no == 0 ~ 1,
      
      disburse_no %in% c(1, 2) ~ 2,
      
      disburse_no == 4 ~ 3,
      
      disburse_no %in% c(3, 5) ~ 4,
      
      TRUE ~ NA_real_
    ),
    
    # SIM-card receipt
    sim_card = 0,
    
    sim_card = 0,
    
    sim_card = dplyr::if_else(
      !is.na(account_noncompliance) &
        account_noncompliance == 0,
      1,
      sim_card
    ),
    
    sim_card = dplyr::if_else(
      !is.na(md_compliance) &
        md_compliance != 2,
      1,
      sim_card
    ),
    
    # Full or partial MD compliance
    md_digital_sim = dplyr::if_else(
      treatment_base == 2,
      0,
      NA_real_
    ),
    
    md_digital_sim = dplyr::if_else(
      md_compliance %in% c(1, 3, 4),
      1,
      md_digital_sim
    ),
    
    # Full MD compliance
    md_digital = dplyr::if_else(
      treatment_base == 2,
      0,
      NA_real_
    ),
    
    md_digital = dplyr::if_else(
      md_compliance == 1,
      1,
      md_digital
    ),
    
    # Cash loan
    cash_loan = dplyr::if_else(
      md_digital == 1,
      0,
      1,
      missing = 1
    )
  )

# ============================================================
# Figure 1: Treatment compliance
# ============================================================
#
# Original Stata:
#
# replace md_digital = 0 if treatment_base != 2
# ciplot cash_loan sim_card md_digital, by(treatment_base) recast(bar)
# graph export ".../takeup.png"
#
# We reproduce the group means and 95% confidence intervals.
# ============================================================

if (RUN_GRAPHS) {
  
  takeup_plot_data <- survey_takeup |>
    dplyr::mutate(
      
      # The Stata graph explicitly sets md_digital = 0
      # outside the Mobile Disbursement treatment arm.
      md_digital_plot = dplyr::if_else(
        treatment_base != 2,
        0,
        md_digital,
        missing = 0
      ),
      
      treatment_label = dplyr::case_when(
        treatment_base == 0 ~ "Control",
        treatment_base == 1 ~ "Mobile Account",
        treatment_base == 2 ~ "Mobile Disburse",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      treatment_base,
      treatment_label,
      cash_loan,
      sim_card,
      md_digital_plot
    ) |>
    tidyr::pivot_longer(
      cols = c(
        cash_loan,
        sim_card,
        md_digital_plot
      ),
      names_to = "outcome",
      values_to = "value"
    ) |>
    dplyr::group_by(
      treatment_base,
      treatment_label,
      outcome
    ) |>
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      n = sum(!is.na(value)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      se = sd / sqrt(n),
      
      critical_value = stats::qt(
        0.975,
        df = pmax(n - 1, 1)
      ),
      
      lower = pmax(
        0,
        mean - critical_value * se
      ),
      
      upper = pmin(
        1,
        mean + critical_value * se
      ),
      
      treatment_label = factor(
        treatment_label,
        levels = c(
          "Control",
          "Mobile Account",
          "Mobile Disburse"
        )
      ),
      
      outcome = factor(
        outcome,
        levels = c(
          "cash_loan",
          "sim_card",
          "md_digital_plot"
        ),
        labels = c(
          "Received loan as cash",
          "Received sim card",
          "Received loan as mobile money"
        )
      )
    )
  
  
  # ----------------------------------------------------------
  # Draw Figure 1
  # ----------------------------------------------------------
  
  p_takeup <- ggplot2::ggplot(
    takeup_plot_data,
    ggplot2::aes(
      x = treatment_label,
      y = mean,
      fill = outcome
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.80),
      width = 0.70
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      position = ggplot2::position_dodge(width = 0.80),
      width = 0.15
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.1)
    ) +
    ggplot2::labs(
      title = "Treatment Compliance",
      x = NULL,
      y = "Proportion",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  
  # ----------------------------------------------------------
  # Save main-text Figure 1
  # ----------------------------------------------------------
  
  save_plot(
    p_takeup,
    file.path(
      OUT_MAIN_FIG,
      "takeup.png"
    ),
    width = 8.5,
    height = 5.2
  )
  
  message(
    "  saved Figure 1: ",
    file.path(OUT_MAIN_FIG, "takeup.png")
  )
}