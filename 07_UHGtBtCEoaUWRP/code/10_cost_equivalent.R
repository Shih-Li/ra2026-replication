# 10_cost_equivalent.R
# Translation of 3.1.CE.do: cost-equivalent regressions and illustrative figures.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
tx <- existing_vars(d, c("treat_HD", "treat_any", "treat_tau"))
all_outcomes <- existing_vars(d, c(primary, secondary_welfare, secondary_wealth, secondary_skills))

res <- purrr::map_dfr(all_outcomes, function(y) {
  m <- fit_model(
    d, y, c(tx, nonempty_vars(d, paste0("L", y)), rhs, blocks),
    weight = "attr_wgt", cluster = "hhid",
    subset = d$round == 1 & d$treat_combined != 1
  )
  e <- extract_terms(m, tx)
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl])
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)
  e
})
res <- apply_q_by_family(res, list(primary = primary, welfare = secondary_welfare, wealth = secondary_wealth, skills = secondary_skills))
write_outputs(dplyr::filter(res, outcome %in% primary), "costequiv_primary.tex")
write_outputs(dplyr::filter(res, outcome %in% secondary), "costequiv_secondary.tex")

# ------------------------------------------------------------------
# Cost-equivalent figures
#
# Reproduce the substantive construction in the Stata figures:
#   - Control
#   - HD actual
#   - four GD arms
#   - fitted GD cost-response line
#   - dashed extrapolation from lowest GD cost to HD cost
#   - estimated GD impact at HD cost
#
# Combined is NOT plotted.
# ------------------------------------------------------------------

make_treatment_cell <- function(x) {
  
  if ("treatment" %in% names(x)) {
    
    lab <- as.character(
      haven::as_factor(
        x$treatment
      )
    )
    
  } else {
    
    lab <- rep(
      NA_character_,
      nrow(x)
    )
    
    lab[
      x$treat_control == 1
    ] <- "Control"
    
    lab[
      x$treat_HD == 1
    ] <- "HD"
  }
  
  if ("gd_treat" %in% names(x)) {
    
    cash <- lab == "Cash"
    
    lab[cash] <- as.character(
      x$gd_treat[cash]
    )
  }
  
  lab
}


d$treatment_cell <- make_treatment_cell(
  d
)

if (
  all(
    c(
      "treat_GD_cost",
      "HD_cost",
      "comb_cost"
    ) %in% names(d)
  )
) {
  
  d$treat_cost <- NA_real_
  
  d$treat_cost[
    d$treat_control == 1
  ] <- 0
  
  if ("treat_GD" %in% names(d)) {
    
    d$treat_cost[
      d$treat_GD == 1
    ] <- d$treat_GD_cost[
      d$treat_GD == 1
    ]
  }
  
  d$treat_cost[
    d$treat_HD == 1
  ] <- d$HD_cost[
    d$treat_HD == 1
  ]
  
  d$treat_cost[
    d$treat_combined == 1
  ] <- d$comb_cost[
    d$treat_combined == 1
  ]
  
  ce_plot <- function(
    y,
    file,
    title = NULL
  ) {
    
    plot_arms <- c(
      "Control",
      "HD",
      "lower",
      "mid",
      "upper",
      "huge"
    )
    
    z <- d |>
      dplyr::filter(
        round == 1,
        treatment_cell %in% plot_arms,
        !is.na(treat_cost)
      ) |>
      dplyr::group_by(
        treatment_cell
      ) |>
      dplyr::summarise(
        outcome = weighted_mean(
          .data[[y]],
          attr_wgt
        ),
        treat_cost = weighted_mean(
          treat_cost,
          attr_wgt
        ),
        samp_size = dplyr::n(),
        .groups = "drop"
      )
    
    gd <- z |>
      dplyr::filter(
        treatment_cell %in%
          c(
            "lower",
            "mid",
            "upper",
            "huge"
          )
      ) |>
      dplyr::arrange(
        treat_cost
      )
    
    if (nrow(gd) != 4) {
      stop(
        "Expected four GD treatment cells for ",
        y,
        "."
      )
    }
    
    # Stata:
    # regr outcome treat_cost if treat_GD==1 [aw=samp_size]
    gd_fit <- stats::lm(
      outcome ~ treat_cost,
      data = gd,
      weights = samp_size
    )
    
    gd$fitted <- as.numeric(
      stats::predict(
        gd_fit,
        newdata = gd
      )
    )
    
    hd_cost <- z$treat_cost[
      z$treatment_cell == "HD"
    ][1]
    
    lower_cost <- z$treat_cost[
      z$treatment_cell == "lower"
    ][1]
    
    if (
      is.na(hd_cost) ||
      is.na(lower_cost)
    ) {
      stop(
        "Could not identify HD/lower treatment costs for ",
        y,
        "."
      )
    }
    
    # Estimated GD impact at HD cost
    hd_hat <- tibble::tibble(
      treat_cost = hd_cost,
      outcome = as.numeric(
        stats::predict(
          gd_fit,
          newdata = data.frame(
            treat_cost = hd_cost
          )
        )
      ),
      series = "Estimated GD impact at HD cost"
    )
    
    # Stata predicts hat_dash only for lower and HD,
    # giving the dashed extrapolation segment.
    extrapolation <- tibble::tibble(
      treat_cost = c(
        lower_cost,
        hd_cost
      )
    )
    
    extrapolation$fitted <- as.numeric(
      stats::predict(
        gd_fit,
        newdata = extrapolation
      )
    )
    
    points <- z |>
      dplyr::mutate(
        series = dplyr::case_when(
          treatment_cell == "Control" ~
            "Control",
          treatment_cell == "HD" ~
            "HD Actual",
          treatment_cell == "lower" ~
            "GD Lower",
          treatment_cell == "mid" ~
            "GD Middle",
          treatment_cell == "upper" ~
            "GD Upper",
          treatment_cell == "huge" ~
            "GD Large",
          TRUE ~ treatment_cell
        )
      )
    
    shape_values <- c(
      "Control" = 16,
      "HD Actual" = 18,
      "GD Lower" = 21,
      "GD Middle" = 22,
      "GD Upper" = 23,
      "GD Large" = 24,
      "Estimated GD impact at HD cost" = 1
    )
    
    p <- ggplot2::ggplot() +
      
      ggplot2::geom_point(
        data = points,
        ggplot2::aes(
          x = treat_cost,
          y = outcome,
          shape = series
        ),
        size = 3
      ) +
      
      ggplot2::geom_point(
        data = hd_hat,
        ggplot2::aes(
          x = treat_cost,
          y = outcome,
          shape = series
        ),
        size = 4,
        stroke = 1.1
      ) +
      
      ggplot2::geom_line(
        data = gd,
        ggplot2::aes(
          x = treat_cost,
          y = fitted,
          linetype = "Fitted in GD treatment"
        ),
        linewidth = 0.9
      ) +
      
      ggplot2::geom_line(
        data = extrapolation,
        ggplot2::aes(
          x = treat_cost,
          y = fitted,
          linetype = "Extrapolation"
        ),
        linewidth = 0.9
      ) +
      
      ggplot2::scale_shape_manual(
        values = shape_values
      ) +
      
      ggplot2::scale_linetype_manual(
        values = c(
          "Fitted in GD treatment" = "solid",
          "Extrapolation" = "dashed"
        )
      ) +
      
      ggplot2::labs(
        x = "Treatment Cost",
        y = var_label(
          d,
          y
        ),
        title = title %||%
          var_label(
            d,
            y
          ),
        shape = NULL,
        linetype = NULL
      ) +
      
      ggplot2::theme_minimal()
    
    save_plot(
      p,
      file
    )
  }
  
  fmap <- c(
    bn_employed =
      "CE_pic_employment.png",
    bn_productive_hrs =
      "CE_pic_productive_hours.png",
    bn_monthly_income =
      "CE_pic_income.png",
    bn_tot_prod_assetval =
      "CE_pic_assets.png",
    hh_month_consumption_pc =
      "CE_pic_cons.png",
    bn_swb =
      "CE_pic_swb.png",
    bn_business_knowledge =
      "CE_pic_bus.png"
  )
  
  for (
    y in intersect(
      names(fmap),
      names(d)
    )
  ) {
    
    ce_plot(
      y,
      fmap[[y]]
    )
  }
}