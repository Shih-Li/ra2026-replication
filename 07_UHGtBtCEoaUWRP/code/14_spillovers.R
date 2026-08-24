# 14_spillovers.R
# Translation of 4.3.Spillovers.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)

# Village identifier.
need_geo <- intersect(c("district", "sector", "cell", "village"), names(d))
if (!length(need_geo)) stop("Village geography variables are missing.")
d$vill_id <- as.integer(do.call(interaction, c(d[need_geo], list(drop = TRUE, lex.order = TRUE))))

d$treat_GD_main_any <- d$treat_GD_main
if ("treat_combined" %in% names(d)) d$treat_GD_main_any[d$treat_combined == 1] <- 1
d$treat_HD_any <- d$treat_HD
if ("treat_combined" %in% names(d)) d$treat_HD_any[d$treat_combined == 1] <- 1

tx <- c("treat_HD_any", "treat_GD_main_any", "treat_GD_huge")
base <- d[d$round == 0, c("youthid", "vill_id", tx, "treat_combined", "HD_complier"), drop = FALSE]

# Leave-one-out village saturations at baseline.
# Leave-one-out village saturations at baseline.
#
# Stata:
#   egen vill_pop = count(youthid) if round==0, by(vill_id)
#
# Construct the village population from a plain numeric vector so that
# `pop` is numeric even when youthid is a labelled/character variable.

pop <- ave(
  rep(1L, nrow(base)),
  base$vill_id,
  FUN = sum
)

pop <- as.numeric(pop)

for (t in c(tx, "treat_combined")) {
  
  # Number assigned to treatment t in each village
  sums <- ave(
    as.numeric(base[[t]]),
    base$vill_id,
    FUN = function(z) {
      sum(z, na.rm = TRUE)
    }
  )
  
  sums <- as.numeric(sums)
  
  # Leave-one-out saturation:
  # subtract the individual's own treatment assignment from
  # the village treatment count, then divide by all other people.
  own_treatment <- ifelse(
    base[[t]] == 1,
    1,
    0
  )
  
  sat <- ifelse(
    pop == 1,
    0,
    (sums - own_treatment) / (pop - 1)
  )
  
  nm <- switch(
    t,
    treat_HD_any = "sat_HD",
    treat_GD_main_any = "sat_GD_main",
    treat_GD_huge = "sat_GD_huge",
    treat_combined = "sat_comb"
  )
  
  base[[nm]] <- sat
}

# Dummy for one-person villages
base$singleton <- as.numeric(
  pop == 1
)
# --------------------------------------------------------------
# Carry baseline village saturation variables AND HD compliance
# to both panel rounds by youthid.
#
# This reproduces the original Stata:
#
#   foreach x in sat_HD sat_GD_main sat_GD_huge sat_comb
#                vill_id HD_complier singleton {
#       egen `x'_m = mean(`x'), by(youthid)
#       drop `x'
#       rename `x'_m `x'
#   }
#
# HD_complier is observed at baseline and must therefore be
# propagated to round 1 before the spillover-on-compliance models.
# --------------------------------------------------------------

carry <- base[
  ,
  c(
    "youthid",
    "vill_id",
    "sat_HD",
    "sat_GD_main",
    "sat_GD_huge",
    "sat_comb",
    "singleton",
    "HD_complier"
  ),
  drop = FALSE
]

# One baseline record per youth.
# This prevents accidental duplication in the panel join.
carry <- carry |>
  dplyr::distinct(
    youthid,
    .keep_all = TRUE
  )

# Remove the original panel versions before attaching the
# baseline values to both rounds.
d <- d |>
  dplyr::select(
    -dplyr::any_of(
      c(
        "vill_id",
        "sat_HD",
        "sat_GD_main",
        "sat_GD_huge",
        "sat_comb",
        "singleton",
        "HD_complier"
      )
    )
  ) |>
  dplyr::left_join(
    carry,
    by = "youthid"
  )

sats <- c("sat_HD", "sat_GD_main", "sat_GD_huge")
for (t in tx) for (s in sats) d[[paste0(s, "_", t)]] <- d[[s]] * d[[t]]
ints <- as.vector(outer(sats, tx, paste, sep = "_"))

# Simple spillover model.
simple <- purrr::map_dfr(existing_vars(d, primary), function(y) {
  m <- fit_model(d, y, c(tx, sats, nonempty_vars(d, paste0("L", y)), rhs, blocks, "singleton"), weight = "attr_wgt", cluster = "vill_id", subset = d$round == 1)
  e <- extract_terms(m, c(tx, sats))
  e$outcome <- y; e$outcome_label <- var_label(d, y)
  ctrl <- d$round == 1 & d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl]); e$n <- stats::nobs(m); e$r2 <- safe_r2(m)
  e$saturation_joint_p <- joint_zero_test(m, sats)
  e
})
simple$q.value <- sharpened_qvalues(simple$p.value)
write_outputs(simple, "saturation_levels.tex")

# Full interaction model, one table per primary outcome.
for (y in existing_vars(d, primary)) {
  m <- fit_model(d, y, c(tx, sats, ints, nonempty_vars(d, paste0("L", y)), rhs, blocks, "singleton"), weight = "attr_wgt", cluster = "vill_id", subset = d$round == 1)
  e <- extract_terms(m, c(tx, sats, ints))
  e$q.value <- sharpened_qvalues(e$p.value)
  e$outcome <- y
  write_outputs(e, paste0("interference_", y, ".tex"))

  # Predicted outcome paths as saturation varies, evaluated at sample means of other regressors.
  grid <- seq(0, 1, by = 0.1)
  for (s in sats) {
    pred <- purrr::map_dfr(c("control", tx), function(status) {
      nd <- d[d$round == 1, , drop = FALSE]
      nd <- nd[rep(1, length(grid)), , drop = FALSE]
      for (v in c(tx, sats, ints)) if (v %in% names(nd)) nd[[v]] <- 0
      nd[[s]] <- grid
      if (status != "control") {
        nd[[status]] <- 1
        iname <- paste0(s, "_", status)
        if (iname %in% names(nd)) nd[[iname]] <- grid
      }
      # Set lag/RHS/block/singleton to weighted sample means.
      controls <- existing_vars(d, c(paste0("L", y), rhs, blocks, "singleton"))
      for (v in controls) nd[[v]] <- weighted_mean(d[[v]][d$round == 1], d$attr_wgt[d$round == 1])
      tibble::tibble(saturation = grid, status = status, prediction = as.numeric(stats::predict(m, newdata = nd)))
    })
    p <- ggplot2::ggplot(pred, ggplot2::aes(saturation, prediction, linetype = status)) +
      ggplot2::geom_line() + ggplot2::labs(x = s, y = var_label(d, y), linetype = "Treatment") + ggplot2::theme_minimal()
    save_plot(p, paste0("interference_", y, "_", sub("sat_", "", s), ".png"))
  }
}

# Saturation density figure.
long_sat <- d[d$round == 0, sats, drop = FALSE] |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "saturation", values_to = "value")
p <- ggplot2::ggplot(long_sat, ggplot2::aes(value)) + ggplot2::geom_density() +
  ggplot2::facet_wrap(~saturation, ncol = 1) + ggplot2::labs(x = "Saturation rate", y = "Density") + ggplot2::theme_minimal()
save_plot(p, "saturation_densities.png", width = 6, height = 8)

# Spillovers on HD compliance, separately in HD and combined arms.
d$treat_HD_pure <- d$treat_HD_any - d$treat_combined
comp <- purrr::map_dfr(c("HD" = "treat_HD_pure", "combined" = "treat_combined"), function(tvar) {
  sub <- d$round == 1 & d[[tvar]] == 1
  m <- fit_model(d, "HD_complier", c(sats, rhs, blocks, "singleton"), weight = "attr_wgt", cluster = "vill_id", subset = sub)
  e <- extract_terms(m, sats)
  e$arm <- tvar; e$mean_compliance <- weighted_mean(d$HD_complier[sub], d$attr_wgt[sub]); e$n <- stats::nobs(m); e$r2 <- safe_r2(m); e$joint_p <- joint_zero_test(m, sats)
  e
})
comp$q.value <- sharpened_qvalues(comp$p.value)
write_outputs(comp, "compliance_saturation.tex")
