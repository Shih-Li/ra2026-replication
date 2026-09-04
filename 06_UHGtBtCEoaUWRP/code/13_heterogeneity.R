# 13_heterogeneity.R
# Translation of 4.2.Heterogeneity.do.

if (!exists("paths")) {
  source("code/01_setup.R")
}

if (!exists("primary")) {
  source("code/02_variable_lists.R")
}

if (!exists("fit_model")) {
  source("code/03_helpers.R")
}

d <- read_panel()

tx <- c(
  "treat_HD",
  "treat_GD_main",
  "treat_GD_huge",
  "treat_combined"
)

missing_tx <- setdiff(
  tx,
  names(d)
)

if (length(missing_tx)) {
  stop(
    "Missing treatment variables: ",
    paste(
      missing_tx,
      collapse = ", "
    )
  )
}

blocks <- block_vars(
  d
)

# ------------------------------------------------------------------
# Heterogeneity dimensions
# ------------------------------------------------------------------

# Gender
d$female <- as.numeric(
  d$m1_3gender == 2
)

# Baseline consumption
d$consump <- d$Lhh_month_consumption_pc

# Baseline risk aversion
d$RA <- d$Lbn_risk_aversion

# ------------------------------------------------------------------
# Older indicator
#
# Original Stata generates older and then writes it to both rounds
# using the youth-level mean.
# ------------------------------------------------------------------

older_raw <- ifelse(
  is.na(d$m1_2age),
  NA_real_,
  as.numeric(
    d$m1_2age >= 23
  )
)

older_youth <- tibble::tibble(
  youthid = d$youthid,
  older_raw = older_raw
) |>
  dplyr::group_by(
    youthid
  ) |>
  dplyr::summarise(
    older = if (
      all(is.na(older_raw))
    ) {
      NA_real_
    } else {
      mean(
        older_raw,
        na.rm = TRUE
      )
    },
    .groups = "drop"
  )

d <- d |>
  dplyr::select(
    -dplyr::any_of(
      "older"
    )
  ) |>
  dplyr::left_join(
    older_youth,
    by = "youthid"
  )

# ------------------------------------------------------------------
# Baseline cell employment rate
#
# Stata:
#   egen employ = mean(bn_employed) if round==0, by(cell)
#   demean employ
#   then copy to both rounds by youthid
# ------------------------------------------------------------------

employment_base <- d |>
  dplyr::filter(
    round == 0
  ) |>
  dplyr::group_by(
    cell
  ) |>
  dplyr::mutate(
    employ = mean(
      bn_employed,
      na.rm = TRUE
    )
  ) |>
  dplyr::ungroup()

employment_mean <- mean(
  employment_base$employ,
  na.rm = TRUE
)

employment_youth <- employment_base |>
  dplyr::transmute(
    youthid,
    employ = employ -
      employment_mean
  ) |>
  dplyr::distinct(
    youthid,
    .keep_all = TRUE
  )

d <- d |>
  dplyr::select(
    -dplyr::any_of(
      "employ"
    )
  ) |>
  dplyr::left_join(
    employment_youth,
    by = "youthid"
  )

# ------------------------------------------------------------------
# Demean continuous moderators
#
# employ was already demeaned BEFORE carrying to both rounds,
# matching the Stata order.
# ------------------------------------------------------------------

for (x in c(
  "consump",
  "RA"
)) {
  
  d[[x]] <- d[[x]] -
    mean(
      d[[x]],
      na.rm = TRUE
    )
}

mods <- c(
  "female",
  "consump",
  "RA",
  "employ",
  "older"
)

missing_mods <- setdiff(
  mods,
  names(d)
)

if (length(missing_mods)) {
  stop(
    "Missing heterogeneity moderators: ",
    paste(
      missing_mods,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------------
# Interactions
# ------------------------------------------------------------------

for (x in mods) {
  
  for (t in tx) {
    
    d[[
      paste0(
        t,
        "_",
        x
      )
    ]] <- d[[t]] *
      d[[x]]
  }
}

# ------------------------------------------------------------------
# Regressions
#
# Original heterogeneity specification has NO LASSO RHS controls
# and NO lagged outcome. It includes:
#
# treatment effects
# treatment x moderator
# moderator main effect
# block fixed indicators
# ------------------------------------------------------------------

for (x in mods) {
  
  ints <- paste0(
    tx,
    "_",
    x
  )
  
  terms <- c(
    tx,
    ints,
    x,
    blocks
  )
  
  res <- purrr::map_dfr(
    primary,
    function(y) {
      
      m <- fit_model(
        d,
        y,
        terms,
        weight = "attr_wgt",
        cluster = "hhid",
        subset = d$round == 1
      )
      
      show <- c(
        tx,
        ints,
        x
      )
      
      e <- extract_terms(
        m,
        show
      )
      
      e$outcome <- y
      
      e$outcome_label <- var_label(
        d,
        y
      )
      
      ctrl <- d$round == 1 &
        d$treat_control == 1
      
      e$control_mean <- weighted_mean(
        d[[y]][ctrl],
        d$attr_wgt[ctrl]
      )
      
      e$n <- stats::nobs(
        m
      )
      
      e$r2 <- safe_r2(
        m
      )
      
      e$interaction_joint_p <- joint_zero_test(
        m,
        ints
      )
      
      e
    }
  )
  
  # Original applies sharpened FDR to the complete
  # moderator-specific coefficient matrix.
  res$q.value <- sharpened_qvalues(
    res$p.value
  )
  
  write_outputs(
    res,
    paste0(
      "heterogeneity_",
      x,
      ".tex"
    )
  )
}