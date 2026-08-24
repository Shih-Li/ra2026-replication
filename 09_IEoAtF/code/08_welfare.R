# 08_welfare.R
# Translation of loan_welfare.do: Tables 10, 11, A11, and A12.
# Implements the original town-cluster bootstrap with firm panels preserved.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("read_stata")) source("code/02_helpers.R")

w <- read_stata(paths$loan_welfare)
require_cols(
  w,
  c("firmid", "survey_town", "round", "type", "treatratio_peer", "newloan",
    "lnpart5revenue", "total_profit", "loanuse", "comploanuse",
    "interpost", "inter4post", "midline", "endline"),
  "loan_welfare.dta"
)
w$inter_untpeer <- (1 - w$type) * w$treatratio_peer

welfare_point <- function(data, sigma, boot = FALSE) {
  fe_id <- if (boot) ".boot_panel" else "firmid"
  cl_id <- if (boot) ".boot_cluster" else "survey_town"
  
  fit_sales <- fit_iv(
    data, "lnpart5revenue",
    exogenous = c("midline", "endline"),
    endogenous = c("loanuse", "comploanuse"),
    instruments = c("interpost", "inter4post"),
    fe = fe_id, cluster = NULL
  )
  fit_profit <- fit_iv(
    data, "total_profit",
    exogenous = c("midline", "endline"),
    endogenous = c("loanuse", "comploanuse"),
    instruments = c("interpost", "inter4post"),
    fe = fe_id, cluster = NULL
  )
  fit_borrow <- fit_ols(
    data, "newloan", c("type", "inter_untpeer"),
    subset = data$round == 3, cluster = cl_id
  )
  
  a <- unname(coef_stat(fit_sales, "loanuse")["estimate"])
  b1 <- unname(coef_stat(fit_profit, "loanuse")["estimate"])
  b2 <- unname(coef_stat(fit_profit, "comploanuse")["estimate"])
  c1 <- unname(coef_stat(fit_borrow, "type")["estimate"])
  c2 <- unname(coef_stat(fit_borrow, "inter_untpeer")["estimate"])
  
  AL <- 29.10
  interest <- 0.0876 - 0.01
  RB <- 359.86
  NF <- 56.926
  exchange <- 6.465
  depreciation2 <- 0.10
  
  w_ps1 <- (b1 + b2) * 1 * c1 / NF
  w_ps2 <- (b1 + b2) * 0.5 * c1 / NF
  w_splor2 <- (b1 + b2) * 0.5 * (1 - 0.5) * c2 / NF +
    a * 0.5 * (1 - 0.5) * c2 * RB / (sigma - 1) / NF
  w_cs1 <- a * 1 * c1 * RB / (sigma - 1) / NF
  w_cs2 <- a * 0.5 * c1 * RB / (sigma - 1) / NF
  
  w_ttl1 <- (b1 + b2) * c1 / NF +
    a * c1 * RB / (sigma - 1) / NF
  w_ttl2 <- (b1 + b2) * 0.5 * c1 / NF +
    (b1 + b2) * 0.5 * 0.5 * c2 / NF +
    a * 0.5 * 0.5 * c2 * RB / (sigma - 1) / NF +
    a * 0.5 * c1 * RB / (sigma - 1) / NF
  
  w_ps1_usd <- round(w_ps1, 4) * NF * 10000 / exchange
  w_ps2_usd <- round(w_ps2, 4) * NF * 10000 / exchange
  w_splor2_usd <- round(w_splor2, 4) * NF * 10000 / exchange
  w_cs1_usd <- round(w_cs1, 4) * NF * 10000 / exchange
  w_cs2_usd <- round(w_cs2, 4) * NF * 10000 / exchange
  w_ttl1_usd <- round(trunc(w_ttl1 * 10000) / 10000, 4) * NF * 10000 / exchange
  w_ttl2_usd <- round(w_ttl2, 4) * NF * 10000 / exchange
  
  yield1 <- b1 / AL + interest
  yield2 <- yield1 + b2 / AL
  yield3 <- yield2 + a * RB / (sigma - 1) / AL
  
  rho <- function(yield, depreciation = depreciation2) {
    if (!is.finite(yield)) return(NA_real_)
    if (yield < 0) return(yield)
    ((4 * yield + 1 + depreciation^2 - 2 * depreciation)^0.5 - 1 - depreciation) / 2
  }
  
  rho1 <- rho(yield1)
  rho2 <- rho(yield2)
  rho3 <- rho(yield3)
  err <- as.integer(any(c(yield1, yield2, yield3) < 0, na.rm = TRUE))
  
  c(
    w_ps1 = w_ps1,
    w_ps1_usd = w_ps1_usd,
    w_ps2 = w_ps2,
    w_ps2_usd = w_ps2_usd,
    w_splor2 = w_splor2,
    w_splor2_usd = w_splor2_usd,
    w_cs1 = w_cs1,
    w_cs1_usd = w_cs1_usd,
    w_cs2 = w_cs2,
    w_cs2_usd = w_cs2_usd,
    w_ttl1 = w_ttl1,
    w_ttl2 = w_ttl2,
    w_ttl1_usd = w_ttl1_usd,
    w_ttl2_usd = w_ttl2_usd,
    r_py_nl_d2 = rho1,
    r_bs_nl_d2 = rho2 - rho1,
    r_cs_nl_d2 = rho3 - rho2,
    r_sr_nl_d2 = rho3,
    r_error = err
  )
}

bc_interval <- function(theta, boot, level = 0.95) {
  z <- boot[is.finite(boot)]
  if (length(z) < 10 || !is.finite(theta)) return(c(low = NA_real_, high = NA_real_))
  p0 <- mean(z < theta)
  p0 <- min(max(p0, 1 / (2 * length(z))), 1 - 1 / (2 * length(z)))
  z0 <- stats::qnorm(p0)
  alpha <- (1 - level) / 2
  probs <- stats::pnorm(2 * z0 + stats::qnorm(c(alpha, 1 - alpha)))
  q <- stats::quantile(z, probs = probs, na.rm = TRUE, names = FALSE, type = 6)
  c(low = q[1], high = q[2])
}

bootstrap_welfare <- function(data, sigma,
                              reps = getOption("ra2026.welfare_reps", 1000L),
                              seed = 12345L) {
  point <- welfare_point(data, sigma, boot = FALSE)
  B <- matrix(
    NA_real_, nrow = reps, ncol = length(point),
    dimnames = list(NULL, names(point))
  )
  
  failed <- rep(FALSE, reps)
  set.seed(seed)
  for (b in seq_len(reps)) {
    bd <- cluster_resample(data, cluster = "survey_town", panel = "firmid")
    val <- try(welfare_point(bd, sigma, boot = TRUE), silent = TRUE)
    if (inherits(val, "try-error")) {
      failed[b] <- TRUE
    } else {
      B[b, names(val)] <- val
    }
  }
  
  summary <- dplyr::bind_rows(lapply(names(point), function(nm) {
    ci <- bc_interval(point[[nm]], B[, nm])
    data.frame(
      sigma = sigma, metric = nm, estimate = point[[nm]],
      std.error = stats::sd(B[, nm], na.rm = TRUE),
      ci_bc_low = ci["low"], ci_bc_high = ci["high"],
      valid_reps = sum(is.finite(B[, nm])),
      reps = reps, stringsAsFactors = FALSE
    )
  }))
  list(point = point, boot = B, summary = summary, failed = failed)
}

boot6 <- bootstrap_welfare(w, sigma = 6, seed = 12345L)
boot4 <- bootstrap_welfare(w, sigma = 4, seed = 12345L)
boot8 <- bootstrap_welfare(w, sigma = 8, seed = 12345L)

all_summary <- dplyr::bind_rows(boot4$summary, boot6$summary, boot8$summary)

metric_row <- function(summary, metric, scale = 1, component, treatment_share, unit) {
  z <- summary[summary$metric == metric, , drop = FALSE]
  if (!nrow(z)) {
    return(data.frame(
      sigma = unique(summary$sigma)[1], component = component,
      treatment_share = treatment_share, unit = unit,
      estimate = NA_real_, std.error = NA_real_, ci_bc_low = NA_real_, ci_bc_high = NA_real_
    ))
  }
  data.frame(
    sigma = z$sigma, component = component, treatment_share = treatment_share, unit = unit,
    estimate = z$estimate * scale, std.error = z$std.error * scale,
    ci_bc_low = z$ci_bc_low * scale, ci_bc_high = z$ci_bc_high * scale
  )
}

welfare_table <- function(summary) {
  dplyr::bind_rows(
    metric_row(summary, "w_ps1", 100, "Producer Surplus", 1.0, "Share of Profit (%)"),
    metric_row(summary, "w_cs1", 100, "Consumer Surplus", 1.0, "Share of Profit (%)"),
    data.frame(sigma = unique(summary$sigma)[1], component = "Spillover", treatment_share = 1.0,
               unit = "Share of Profit (%)", estimate = NA_real_, std.error = NA_real_,
               ci_bc_low = NA_real_, ci_bc_high = NA_real_),
    metric_row(summary, "w_ttl1", 100, "Total", 1.0, "Share of Profit (%)"),
    metric_row(summary, "w_ps1_usd", 1, "Producer Surplus", 1.0, "USD"),
    metric_row(summary, "w_cs1_usd", 1, "Consumer Surplus", 1.0, "USD"),
    data.frame(sigma = unique(summary$sigma)[1], component = "Spillover", treatment_share = 1.0,
               unit = "USD", estimate = NA_real_, std.error = NA_real_,
               ci_bc_low = NA_real_, ci_bc_high = NA_real_),
    metric_row(summary, "w_ttl1_usd", 1, "Total", 1.0, "USD"),
    metric_row(summary, "w_ps2", 100, "Producer Surplus", 0.5, "Share of Profit (%)"),
    metric_row(summary, "w_cs2", 100, "Consumer Surplus", 0.5, "Share of Profit (%)"),
    metric_row(summary, "w_splor2", 100, "Spillover", 0.5, "Share of Profit (%)"),
    metric_row(summary, "w_ttl2", 100, "Total", 0.5, "Share of Profit (%)"),
    metric_row(summary, "w_ps2_usd", 1, "Producer Surplus", 0.5, "USD"),
    metric_row(summary, "w_cs2_usd", 1, "Consumer Surplus", 0.5, "USD"),
    metric_row(summary, "w_splor2_usd", 1, "Spillover", 0.5, "USD"),
    metric_row(summary, "w_ttl2_usd", 1, "Total", 0.5, "USD")
  )
}

return_table <- function(summary) {
  dplyr::bind_rows(
    metric_row(summary, "r_py_nl_d2", 100, "Private Return (%)", NA_real_, "Percent"),
    metric_row(summary, "r_bs_nl_d2", 100, "Business Stealing (pp)", NA_real_, "Percentage points"),
    metric_row(summary, "r_cs_nl_d2", 100, "Consumer Surplus (pp)", NA_real_, "Percentage points"),
    metric_row(summary, "r_sr_nl_d2", 100, "Social Return (%)", NA_real_, "Percent")
  )
}

write_table(welfare_table(boot6$summary), "Table10", "Table 10. Welfare gains, sigma = 6")
write_table(return_table(boot6$summary), "Table11", "Table 11. Return decomposition, sigma = 6")

# The original script bootstraps sigma=4 and sigma=8 for Tables A11/A12.
# Include sigma=6 as the baseline comparison in the tidy R appendix outputs.
write_table(
  dplyr::bind_rows(welfare_table(boot4$summary), welfare_table(boot6$summary), welfare_table(boot8$summary)),
  "TableA11", "Table A11. Welfare sensitivity to sigma"
)
write_table(
  dplyr::bind_rows(return_table(boot4$summary), return_table(boot6$summary), return_table(boot8$summary)),
  "TableA12", "Table A12. Return sensitivity to sigma"
)

error_counts <- dplyr::bind_rows(lapply(list(`4` = boot4, `6` = boot6, `8` = boot8), function(obj) {
  x <- obj$boot[, "r_error"]
  metric_cols <- setdiff(colnames(obj$boot), "r_error")
  valid_by_metric <- vapply(metric_cols, function(nm) sum(is.finite(obj$boot[, nm])), numeric(1))
  data.frame(
    sigma = unique(obj$summary$sigma)[1],
    negative_yield_reps = sum(x == 1, na.rm = TRUE),
    failed_bootstrap_reps = sum(obj$failed),
    min_valid_metric_reps = min(valid_by_metric),
    total_reps = nrow(obj$boot)
  )
}))
write_table(
  error_counts,
  "Welfare-bootstrap-diagnostics",
  "Welfare bootstrap diagnostics (negative-yield draws are valid, not failed replications)"
)
