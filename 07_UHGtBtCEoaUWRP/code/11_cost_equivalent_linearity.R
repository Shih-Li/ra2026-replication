# 11_cost_equivalent_linearity.R
# Translation of 3.2.CE_TestLinearity.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("primary")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

rhs <- rhs_controls(required = TRUE)
d <- read_panel()
blocks <- block_vars(d)
if ("treat_tau" %in% names(d)) {
  d$treat_tau_sq <- d$treat_tau^2
  d$treat_tau_cu <- d$treat_tau^3
}
tx <- existing_vars(d, c("treat_HD", "treat_any", "treat_tau"))

run_linearity <- function(outcomes) {
  specs <- list(
    Base_Linear = list(extra = character(), drop = NULL),
    Quadratic = list(extra = "treat_tau_sq", drop = NULL),
    Cubic = list(extra = c("treat_tau_sq", "treat_tau_cu"), drop = NULL),
    Drop_lower = list(extra = character(), drop = "lower"),
    Drop_mid = list(extra = character(), drop = "mid"),
    Drop_upper = list(extra = character(), drop = "upper"),
    Drop_huge = list(extra = character(), drop = "huge")
  )
  purrr::map_dfr(outcomes, function(y) {
    purrr::imap_dfr(specs, function(sp, nm) {
      sub <- d$round == 1 & d$treat_combined != 1
      if (!is.null(sp$drop) && "gd_treat" %in% names(d)) sub <- sub & as.character(d$gd_treat) != sp$drop
      m <- fit_model(d, y, c(tx, sp$extra, nonempty_vars(d, paste0("L", y)), rhs, blocks), weight = "attr_wgt", cluster = "hhid", subset = sub)
      e <- extract_terms(m, "treat_HD")
      tibble::tibble(outcome = y, outcome_label = var_label(d, y), specification = nm,
                     estimate = e$estimate[1], std.error = e$std.error[1], p.value = e$p.value[1])
    })
  })
}

r1 <- run_linearity(existing_vars(d, primary))
r2 <- run_linearity(existing_vars(d, secondary))
write_outputs(r1, "costequiv_linearity_primary.tex")
write_outputs(r2, "costequiv_linearity_secondary.tex")
