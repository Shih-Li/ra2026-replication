# 06_balance.R
# Translation of 1.2.Balance.do.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("balancevars")) source("code/02_variable_lists.R")
if (!exists("fit_model")) source("code/03_helpers.R")

d <- read_panel()
counts <- table(d$youthid)
panel_ids <- names(counts[counts == 2])
d <- d[d$round == 0 & as.character(d$youthid) %in% panel_ids, , drop = FALSE]

tx <- existing_vars(d, c("treat_HD", "treat_GD_lower", "treat_GD_middle", "treat_GD_upper", "treat_GD_huge", "treat_combined"))
blocks <- block_vars(d)
vars <- existing_vars(d, balancevars)

res <- purrr::map_dfr(vars, function(y) {
  m <- fit_model(d, y, c(tx, blocks), weight = "attr_wgt", cluster = "hhid")
  e <- extract_terms(m, tx)
  e$outcome <- y
  e$outcome_label <- var_label(d, y)
  ctrl <- d$treat_control == 1
  e$control_mean <- weighted_mean(d[[y]][ctrl], d$attr_wgt[ctrl])
  e$n <- stats::nobs(m)
  e$r2 <- safe_r2(m)
  e$joint_p <- joint_zero_test(m, tx)
  e
})
res$q.value <- sharpened_qvalues(res$p.value)
res <- res[, c("outcome", "outcome_label", "term", "estimate", "std.error", "p.value", "q.value", "control_mean", "n", "r2", "joint_p")]
write_outputs(res, "balance.tex")
