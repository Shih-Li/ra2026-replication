# 14_table3_multinomial_first.R
# Stata Table 3 (original analysis lines ~1550-1615): first encounter with desludger.
# Must run after 07_table2_decision_spillovers.R, matching the Stata dependency.

d <- read_source("multi_first")
sel <- union(
  state_get("selected_table2_since_mechaniz_el"),
  state_get("selected_table2_since_manual_el")
)
treatments <- c("subsidy_high", "spillover", "neighbor_high")
sel <- setdiff(sel, treatments)
sel <- existing_vars(d, sel)
if (!length(sel)) stop("Table 3 has no Table-2-selected controls. Run 07_table2_decision_spillovers.R first.")
casevars <- unique(c(treatments, sel, fe_arrondissement_vars(d)))

fit <- fit_asclogit_r(d, casevars = casevars)
chosen <- d[d$choice == 1, , drop = FALSE]
denom <- nrow(chosen)
pct <- function(a) round(100 * mean(chosen$desludge == a, na.rm = TRUE))
labels <- c(
  `2` = paste0("Manual (", pct(2), "\\%)"),
  `3` = paste0("Mechanized - first (", pct(3), "\\%)"),
  `4` = paste0("Mechanized - not first (", pct(4), "\\%)")
)
write_asclogit_tex(
  fit, result_path("Table3_Multinominal_FirstTime.tex", "table"),
  alt_labels = labels, show_casevars = c("subsidy_high", "neighbor_high")
)
