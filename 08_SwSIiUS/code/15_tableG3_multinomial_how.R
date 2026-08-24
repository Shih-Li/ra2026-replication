# 15_tableG3_multinomial_how.R
# Stata Table G-3 (original analysis lines ~1618-1689): method of finding desludger provider.
# Must run after 07_table2_decision_spillovers.R, matching the Stata dependency.

d <- read_source("multi_how")
sel <- union(
  state_get("selected_table2_since_mechaniz_el"),
  state_get("selected_table2_since_manual_el")
)
treatments <- c("subsidy_high", "spillover", "neighbor_high")
sel <- setdiff(sel, treatments)
sel <- existing_vars(d, sel)
if (!length(sel)) stop("Table G-3 has no Table-2-selected controls. Run 07_table2_decision_spillovers.R first.")
casevars <- unique(c(treatments, sel, fe_arrondissement_vars(d)))

fit <- fit_asclogit_r(d, casevars = casevars)
chosen <- d[d$choice == 1, , drop = FALSE]
pct <- function(a) round(100 * mean(chosen$desludge == a, na.rm = TRUE))
labels <- c(
  `2` = paste0("Manual (", pct(2), "\\%)"),
  `3` = paste0("Mechanized - garage (", pct(3), "\\%)"),
  `4` = paste0("Mechanized - call, flag, or referral (", pct(4), "\\%)"),
  `5` = paste0("Mechanized - other (", pct(5), "\\%)")
)
write_asclogit_tex(
  fit, result_path("TableG3_Multinominal_How.tex", "table"),
  alt_labels = labels, show_casevars = c("subsidy_high", "neighbor_high")
)
