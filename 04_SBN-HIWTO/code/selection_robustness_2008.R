# ============================================================================
# Attrition robustness for 2008
# Parallel to: code/selection_robustness_2008.do
# ============================================================================
if (!exists("run_selection_robustness")) source(file.path("code", "config_stata.R"))
d <- readRDS(file.path("data", "processed", "hivtest_mortality.rds"))
run_selection_robustness(
  d, year = "2008", j_max = 8, l_max = 5,
  figure_file = file.path("figures", "figureA2.pdf"),
  benchmark_neg = 0.002, benchmark_pos = -0.183
)
