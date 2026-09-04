# ============================================================================
# Attrition robustness for 2018-19
# Parallel to: code/selection_robustness_2018.do
# ============================================================================
if (!exists("run_selection_robustness")) source(file.path("code", "config_stata.R"))
d <- readRDS(file.path("data", "processed", "hivtest_mortality.rds"))
run_selection_robustness(
  d, year = "2018", j_max = 12, l_max = 9,
  figure_file = file.path("figures", "figureA4.pdf"),
  benchmark_neg = 0.006, benchmark_pos = -0.2269666
)
