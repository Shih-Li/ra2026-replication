# ============================================================================
# Attrition robustness for 2010
# Parallel to: code/selection_robustness_2010.do
# ============================================================================
if (!exists("run_selection_robustness")) source(file.path("code", "config_stata.R"))
d <- readRDS(file.path("data", "processed", "hivtest_mortality.rds"))
run_selection_robustness(
  d, year = "2010", j_max = 7, l_max = 5,
  figure_file = file.path("figures", "figureA3.pdf"),
  benchmark_neg = -0.007, benchmark_pos = -0.231
)
