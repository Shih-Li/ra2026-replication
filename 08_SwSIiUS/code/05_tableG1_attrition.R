# 05_tableG1_attrition.R
# Stata Table G-1 (original analysis lines ~358-725): survey attrition.

panel_a <- read_source("complete_bl_decider")
pa_left <- balance_panel(panel_a, "in_endline", "Responded to endline survey",
                         "subsidy_low", "subsidy_high", "spillover", TRUE)
pa_right <- balance_panel(panel_a, "in_endline", "Responded to endline survey",
                          "neighbor_high46", "neighbor_high13", "neighbor_high79", FALSE)

panel_b <- read_source("complete_bl_demo")
pb_vars <- c("complete_decr_deci", "in_endline")
pb_labels <- c("Responded to 2nd baseline survey", "Responded to endline survey")
pb_left <- balance_panel(panel_b, pb_vars, pb_labels,
                         "subsidy_low", "subsidy_high", "spillover", TRUE)
pb_right <- balance_panel(panel_b, pb_vars, pb_labels,
                          "neighbor_high46", "neighbor_high13", "neighbor_high79", FALSE)

file <- result_path("TableG1_Attrition.tex", "table")
lines <- c(
  "\\begin{tabular}{lccccccccc}", "\\hline\\hline",
  " & \\multicolumn{4}{c}{Treatments} & \\multicolumn{4}{c}{High-subsidy households in cluster} & Obs. \\\\",
  "Variable & Mean (SD) & HS & SO & joint $p$ & Mean (SD) & HS13 & HS79 & joint $p$ & N \\\\",
  "\\hline"
)
add_panel <- function(lines, title, L, R) {
  lines <- c(lines, paste0("\\multicolumn{10}{l}{\\textit{", title, "}} \\\\"))
  for (i in seq_len(nrow(L))) {
    row <- c(
      L$label[i], fmt_mean_sd(L$mean[i], L$sd[i]),
      fmt_est_se(L$b1[i], L$se1[i]),
      fmt_est_se(L$b2[i], L$se2[i]), fmt_num(L$pjoint[i]),
      fmt_mean_sd(R$mean[i], R$sd[i]),
      fmt_est_se(R$b1[i], R$se1[i]),
      fmt_est_se(R$b2[i], R$se2[i]), fmt_num(R$pjoint[i]), L$N[i]
    )
    lines <- c(lines, paste(row, collapse = " & ") |> paste0(" \\\\"))
  }
  lines
}
lines <- add_panel(lines, "Panel A: Hhds that responded to both baseline surveys", pa_left, pa_right)
lines <- c(lines, "\\hline")
lines <- add_panel(lines, "Panel B: Hhds that responded to first baseline survey", pb_left, pb_right)
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, file)
