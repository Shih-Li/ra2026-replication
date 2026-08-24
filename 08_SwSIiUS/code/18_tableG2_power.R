# 18_tableG2_power.R
# Stata Table G-2 (original analysis lines ~1798-1944): ex-post power calculations.
# Requires all earlier numbered scripts because they populate standard-error state entries.

d <- read_source("complete_bl_decider_lasso")

ref_vars <- c("mech_year", "manu_year", "diarrhea_all", "diarrhea_share")
refs <- data.frame(
  variable = ref_vars,
  mean = vapply(ref_vars, function(v) mean(d[[v]], na.rm = TRUE), numeric(1)),
  sd = vapply(ref_vars, function(v) stats::sd(d[[v]], na.rm = TRUE), numeric(1))
)

mde <- function(key) 2.8 * state_get(key)
rows <- list(
  c("Table 2", "(treated households) # high subsidy hhds in cluster",
    mde("since_mechaniz_el_t2_t_se"), mde("since_manual_el_t2_t_se"), NA, NA),
  c("", "(spillover households) # high subsidy hhds in cluster",
    mde("since_mechaniz_el_t2_s_se"), mde("since_manual_el_t2_s_se"), NA, NA),
  c("", "(all households) # high subsidy hhds in cluster",
    mde("since_mechaniz_el_t2_f_se"), mde("since_manual_el_t2_f_se"), NA, NA),
  c("Table 4", "# high subsidy hhds in cluster", NA, NA,
    mde("diarrhea_t4_an_se"), mde("diarrhea_t4_as_se")),
  c("", "# high subsidy hhds in nearest 4", NA, NA,
    mde("diarrhea_t4_nn_se"), mde("diarrhea_t4_ns_se"))
)

for (nw in c("testknow", "nghbrtea", "nghbrldhlth", "nghbrsani", "hhwealthy", "hhnear5")) {
  rows[[length(rows) + 1L]] <- c(
    if (nw == "testknow") "Table 5" else "", paste0("Network: ", nw),
    mde(paste0("since_mech_el_t5_", nw, "_se")), mde(paste0("since_man_el_t5_", nw, "_se")), NA, NA
  )
}
rows[[length(rows) + 1L]] <- c("Table 6", "# signed up first 5 x Public-how-many",
                                mde("since_mechaniz_el_t6_how_se"), mde("since_manual_el_t6_how_se"), NA, NA)
rows[[length(rows) + 1L]] <- c("", "# signed up first 5 x Public-who",
                                mde("since_mechaniz_el_t6_who_se"), mde("since_manual_el_t6_who_se"), NA, NA)
for (nw in c("testknow", "tea", "leadhealth", "sanitation", "wealthy", "near5inc")) {
  rows[[length(rows) + 1L]] <- c(
    if (nw == "testknow") "Table 7" else "", paste0("Public-who network: ", nw),
    mde(paste0("since_mech_el_t7_", nw, "_se")), mde(paste0("since_man_el_t7_", nw, "_se")), NA, NA
  )
}
rows[[length(rows) + 1L]] <- c("Table 8", "High subsidy x Public-price cluster",
                                mde("since_mechaniz_el_t8_se"), mde("since_manual_el_t8_se"), NA, NA)

power <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(power) <- c("Table", "Variables", "used_any_mechanized_desludging", "used_manual_desludging",
                  "no_of_hh_diarrhea", "share_of_hh_diarrhea")
for (v in names(power)[3:6]) power[[v]] <- as.numeric(power[[v]])

# Tab-delimited text equivalent of Ex_post_powercal.txt.
text_file <- result_path("Ex_post_powercal.txt", "table")
ref_out <- data.frame(
  Table = c("Baseline data", ""), Variables = c("Mean", "SD"),
  used_any_mechanized_desludging = c(refs$mean[refs$variable == "mech_year"], refs$sd[refs$variable == "mech_year"]),
  used_manual_desludging = c(refs$mean[refs$variable == "manu_year"], refs$sd[refs$variable == "manu_year"]),
  no_of_hh_diarrhea = c(refs$mean[refs$variable == "diarrhea_all"], refs$sd[refs$variable == "diarrhea_all"]),
  share_of_hh_diarrhea = c(refs$mean[refs$variable == "diarrhea_share"], refs$sd[refs$variable == "diarrhea_share"])
)
utils::write.table(dplyr::bind_rows(ref_out, power), text_file, sep = "\t", row.names = FALSE, quote = FALSE, na = "")

# LaTeX table.
file <- result_path("TableG2_ExPost_PowerCalculations.tex", "table")
lines <- c(
  "\\begin{tabular}{llllll}", "\\hline\\hline",
  " & & (1) & (2) & (3) & (4) \\\\",
  " & & Used Any Mechanized Desludging & Used Manual Desludging & # hhd members diarrhea & Share hhd diarrhea \\\\",
  "\\hline", "\\multicolumn{6}{l}{\\textit{Panel A: Reference points}} \\\\",
  paste(c("Baseline data", "Mean", fmt_num(unlist(ref_out[1, 3:6]))), collapse = " & ") |> paste0(" \\\\"),
  paste(c("", "SD", paste0("(", fmt_num(unlist(ref_out[2, 3:6])), ")")), collapse = " & ") |> paste0(" \\\\"),
  "\\hline", "\\multicolumn{6}{l}{\\textit{Panel B: Ex-post power calculations - Minimum detectable effect sizes (MDE)}} \\\\")
for (i in seq_len(nrow(power))) {
  lines <- c(lines, paste(c(power$Table[i], power$Variables[i], fmt_num(unlist(power[i, 3:6]))), collapse = " & ") |> paste0(" \\\\"))
}
lines <- c(lines, "\\hline\\hline", "\\end{tabular}")
writeLines(lines, file)
