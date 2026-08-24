# 07_local_spillovers_iv.R
# Translation of loan_main.do: Tables 7, 8, A10, and 9.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("prepare_loanmain")) source("code/02_helpers.R")

d <- prepare_loanmain()

local_share_terms <- existing_vars(
  d, c("treatratio_lcomp", "treatratio_lnoncomp",
       "treatratio_nonlcomp", "treatratio_nonlnoncomp")
)
local_dummy_terms <- existing_vars(
  d, c("dummy_lcomp", "dummy_lnoncomp", "dummy_nonlcomp", "dummy_nonlnoncomp")
)

# ------------------------------------------------------------------
# Table 7: borrowing by local/non-local exposure
# ------------------------------------------------------------------

t7 <- dplyr::bind_rows(lapply(c(1, 0), function(treated) {
  rhs <- c(local_share_terms, local_dummy_terms)
  fit <- fit_ols(
    d, "newloan", rhs,
    subset = d$round == 3 & d$type == treated,
    cluster = "survey_town"
  )
  model_rows(
    fit, local_share_terms,
    paste0("Table7_", ifelse(treated == 1, "treated", "untreated")),
    "newloan",
    control = control_mean(d, "newloan", d$round == 1 & d$survey_town_type == 0),
    fixed_effects = "No",
    notes = ifelse(treated == 1, "Treated firms", "Untreated firms")
  )
}))
write_table(t7, "Table7", "Table 7. Borrowing and local exposure")

# ------------------------------------------------------------------
# Table 8: firm outcomes and local/non-local spillovers
# ------------------------------------------------------------------

local_inter_terms <- existing_vars(d, c("localinter1", "localinter2", "localinter5", "localinter6"))
local_controls <- existing_vars(
  d, c("post", "pldummy1", "pldummy2", "pldummy3", "pldummy4", "localinter13")
)
t8_outcomes <- existing_vars(d, c("lnpart5revenue", "total_profit", "lnlabor"))

t8_rows <- list()
for (sample_name in c("all_firms", "treated_and_pure_control")) {
  mask <- if (sample_name == "all_firms") {
    rep(TRUE, nrow(d))
  } else {
    d$type == 1 | d$survey_town_type == 0
  }

  for (y in t8_outcomes) {
    rhs <- c("interpost", local_inter_terms, local_controls)
    fit <- fit_ols(d, y, rhs, subset = mask, cluster = "survey_town", fe = "firmid")
    cmask <- if (sample_name == "all_firms") {
      d$round == 1 & d$survey_town_type == 0
    } else {
      d$round == 1 & (d$type == 1 | d$survey_town_type == 0)
    }
    z <- model_rows(
      fit, intersect(c("interpost", local_inter_terms), rhs),
      paste0("Table8_", sample_name, "_", y), y,
      control = control_mean(d, y, cmask),
      fixed_effects = "Firm FE + Post"
    )
    z$sample <- sample_name
    t8_rows[[length(t8_rows) + 1L]] <- z
  }
}
write_table(dplyr::bind_rows(t8_rows), "Table8", "Table 8. Local and non-local spillovers")

# ------------------------------------------------------------------
# Table A10: local robustness
# ------------------------------------------------------------------

a10_rows <- list()
a10_controls <- c(
  existing_vars(d, c("type", "pldummy1", "pldummy2", "pldummy3", "pldummy4", "indsize")),
  endline_controls(d)
)

for (y in existing_vars(d, c("zconeval_squality", "zconeval_env", "zconeval_money", "zconeval_all"))) {
  rhs <- c("type", local_share_terms, a10_controls)
  rhs <- unique(existing_vars(d, rhs))
  fit <- fit_ols(d, y, rhs, subset = d$round == 3, cluster = "survey_town")
  z <- model_rows(
    fit, intersect(c("type", local_share_terms), rhs),
    paste0("A10_consumer_", y), y
  )
  z$specification <- "consumer local-exposure robustness"
  a10_rows[[length(a10_rows) + 1L]] <- z
}

for (y in t8_outcomes) {
  rhs_candidates <- c("interpost", "localinter1", "localinter2", "localinter10",
                      "post", "pldummy1", "pldummy2", "pldummy3", "pldummy4",
                      "localinter5", "localinter6", "localinter13")
  # The supplied Stata code omits localinter5 only in the total-profit A10 regression.
  if (y == "total_profit") rhs_candidates <- setdiff(rhs_candidates, "localinter5")
  rhs <- existing_vars(d, rhs_candidates)
  fit <- fit_ols(d, y, rhs, cluster = "survey_town", fe = "firmid")
  z <- model_rows(
    fit, intersect(c("interpost", "localinter1", "localinter2", "localinter10",
                     "localinter5", "localinter6"), rhs),
    paste0("A10_firm_", y), y, fixed_effects = "Firm FE + Post"
  )
  z$specification <- "firm local-exposure robustness"
  a10_rows[[length(a10_rows) + 1L]] <- z
}
write_table(dplyr::bind_rows(a10_rows), "TableA10", "Table A10. Local robustness")

# ------------------------------------------------------------------
# Table 9: first stages and fixed-effect IV
# ------------------------------------------------------------------

require_cols(d, c("loanuse", "comploanuse", "midline", "endline", "interpost", "inter4post"), "loanmain.dta")

fs_rows <- list()
for (y in c("loanuse", "comploanuse")) {
  fit <- fit_ols(
    d, y, c("interpost", "inter4post", "midline", "endline"),
    subset = !is.na(d$lnlabor), cluster = "survey_town", fe = "firmid"
  )
  z <- model_rows(
    fit, c("interpost", "inter4post"), paste0("Table9_firststage_", y), y,
    fixed_effects = "Firm FE + midline/endline"
  )
  z$first_stage_F <- joint_wald_stat(fit, c("interpost", "inter4post"))
  z$stage <- "First stage"
  fs_rows[[length(fs_rows) + 1L]] <- z
}

iv_rows <- list()
for (y in t8_outcomes) {
  fit <- fit_iv(
    d, y,
    exogenous = c("midline", "endline"),
    endogenous = c("loanuse", "comploanuse"),
    instruments = c("interpost", "inter4post"),
    fe = "firmid",
    cluster = NULL
  )
  z <- model_rows(
    fit, c("loanuse", "comploanuse"), paste0("Table9_IV_", y), y,
    fixed_effects = "Firm FE + midline/endline",
    notes = "Stata xtivreg specification uses default (non-clustered) IV standard errors."
  )
  z$first_stage_F <- NA_real_
  z$stage <- "IV"
  iv_rows[[length(iv_rows) + 1L]] <- z
}

write_table(
  dplyr::bind_rows(dplyr::bind_rows(fs_rows), dplyr::bind_rows(iv_rows)),
  "Table9", "Table 9. First stages and IV"
)
