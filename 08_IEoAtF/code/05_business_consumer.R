# 05_business_consumer.R
# Translation of loan_main.do: Table 4, Table 5, Table A8, and Table A9.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("prepare_loanmain")) source("code/02_helpers.R")

d <- prepare_loanmain()
controls_end <- endline_controls(d)

# ------------------------------------------------------------------
# Table 4: other business outcomes
# ------------------------------------------------------------------

t4_specs <- list()

for (y in existing_vars(d, c("lnnum_clients", "rennovation", "newproduct"))) {
  t4_specs[[length(t4_specs) + 1L]] <- list(
    id = paste0("Table4_", y),
    outcome = y,
    rhs = c("post", "interpost", "inter4post"),
    test_terms = c("interpost", "inter4post"),
    subset_fun = function(x) rep(TRUE, nrow(x)),
    cluster = "survey_town",
    fe = "firmid",
    panel = "firmid"
  )
}

for (y in existing_vars(d, c("shareeducl", "newsupplier", "stock_frequency", "stockmanage"))) {
  rhs <- c("type", "treatratio_comp", controls_end)
  t4_specs[[length(t4_specs) + 1L]] <- list(
    id = paste0("Table4_", y),
    outcome = y,
    rhs = rhs,
    test_terms = c("type", "treatratio_comp"),
    subset_fun = function(x) x$round == 3,
    cluster = "survey_town",
    fe = NULL,
    panel = "firmid"
  )
}

t4_models <- lapply(t4_specs, function(s) fit_spec(d, s))
t4 <- dplyr::bind_rows(lapply(seq_along(t4_specs), function(i) {
  s <- t4_specs[[i]]
  y <- s$outcome
  if (y == "lnnum_clients") {
    cm <- control_mean(d, y, d$survey_town_type == 0 & d$round == 1)
    fe <- "Firm FE + Post"
  } else if (y %in% c("rennovation", "newproduct")) {
    cm <- control_mean(d, y, d$survey_town_type == 0 & d$round == 3)
    fe <- "Firm FE + Post"
  } else {
    cm <- control_mean(d, y, d$survey_town_type == 0 & d$round == 3)
    fe <- "No"
  }
  model_rows(t4_models[[i]], s$test_terms, s$id, y, cm, fe)
}))

rw4 <- romano_wolf_cluster(d, t4_specs, reps = getOption("ra2026.rwolf_reps", 1000L),
                           seed = 123L, cluster = "survey_town", panel = "firmid")
t4 <- dplyr::left_join(
  t4, rw4[, c("model", "outcome", "term", "rw_p.value")],
  by = c("model", "outcome", "term")
)
write_table(t4, "Table4", "Table 4. Other business outcomes")

# ------------------------------------------------------------------
# Table 5: consumer outcomes
# ------------------------------------------------------------------

consumer_outcomes <- c(
  "lnprice", "selleradvice",
  "zconeval_squality", "zconeval_env", "zconeval_money", "zconeval_all"
)
consumer_outcomes <- existing_vars(d, consumer_outcomes)

t5_specs <- lapply(consumer_outcomes, function(y) {
  list(
    id = paste0("Table5_", y),
    outcome = y,
    rhs = c("type", "treatratio_comp", controls_end),
    test_terms = c("type", "treatratio_comp"),
    subset_fun = function(x) x$round == 3,
    cluster = "survey_town",
    fe = NULL,
    panel = "firmid"
  )
})

t5_models <- lapply(t5_specs, function(s) fit_spec(d, s))
t5 <- dplyr::bind_rows(lapply(seq_along(t5_specs), function(i) {
  s <- t5_specs[[i]]
  if (s$outcome %in% c("lnprice", "selleradvice")) {
    cm <- control_mean(d, s$outcome, d$survey_town_type == 0)
  } else {
    cm <- control_mean(d, s$outcome, d$survey_town_type == 0 & d$round == 3)
  }
  model_rows(t5_models[[i]], s$test_terms, s$id, s$outcome, cm, "No")
}))

rw5 <- romano_wolf_cluster(d, t5_specs, reps = getOption("ra2026.rwolf_reps", 1000L),
                           seed = 123L, cluster = "survey_town", panel = "firmid")
t5 <- dplyr::left_join(
  t5, rw5[, c("model", "outcome", "term", "rw_p.value")],
  by = c("model", "outcome", "term")
)
write_table(t5, "Table5", "Table 5. Consumer outcomes")

# ------------------------------------------------------------------
# Table A8: additional business outcomes
# ------------------------------------------------------------------

d_a8 <- d
if (all(c("material_cost", "wage_cost") %in% names(d_a8))) {
  d_a8$costtemp <- d_a8$material_cost + d_a8$wage_cost
  d_a8$markup <- d_a8$part5revenue / d_a8$costtemp
  d_a8$lnmarkup <- log(d_a8$markup)
}

a8_outcomes <- existing_vars(
  d_a8, c("tradecredit_supplier", "tradecredit_client", "lnmarkup",
          "lnrent", "lnnum_supplier", "ad_cost")
)
a8 <- dplyr::bind_rows(lapply(a8_outcomes, function(y) {
  rhs <- existing_vars(d_a8, c("interpost", "inter4post", "post"))
  m <- fit_ols(d_a8, y, rhs, cluster = "survey_town", fe = "firmid")
  model_rows(
    m, intersect(c("interpost", "inter4post"), rhs),
    paste0("A8_", y), y,
    control = control_mean(d_a8, y, d_a8$round == 1 & d_a8$survey_town_type == 0),
    fixed_effects = "Firm FE + Post"
  )
}))
write_table(a8, "TableA8", "Table A8. Additional business outcomes")

# ------------------------------------------------------------------
# Table A9: alternative sales and financial-knowledge outcomes
# ------------------------------------------------------------------

a9_rows <- list()
if ("lnsales2" %in% names(d)) {
  m1 <- fit_ols(
    d, "lnsales2", existing_vars(d, c("type", "treatratio_comp")),
    subset = d$round == 3, cluster = "survey_town"
  )
  z1 <- model_rows(m1, existing_vars(d, c("type", "treatratio_comp")),
                   "A9_lnsales2_endline", "lnsales2")
  z1$specification <- "endline cross-section"
  a9_rows[[length(a9_rows) + 1L]] <- z1

  d_fe <- d
  d_fe$lnsales2[d_fe$round == 1] <- d_fe$lnpart5revenue[d_fe$round == 1]
  m2 <- fit_ols(
    d_fe, "lnsales2", existing_vars(d_fe, c("interpost", "inter4post", "post")),
    cluster = "survey_town", fe = "firmid"
  )
  z2 <- model_rows(m2, existing_vars(d_fe, c("interpost", "inter4post")),
                   "A9_lnsales2_panel", "lnsales2", fixed_effects = "Firm FE + Post")
  z2$specification <- "panel with baseline main-sales measure"
  a9_rows[[length(a9_rows) + 1L]] <- z2
}

if ("lnsalesdif" %in% names(d)) {
  rhs <- existing_vars(d, c("type", "treatratio_comp"))
  m <- fit_ols(d, "lnsalesdif", rhs, subset = d$round == 3, cluster = "survey_town")
  z <- model_rows(m, rhs, "A9_lnsalesdif", "lnsalesdif")
  z$specification <- "alternative sales difference"
  a9_rows[[length(a9_rows) + 1L]] <- z
}

for (y in existing_vars(d, c("zfinknowledge", "zdifficultyloan"))) {
  rhs <- existing_vars(d, c("type", "treatratio_comp"))
  m <- fit_ols(d, y, rhs, subset = d$round == 3, cluster = "survey_town")
  z <- model_rows(m, rhs, paste0("A9_", y), y)
  z$specification <- "standardized using untreated mean absolute deviation"
  a9_rows[[length(a9_rows) + 1L]] <- z
}

write_table(dplyr::bind_rows(a9_rows), "TableA9", "Table A9")
