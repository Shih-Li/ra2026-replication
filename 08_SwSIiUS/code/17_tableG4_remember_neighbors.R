# 17_tableG4_remember_neighbors.R
# Stata Table G-4 (original analysis lines ~1742-1795): remembering neighbors' prices/decisions.

d <- read_source("dyadic")
base_interest <- c("neighbor_public", "who_2hh", "who_1nghbr", "who_2hh1nghbr")
other_candidates <- c("treated", "subsidy_high", "deposit", "split_number_s5")
candidates <- unique(c(other_candidates, controls_all, miss_vars(d)))

specs <- list(
  bl_price = list(y = "returnbl_ct_subsidy_nghbr", extra = c("returnbl_deci_same", "weeks_bldeci_blreturn")),
  bl_offer = list(y = "returnbl_ct_offer_nghbr", extra = c("returnbl_deci_same", "weeks_bldeci_blreturn")),
  el_price = list(y = "returnel_ct_subsidy_nghbr", extra = c("returnel_el_same", "weeks_bldeci_elreturn")),
  el_offer = list(y = "returnel_ct_offer_nghbr", extra = c("returnel_el_same", "weeks_bldeci_elreturn")),
  el_deslu = list(y = "returnel_ct_desludge_nghbr", extra = c("returnel_el_same", "weeks_bldeci_elreturn"))
)
models <- list(); means <- numeric()
for (nm in names(specs)) {
  s <- specs[[nm]]
  interest <- c(base_interest, s$extra)
  fit <- dsregress_r(d, s$y, interest, candidates = candidates)
  models[[nm]] <- fit$model
  means[[nm]] <- mean(fit$data[[s$y]][fit$data$neighbor_public == 0], na.rm = TRUE)
}
all_terms <- unique(c(base_interest, unlist(lapply(specs, `[[`, "extra"))))
write_models_tex(
  models, result_path("TableG4_RememberNeighbors.tex", "table"), terms = all_terms, labels = all_terms,
  titles = c("BL Subsidy", "BL Signed Up", "EL Subsidy", "EL Signed Up", "EL Used Subs Desl"), means = means
)
