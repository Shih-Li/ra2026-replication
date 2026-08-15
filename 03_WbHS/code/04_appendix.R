# ============================================================
# 04_appendix.R
# Reproduce Appendix Tables 1-5
# ============================================================

if (!exists("ROOT", inherits = TRUE)) source("code/01_setup.R")


appendix_covariates <- c(
  "edad", "lb_genero", "casado_o_convive", "lb_jefe_hogar", "lb_residentes_totales",
  "lb_residentes_menores", "lb_residentes_ninos", "lb_residentes_adulto", "ad_mayor",
  "ed_8menos", "ed_9_11", "ed_12", "ed_mas_12", "lb_years_education", "lb_employ",
  "lb_cont_formal", "fonasa_1", "fonasa_2", "fonasa_3", "fonasa_4", "lb_ohip14",
  "lb_rosenberg_score", "lb_dientes_falt_tot", "lb_dientes_falt_front",
  "lb_prosthesis_up", "lb_prosthesis_down", "lb_ingreso_th"
)

run_appendix <- function(root) {
  out_dir <- ensure_dir(file.path(root, "output", "tables", "appendix"))
  afile <- file.path(root, "data", "processed", "analysis_dataset.rds")
  if (file.exists(afile)) {
    d <- readRDS(afile)
  } else {
    mfile <- file.path(root, "data", "processed", "main_dataset.rds")
    require_files(mfile)
    d <- prepare_analysis_vars(readRDS(mfile))
  }
  require_cols(d, appendix_covariates)

  # Appendix Table 1: determinants of participation in follow-up surveys.
  a1 <- list(); k <- 1L
  for (y in c("FirstFU", "SecondFU")) {
    fit1 <- robust_lm(d, y, c("treatment", strata_vars(d)))
    st1 <- coef_stats(fit1, "treatment")
    a1[[k]] <- data.frame(followup=y, specification="Treatment only", variable="treatment",
                          estimate=st1[["estimate"]], se=st1[["se"]], p=st1[["p"]], joint_p=NA_real_); k <- k+1L
    fit2 <- robust_lm(d, y, c("treatment", appendix_covariates, strata_vars(d)))
    for (v in c("treatment", appendix_covariates)) {
      st <- coef_stats(fit2, v)
      a1[[k]] <- data.frame(followup=y, specification="Treatment + baseline covariates", variable=v,
                            estimate=st[["estimate"]], se=st[["se"]], p=st[["p"]], joint_p=NA_real_); k <- k+1L
    }
    jp <- wald_p(fit2, appendix_covariates)
    a1[[k-1L]]$joint_p <- jp
  }
  a1 <- dplyr::bind_rows(a1)
  write_table_outputs(a1, "WBHS_Appendix_Table1", "Determinants of Participation in Follow-Up Surveys", out_dir)

  # Appendix Table 2: determinants of treatment take-up among treated subjects.
  a2vars <- c(
    "edad", "lb_genero", "casado_o_convive", "lb_jefe_hogar", "lb_residentes_menores",
    "lb_residentes_ninos", "lb_residentes_adulto", "ad_mayor", "ed_8menos", "ed_9_11",
    "ed_12", "ed_mas_12", "lb_years_education", "lb_employ", "lb_cont_formal",
    "fonasa_2", "fonasa_3", "fonasa_4", "sd_ohip0", "sd_rosen0", "lb_dientes_falt_tot",
    "lb_dientes_falt_front", "lb_prosthesis_up", "lb_prosthesis_down", "lb_ingreso_th"
  )
  a2 <- list(); k <- 1L
  for (g in c("All","Men","Women")) {
    mask <- subgroup_mask(d,g) & d$treatment == 1
    fit <- robust_lm(d, "completed", c(a2vars, strata_vars(d)), subset = mask)
    jp <- wald_p(fit, a2vars)
    for (v in a2vars) {
      st <- coef_stats(fit, v)
      a2[[k]] <- data.frame(group=g, variable=v, estimate=st[["estimate"]], se=st[["se"]],
                            p=st[["p"]], stars=stars(st[["p"]]), joint_p=jp, n=stats::nobs(fit$model)); k <- k+1L
    }
  }
  a2 <- dplyr::bind_rows(a2)
  write_table_outputs(a2, "WBHS_Appendix_Table2", "Determinants of Treatment Take-up", out_dir)

  # Appendix Table 3: inverse probability weighting estimates.
  ipw_outcomes <- c(
    "new_sd_ohip1", "new_sd_ohip2", "sd_index_photos", "new_sd_rosen1", "smiles",
    "employed", "ltotal_1a", "interact", "efforts", "new_sd_rosen2", "sf12_ph_sd",
    "sf12_mh_sd", "SEG2_employed", "ltotal_2a"
  )
  ipw_reps <- as.integer(getOption("wbhs.ipw_boot_reps", 200L))
  a3 <- list(); k <- 1L
  for (y in ipw_outcomes) for (g in c("All","Men","Women")) {
    message("IPW: ", y, ", ", g, " (", ipw_reps, " bootstrap reps)")
    a3[[k]] <- ipw_ate(d, y, group=g, reps=ipw_reps, seed=456L); k <- k+1L
  }
  a3 <- dplyr::bind_rows(a3)
  write_table_outputs(a3, "WBHS_Appendix_Table3", "Inverse Probability Weighting Estimates", out_dir)

  # Appendix Table 4: IV effects of subjective oral health on outcomes.
  d$Lsd_ohip1 <- d$new_sd_ohip0; d$Lsd_rosen1 <- d$new_sd_rosen0; d$LEmployed <- d$lb_employ; d$Lltotal_1a <- d$ltotal_bl
  d$Lsd_ohip2 <- d$new_sd_ohip0; d$Lsd_rosen2 <- d$new_sd_rosen0; d$LSEG2_employed <- d$lb_employ; d$Lltotal_2a <- d$ltotal_bl
  d$treat_genero <- d$treatment * d$lb_genero
  d$ohip1_genero <- d$new_sd_ohip1 * d$lb_genero
  d$ohip2_genero <- d$new_sd_ohip2 * d$lb_genero

  iv_specs <- data.frame(
    followup = c("FU1","FU1","FU1","FU2","FU2","FU2"),
    outcome = c("sd_rosen1","ltotal_1a","employed","sd_rosen2","ltotal_2a","SEG2_employed"),
    endogenous = c("sd_ohip1","sd_ohip1","sd_ohip1","sd_ohip2","sd_ohip2","sd_ohip2"),
    baseline = c("Lsd_rosen1","Lltotal_1a","LEmployed","Lsd_rosen2","Lltotal_2a","LEmployed"),
    stringsAsFactors = FALSE
  )
  a4 <- list(); k <- 1L
  for (i in seq_len(nrow(iv_specs))) {
    y <- iv_specs$outcome[i]; endo <- iv_specs$endogenous[i]; bl <- iv_specs$baseline[i]
    for (g in c("All","Men","Women")) {
      mask <- subgroup_mask(d,g)
      fit <- robust_iv(d, y, endo, "treatment", c(bl, controls_unb(), "factor(n_estrato)"), subset=mask)
      st <- coef_stats(fit, endo)
      used <- as.integer(rownames(stats::model.frame(fit$model)))
      ud <- d[used,,drop=FALSE]
      cm <- safe_mean(ud[[y]][ud$treatment==0])
      a4[[k]] <- data.frame(followup=iv_specs$followup[i], outcome=y, group=g, control_mean=cm,
                            estimate=st[["estimate"]], se=st[["se"]], p=st[["p"]], gender_diff_p=NA_real_,
                            across_survey_p=NA_real_, n=stats::nobs(fit$model)); k <- k+1L
    }
    # Gender-effect IV: two endogenous regressors instrumented by treatment and treatment x gender.
    og <- if (iv_specs$followup[i] == "FU1") "ohip1_genero" else "ohip2_genero"
    exog <- c(bl, "lb_genero", paste0(bl, ":lb_genero"), controls_unb_gender(), strata_vars(d), paste0(strata_vars(d), ":lb_genero"))
    fitg <- robust_iv(d, y, c(endo, og), c("treatment","treat_genero"), exog)
    pg <- coef_stats(fitg, og)[["p"]]
    for (j in (k-3L):(k-1L)) a4[[j]]$gender_diff_p <- pg
  }
  a4 <- dplyr::bind_rows(a4)

  iv_panel_diff <- function(y0,y1,y2,group) {
    mask <- subgroup_mask(d,group)
    z <- d[mask,,drop=FALSE]; z$.id <- seq_len(nrow(z))
    long <- dplyr::bind_rows(
      dplyr::transmute(z,.id,treatment,time=0L,y=.data[[y0]],ohip=sd_ohip0),
      dplyr::transmute(z,.id,treatment,time=1L,y=.data[[y1]],ohip=sd_ohip1),
      dplyr::transmute(z,.id,treatment,time=2L,y=.data[[y2]],ohip=sd_ohip2)
    )
    long$d1 <- as.numeric(long$time==1L); long$d2 <- as.numeric(long$time==2L)
    long$t1 <- long$treatment*long$d1; long$t2 <- long$treatment*long$d2
    long$o1 <- long$ohip*long$d1; long$o2 <- long$ohip*long$d2
    fit <- try(robust_iv(long, "y", c("o1","o2"), c("t1","t2"), c("d1","d2","factor(.id)"), cluster=".id"), silent=TRUE)
    if (inherits(fit,"try-error")) return(NA_real_)
    ans <- try(car::linearHypothesis(fit$model, "o1 = o2", vcov.=fit$vcov, test="F"), silent=TRUE)
    if (inherits(ans,"try-error")) return(NA_real_)
    pcol <- grep("Pr\\(>F\\)",names(ans),value=TRUE)
    as.numeric(ans[nrow(ans),pcol[1]])
  }
  for (g in c("All","Men","Women")) {
    p_ros <- iv_panel_diff("sd_rosen0","sd_rosen1","sd_rosen2",g)
    p_emp <- iv_panel_diff("lb_employ","employed","SEG2_employed",g)
    p_inc <- iv_panel_diff("ltotal_bl","ltotal","ltotal_2",g)
    a4$across_survey_p[a4$followup=="FU2" & a4$outcome=="sd_rosen2" & a4$group==g] <- p_ros
    a4$across_survey_p[a4$followup=="FU2" & a4$outcome=="SEG2_employed" & a4$group==g] <- p_emp
    a4$across_survey_p[a4$followup=="FU2" & a4$outcome=="ltotal_2a" & a4$group==g] <- p_inc
  }
  write_table_outputs(a4, "WBHS_Appendix_Table4", "Instrumental Variables Regressions: Effects of Health Status on Outcomes", out_dir)

  # Appendix Table 5: components of additional outcomes.
  component_sets <- list(
    "Complementary Investments" = paste0("SEG2_e1_",1:9),
    "Quality of Interactions" = c(paste0("Qd_act_",1:3), paste0("Qnd_act_",4:7)),
    "Appearance" = c("sd_cs4_mean","sd_hi4_mean","sd_ap4_mean","sd_sr4_mean")
  )
  a5 <- list(); k <- 1L
  for (panel in names(component_sets)) for (y in component_sets[[panel]]) for (g in c("All","Men","Women")) {
    r <- outcome_result(d, y, group=g, controls=controls_all())
    r$panel <- panel
    a5[[k]] <- r; k <- k+1L
  }
  a5 <- dplyr::bind_rows(a5)
  write_table_outputs(a5, "WBHS_Appendix_Table5", "Treatment Effects on Components of Additional Outcomes", out_dir)

  message("Appendix Tables 1-5 written to output/tables/appendix.")
  invisible(d)
}


run_appendix(ROOT)
