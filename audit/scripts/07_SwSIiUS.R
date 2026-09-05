# ==============================================================================
# audit/scripts/07_SwSIiUS.R
#
# Paper:
#   Deutschmann, Lipscomb, Schechter, and Zhu
#   "Spillovers without Social Interactions in Urban Sanitation"
#
# MIS estimand:
#   Table 2 — Decision Spillovers
#   Treated-household sample
#
# Outcome:
#   signup_all
#
# Target:
#   neighbor_high
#
# Other causal regressor retained:
#   subsidy_high
#
# Estimator:
#   Post-double-selection LASSO determines the baseline nuisance controls,
#   followed by FE-OLS:
#
#     signup_all ~
#       neighbor_high +
#       subsidy_high +
#       selected baseline controls
#       | Arrondissement_categorical
#
#   Cluster:
#     s0d_dclusterid
#
# IMPORTANT MODEL-SELECTION NOTE
# ------------------------------------------------------------------------------
# The authors' dsregress specification uses post-double-selection LASSO.
#
# For the empirical MIS audit, we:
#
#   1. reproduce the baseline PDS model;
#   2. freeze the controls selected by that baseline model;
#   3. construct an explicit-dummy OLS representation of the resulting
#      preferred FE-OLS specification;
#   4. require exact baseline coefficient equivalence;
#   5. keep all baseline nuisance regressors fixed throughout the deletion path;
#   6. exact-refit the frozen FE-OLS paper specification after each deletion.
#
# We do NOT re-run LASSO after every deletion because that would change the
# nuisance specification as k changes rather than audit observation sensitivity
# of the preferred baseline coefficient.
#
# Retained Table 2 benchmark:
#   N = 3757
#   beta(neighbor_high), rounded to 3 decimals = -0.010
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both directions
#   exact FE-OLS refit after each selected deletion set
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. General options
# ------------------------------------------------------------------------------

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ------------------------------------------------------------------------------
# 1. Locate audit project
# ------------------------------------------------------------------------------

find_audit_root <- function() {
  
  wd <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
  
  if (
    basename(wd) == "audit" &&
    dir.exists(file.path(wd, "function"))
  ) {
    return(wd)
  }
  
  
  script_file <- tryCatch(
    sys.frame(1)$ofile,
    error = function(e) NULL
  )
  
  
  if (!is.null(script_file)) {
    
    script_file <- normalizePath(
      script_file,
      winslash = "/",
      mustWork = TRUE
    )
    
    candidate <- dirname(
      dirname(script_file)
    )
    
    if (
      basename(candidate) == "audit" &&
      dir.exists(file.path(candidate, "function"))
    ) {
      return(candidate)
    }
  }
  
  
  stop(
    paste0(
      "Could not locate audit project.\n",
      "Open audit/audit.Rproj and run:\n",
      'source("scripts/07_SwSIiUS.R")'
    ),
    call. = FALSE
  )
}


AUDIT_ROOT <- find_audit_root()


REPO_ROOT <- dirname(
  AUDIT_ROOT
)


PAPER_ROOT <- file.path(
  REPO_ROOT,
  "07_SwSIiUS"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "FinalData_CompleteBLDecider_LASSO.dta"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "07_SwSIiUS"
)


if (!dir.exists(PAPER_ROOT)) {
  
  stop(
    "Paper folder does not exist: ",
    PAPER_ROOT,
    call. = FALSE
  )
}


if (!file.exists(DATA_FILE)) {
  
  stop(
    "Required Paper 7 analysis dataset does not exist:\n",
    DATA_FILE,
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "haven",
  "fixest",
  "glmnet",
  "ggplot2"
)


missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "Install required package(s): ",
      paste(
        missing_packages,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 3. Load shared MIS audit functions
# ------------------------------------------------------------------------------

shared_files <- c(
  "dinkelbach_topk.R",
  "audit_validate.R",
  "audit_engine.R",
  "audit_output.R",
  "audit_plot.R"
)


for (f in shared_files) {
  
  path <- file.path(
    AUDIT_ROOT,
    "function",
    f
  )
  
  if (!file.exists(path)) {
    
    stop(
      "Missing shared audit file: ",
      path,
      call. = FALSE
    )
  }
  
  source(path)
}


# ------------------------------------------------------------------------------
# 4. Load Paper 7 replication helpers in an isolated environment
# ------------------------------------------------------------------------------

# Use a separate environment so Paper 7 helper objects such as `paths` do not
# overwrite objects in the audit project.
paper_env <- new.env(
  parent = globalenv()
)


paper_env$paths <- list(
  
  root = PAPER_ROOT,
  
  code = file.path(
    PAPER_ROOT,
    "code"
  ),
  
  source_cleaned = file.path(
    PAPER_ROOT,
    "data",
    "source",
    "cleaned"
  ),
  
  intermediate = file.path(
    PAPER_ROOT,
    "data",
    "intermediate"
  ),
  
  results = file.path(
    PAPER_ROOT,
    "results"
  ),
  
  tables = file.path(
    PAPER_ROOT,
    "results",
    "tables"
  ),
  
  figures = file.path(
    PAPER_ROOT,
    "results",
    "figures"
  )
)


paper_env$paths$data <- list(
  
  complete_bl_decider_lasso = DATA_FILE
)


paper_env$paths$state <- file.path(
  PAPER_ROOT,
  "data",
  "intermediate",
  "analysis_state.rds"
)


paper_env$paths$result_prefix <- "R-"


source(
  file.path(
    PAPER_ROOT,
    "code",
    "02_variable_lists.R"
  ),
  local = paper_env
)


source(
  file.path(
    PAPER_ROOT,
    "code",
    "03_helpers.R"
  ),
  local = paper_env
)


# ------------------------------------------------------------------------------
# 5. Paper-specific constants
# ------------------------------------------------------------------------------

STUDY_ID <- "07_SwSIiUS"


ESTIMAND_ID <-
  "table2_treated_signup_neighbor_high"


OUTCOME <- "signup_all"


TARGET <- "neighbor_high"


INTEREST <- c(
  "neighbor_high",
  "subsidy_high"
)


FE_VAR <- "Arrondissement_categorical"


CLUSTER_VAR <- "s0d_dclusterid"


EXPECTED_N <- 3757L


# Repository Table 2 retains only three decimal places.
EXPECTED_BETA_REPORTED <- -0.010


EXPECTED_BETA_DIGITS <- 3L


BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 6. Load analysis-ready Paper 7 dataset
# ------------------------------------------------------------------------------

d <- paper_env$read_source(
  "complete_bl_decider_lasso"
)


d <- as.data.frame(
  d
)


# Permanent source-row identifier, created before any restriction.
if ("source_row_id" %in% names(d)) {
  
  stop(
    "`source_row_id` already exists in Paper 7 source data.",
    call. = FALSE
  )
}


d$source_row_id <- seq_len(
  nrow(d)
)


# ------------------------------------------------------------------------------
# 7. Check key variables
# ------------------------------------------------------------------------------

required_vars <- c(
  OUTCOME,
  INTEREST,
  "treated",
  FE_VAR,
  CLUSTER_VAR,
  "source_row_id"
)


missing_vars <- setdiff(
  required_vars,
  names(d)
)


if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required Paper 7 variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8. Reconstruct Table 2 candidate-control set
# ------------------------------------------------------------------------------

# This is exactly the candidate structure used by
# code/07_table2_decision_spillovers.R.
treatment_candidates <- c(
  "deposit",
  "neighbor_public",
  "split_who_s5",
  "split_number_s5"
)


base_candidates <- unique(
  c(
    paper_env$controls_all,
    treatment_candidates,
    paper_env$miss_vars(d)
  )
)


# ------------------------------------------------------------------------------
# 9. Reproduce baseline post-double-selection specification
# ------------------------------------------------------------------------------

baseline_pds <- paper_env$dsregress_r(
  data = d,
  
  y = OUTCOME,
  
  interest = INTEREST,
  
  always = character(),
  
  candidates = base_candidates,
  
  fe = FE_VAR,
  
  cluster = CLUSTER_VAR,
  
  subset = d$treated == 1
)


pds_fit <- baseline_pds$model


analysis_sample <- as.data.frame(
  baseline_pds$data
)


row.names(analysis_sample) <- NULL


SELECTED_CONTROLS <- baseline_pds$selected


message(
  "Paper 7 baseline PDS selected ",
  length(SELECTED_CONTROLS),
  " nuisance control(s)."
)


if (length(SELECTED_CONTROLS) > 0L) {
  
  message(
    "Selected controls: ",
    paste(
      SELECTED_CONTROLS,
      collapse = ", "
    )
  )
}


# ------------------------------------------------------------------------------
# 10. Baseline PDS benchmark
# ------------------------------------------------------------------------------

N_pds <- as.integer(
  stats::nobs(pds_fit)
)


pds_coef <- stats::coef(
  pds_fit
)


if (!TARGET %in% names(pds_coef)) {
  
  stop(
    paste0(
      "Target coefficient `",
      TARGET,
      "` is absent from baseline PDS model."
    ),
    call. = FALSE
  )
}


beta_pds <- unname(
  pds_coef[[TARGET]]
)


if (N_pds != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    07_SwSIiUS.R\n\n",
      
      "Problem:\n",
      "    Baseline Table 2 PDS model does not reproduce expected N.\n\n",
      
      "Expected N:\n",
      "    ", EXPECTED_N, "\n\n",
      
      "Obtained N:\n",
      "    ", N_pds
    ),
    call. = FALSE
  )
}


beta_pds_reported <- round(
  beta_pds,
  digits = EXPECTED_BETA_DIGITS
)


if (
  abs(
    beta_pds_reported -
    EXPECTED_BETA_REPORTED
  ) > 1e-12
) {
  
  stop(
    paste0(
      "\n",
      "Baseline Paper 7 PDS coefficient does not reproduce ",
      "the retained Table 2 value.\n\n",
      
      "Expected, rounded to 3 decimals:\n",
      "    ",
      format(
        EXPECTED_BETA_REPORTED,
        nsmall = EXPECTED_BETA_DIGITS
      ),
      "\n\n",
      
      "Obtained full precision:\n",
      "    ",
      format(
        beta_pds,
        digits = 16
      ),
      "\n\n",
      
      "Obtained rounded value:\n",
      "    ",
      format(
        beta_pds_reported,
        nsmall = EXPECTED_BETA_DIGITS
      )
    ),
    call. = FALSE
  )
}


message(
  "Baseline Paper 7 PDS replication check PASSED."
)


message(
  "N_pds = ",
  N_pds
)


message(
  "beta_pds = ",
  format(
    beta_pds,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 11. Freeze the selected preferred nuisance specification
# ------------------------------------------------------------------------------

# dsregress final estimation is:
#
#   interest + selected controls | arrondissement FE
#
# If fixest removed an exactly collinear nuisance variable, omit that same
# nuisance column from the frozen explicit specification.
collin_vars <- pds_fit$collin.var


if (is.null(collin_vars)) {
  
  collin_vars <- character()
}


FINAL_RHS <- unique(
  c(
    INTEREST,
    SELECTED_CONTROLS
  )
)


FINAL_RHS <- setdiff(
  FINAL_RHS,
  collin_vars
)


if (!TARGET %in% FINAL_RHS) {
  
  stop(
    "Target was removed as collinear in baseline PDS model.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 12. Construct exact frozen FE-OLS formula
# ------------------------------------------------------------------------------

bt_local <- function(x) {
  
  paste0(
    "`",
    gsub(
      "`",
      "",
      x
    ),
    "`"
  )
}


rhs_fixest <- paste(
  bt_local(FINAL_RHS),
  collapse = " + "
)


original_formula <- stats::as.formula(
  paste0(
    bt_local(OUTCOME),
    " ~ ",
    rhs_fixest,
    " | ",
    bt_local(FE_VAR)
  )
)


cluster_formula <- stats::as.formula(
  paste0(
    "~",
    bt_local(CLUSTER_VAR)
  )
)


# ------------------------------------------------------------------------------
# 13. ORIGINAL ESTIMATOR — frozen preferred FE-OLS specification
# ------------------------------------------------------------------------------

original_fit <- fixest::feols(
  fml = original_formula,
  
  data = analysis_sample,
  
  cluster = cluster_formula,
  
  ssc = paper_env$stata_cluster_ssc(
    FALSE
  ),
  
  warn = FALSE,
  
  notes = FALSE
)


N_original <- as.integer(
  stats::nobs(original_fit)
)


original_coef <- stats::coef(
  original_fit
)


if (!TARGET %in% names(original_coef)) {
  
  stop(
    paste0(
      "Target coefficient `",
      TARGET,
      "` is absent from frozen original estimator."
    ),
    call. = FALSE
  )
}


beta_original <- unname(
  original_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 14. Validate frozen model against baseline PDS model
# ------------------------------------------------------------------------------

if (N_original != N_pds) {
  
  stop(
    paste0(
      "Frozen FE-OLS model changes N.\n",
      "PDS N = ",
      N_pds,
      "\n",
      "Frozen N = ",
      N_original
    ),
    call. = FALSE
  )
}


if (
  !is.finite(beta_original) ||
  abs(
    beta_original -
    beta_pds
  ) > BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "\n",
      "Frozen selected-control FE-OLS does not reproduce ",
      "the baseline PDS coefficient.\n\n",
      
      "PDS beta:\n",
      "    ",
      format(
        beta_pds,
        digits = 16
      ),
      "\n\n",
      
      "Frozen FE-OLS beta:\n",
      "    ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "Absolute difference:\n",
      "    ",
      format(
        abs(
          beta_original -
            beta_pds
        ),
        scientific = TRUE
      )
    ),
    call. = FALSE
  )
}


message(
  "Frozen Paper 7 FE-OLS validation PASSED."
)


message(
  "beta_original = ",
  format(
    beta_original,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 15. MIS-compatible explicit fixed-effect representation
# ------------------------------------------------------------------------------

rhs_mis <- c(
  bt_local(FINAL_RHS),
  paste0(
    "factor(",
    bt_local(FE_VAR),
    ")"
  )
)


mis_formula <- stats::as.formula(
  paste(
    bt_local(OUTCOME),
    "~",
    paste(
      rhs_mis,
      collapse = " + "
    )
  )
)


mis_fit <- stats::lm(
  formula = mis_formula,
  
  data = analysis_sample,
  
  na.action = stats::na.fail
)


N_mis <- as.integer(
  stats::nobs(mis_fit)
)


mis_coef <- stats::coef(
  mis_fit
)


if (!TARGET %in% names(mis_coef)) {
  
  stop(
    paste0(
      "Target coefficient `",
      TARGET,
      "` is absent from MIS-compatible lm."
    ),
    call. = FALSE
  )
}


beta_mis <- unname(
  mis_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 16. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  
  stop(
    paste0(
      "MIS explicit-FE model changes sample N.\n",
      "Original N = ",
      N_original,
      "\n",
      "MIS N = ",
      N_mis
    ),
    call. = FALSE
  )
}


if (
  !is.finite(beta_mis) ||
  abs(
    beta_mis -
    beta_original
  ) > BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "\n",
      "MIS explicit-FE lm does not reproduce original FE-OLS slope.\n\n",
      
      "Original beta:\n",
      "    ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "MIS beta:\n",
      "    ",
      format(
        beta_mis,
        digits = 16
      ),
      "\n\n",
      
      "Absolute difference:\n",
      "    ",
      format(
        abs(
          beta_mis -
            beta_original
        ),
        scientific = TRUE
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 17. Required full-rank check
# ------------------------------------------------------------------------------

X_mis <- stats::model.matrix(
  mis_fit
)


rank_mis <- qr(
  X_mis
)$rank


if (rank_mis < ncol(X_mis)) {
  
  stop(
    paste0(
      "\n",
      "MIS model matrix is rank deficient.\n\n",
      
      "Rank:\n",
      "    ",
      rank_mis,
      "\n\n",
      
      "Columns:\n",
      "    ",
      ncol(X_mis),
      "\n\n",
      
      "Inspect selected nuisance controls before proceeding."
    ),
    call. = FALSE
  )
}


message(
  "Baseline MIS validation PASSED."
)


message(
  "N_mis = ",
  N_mis
)


message(
  "beta_mis = ",
  format(
    beta_mis,
    digits = 16
  )
)


message(
  "original/MIS absolute beta difference = ",
  format(
    abs(
      beta_mis -
        beta_original
    ),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 18. Exact original-estimator refit after deletion
# ------------------------------------------------------------------------------

# IMPORTANT:
#
# Keep the baseline-selected nuisance controls fixed.
#
# Each deletion set is therefore refit using exactly the same substantive
# FE-OLS model selected at baseline.
paper_refit <- function(
    data,
    spec
) {
  
  fixest::feols(
    fml = original_formula,
    
    data = data,
    
    cluster = cluster_formula,
    
    ssc = paper_env$stata_cluster_ssc(
      FALSE
    ),
    
    warn = FALSE,
    
    notes = FALSE
  )
}


# Baseline sanity check.
refit_baseline <- paper_refit(
  analysis_sample,
  NULL
)


beta_refit_baseline <- unname(
  stats::coef(
    refit_baseline
  )[[TARGET]]
)


if (
  !is.finite(beta_refit_baseline) ||
  abs(
    beta_refit_baseline -
    beta_original
  ) > BETA_TOLERANCE
) {
  
  stop(
    "Exact Paper 7 baseline refit does not reproduce original beta.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 19. Stable observation-ID check
# ------------------------------------------------------------------------------

if (anyNA(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` contains missing values.",
    call. = FALSE
  )
}


if (anyDuplicated(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` is not unique in Paper 7 estimation sample.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 20. Define MIS audit specification
# ------------------------------------------------------------------------------

spec <- audit_spec(
  study_id = STUDY_ID,
  
  estimand_id = ESTIMAND_ID,
  
  data = analysis_sample,
  
  mis_formula = mis_formula,
  
  target = TARGET,
  
  id_var = "source_row_id",
  
  refit_fn = paper_refit,
  
  refit_target = TARGET,
  
  # Table 2 stores beta only to 3 decimals.
  # The independent -0.010 check was performed above.
  expected_beta = NULL,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 21. MIS SEARCH + EXACT FE-OLS REFITS
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 22. Add standardized instruction-compatible aliases
# ------------------------------------------------------------------------------

audit_result$path$N <- EXPECTED_N


audit_result$path$beta_after <-
  audit_result$path$beta_mis


audit_result$path$delta_beta <-
  audit_result$path$delta_mis


audit_result$path$abs_delta_beta <-
  audit_result$path$abs_delta_mis


audit_result$path$relative_change <-
  audit_result$path$relative_change_mis


audit_result$path$nested <-
  audit_result$path$nested_mis


audit_result$path$valid_refit <-
  audit_result$path$valid_mis


audit_result$path$refit_error <-
  audit_result$path$refit_error_mis


audit_result$baseline$N <-
  audit_result$baseline$n


audit_result$baseline$max_k <-
  max(
    audit_result$path$k,
    na.rm = TRUE
  )


audit_result$baseline$max_removal_fraction <-
  max(
    audit_result$path$removal_fraction,
    na.rm = TRUE
  )


audit_result$summary$N <-
  audit_result$summary$n


# ------------------------------------------------------------------------------
# 23. Save standardized outputs
# ------------------------------------------------------------------------------

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


saved_files <- save_mis_audit(
  audit = audit_result,
  output_dir = OUTPUT_DIR,
  prefix = "audit"
)


# ------------------------------------------------------------------------------
# 24. Ensure influential-ID file has model_row_position
# ------------------------------------------------------------------------------

ids_file <- saved_files$influential_ids


ids_output <- utils::read.csv(
  ids_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


if (
  "position" %in% names(ids_output) &&
  !"model_row_position" %in% names(ids_output)
) {
  
  ids_output$model_row_position <-
    ids_output$position
}


utils::write.csv(
  ids_output,
  ids_file,
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------------------------
# 25. Standard MIS figure
# ------------------------------------------------------------------------------

plot_mis_audit(
  audit = audit_result,
  
  output_file = file.path(
    OUTPUT_DIR,
    "fig_mis_audit.pdf"
  ),
  
  title = paste0(
    "Deutschmann et al.: Signup — Neighbor Spillover"
  )
)


# ------------------------------------------------------------------------------
# 26. Final report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 7 MIS audit complete")
message("============================================================")


message(
  "Study: ",
  STUDY_ID
)


message(
  "Estimand: ",
  ESTIMAND_ID
)


message(
  "Outcome: ",
  OUTCOME
)


message(
  "Target: ",
  TARGET
)


message(
  "Baseline selected controls: ",
  length(SELECTED_CONTROLS)
)


message(
  "N: ",
  EXPECTED_N
)


message(
  "Original beta: ",
  format(
    beta_original,
    digits = 16
  )
)


message(
  "Original beta rounded to 3 decimals: ",
  format(
    round(
      beta_original,
      EXPECTED_BETA_DIGITS
    ),
    nsmall = EXPECTED_BETA_DIGITS
  )
)


message(
  "MIS-compatible beta: ",
  format(
    beta_mis,
    digits = 16
  )
)


message(
  "Absolute baseline difference: ",
  format(
    abs(
      beta_mis -
        beta_original
    ),
    scientific = TRUE
  )
)


message(
  "Maximum k audited: ",
  max(
    audit_result$path$k,
    na.rm = TRUE
  )
)


message(
  "Maximum removal fraction: ",
  format(
    max(
      audit_result$path$removal_fraction,
      na.rm = TRUE
    ),
    digits = 6
  )
)


message(
  "Output folder: ",
  OUTPUT_DIR
)


message("")


print(
  audit_result$baseline
)


print(
  audit_result$summary
)