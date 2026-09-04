# ==============================================================================
# audit/scripts/04_SBN-HIWTO.R
#
# Paper:
#   Ciancio, Kämpfen, Kohler, and Thornton
#   "Surviving Bad News: Health Information without Treatment Options"
#
# IMPORTANT SCOPE NOTE
# ------------------------------------------------------------------------------
# The paper's preferred causal specifications are IV / efficient linear GMM.
# The current shared MIS engine (dinkelbach_topk_lm) is NOT valid for IV/GMM.
#
# Therefore this script audits the OLS companion specification reported in
# Table 2. It MUST NOT be described as an MIS audit of the paper's IV-GMM
# causal estimate.
#
# Estimand audited here:
#   Table 2
#   Survival in 2018
#   OLS companion specification
#
# Outcome:
#   alive2018
#
# Target:
#   learnhivpos
#
# Nuisance treatment/information coefficient:
#   learnhivneg
#
# Controls:
#   hiv
#   male
#   age
#   age2
#   south
#   north
#
# Original Table 2 OLS coefficient:
#   learnhivpos ≈ -0.157
#   N = 2332
#
# Clustering:
#   DK_village_number
#
# Clustering affects inference but not the OLS coefficient itself.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both coefficient-increasing and coefficient-decreasing directions
#   exact OLS refit after every deletion set
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. General options
# ------------------------------------------------------------------------------

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ------------------------------------------------------------------------------
# 1. Locate repository
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
      "Could not locate the audit project.\n",
      "Open audit/audit.Rproj and run:\n",
      'source("scripts/04_SBN-HIWTO.R")'
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
  "04_SBN-HIWTO"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "processed",
  "hivtest_mortality.rds"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "04_SBN-HIWTO"
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
    paste0(
      "Required analysis-ready dataset does not exist:\n",
      DATA_FILE,
      "\n\n",
      "Run the Paper 4 replication first."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
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
# 3. Load shared MIS machinery
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
# 4. Paper-specific constants
# ------------------------------------------------------------------------------

STUDY_ID <- "04_SBN-HIWTO"


# Explicitly include OLS in the estimand ID.
#
# This prevents this audit from being confused with the preferred IV-GMM
# specification.
ESTIMAND_ID <- "table2_2018_ols_hiv_positive"


TARGET <- "learnhivpos"


EXPECTED_N <- 2332L


# Retained Table 2 output reports coefficients to three decimal places.
EXPECTED_BETA_REPORTED <- -0.157


EXPECTED_BETA_DIGITS <- 3L


BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 5. Load analysis-ready data
# ------------------------------------------------------------------------------

analysis_all <- readRDS(
  DATA_FILE
)


if (!is.data.frame(analysis_all)) {
  
  analysis_all <- as.data.frame(
    analysis_all
  )
}


# ------------------------------------------------------------------------------
# 6. Permanent observation ID
# ------------------------------------------------------------------------------

# Create the ID BEFORE imposing the Table 2 estimation sample.
#
# This is the permanent row identifier in the frozen author-replication
# analysis-ready object hivtest_mortality.rds.
if ("source_row_id" %in% names(analysis_all)) {
  
  stop(
    paste0(
      "`source_row_id` already exists in hivtest_mortality.rds. ",
      "Inspect the processed dataset before proceeding."
    ),
    call. = FALSE
  )
}


analysis_all$source_row_id <- seq_len(
  nrow(analysis_all)
)


# ------------------------------------------------------------------------------
# 7. Required variables
# ------------------------------------------------------------------------------

demog <- c(
  "hiv",
  "male",
  "age",
  "age2",
  "south",
  "north"
)


required_vars <- c(
  "alive2018",
  demog,
  "learnhivneg",
  "learnhivpos",
  "DK_village_number",
  "source_row_id"
)


missing_vars <- setdiff(
  required_vars,
  names(analysis_all)
)


if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required Paper 4 variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8. Reconstruct exact Table 2 OLS estimation sample
# ------------------------------------------------------------------------------

# Original helper fit_gmm_iv() forms its sample using complete cases over:
#
#   outcome
#   regressors
#   cluster
#
# even when the model is the OLS companion specification.
#
# DK_village_number must therefore be included when freezing the sample,
# although clustering does not change the coefficient.
model_vars <- c(
  "alive2018",
  demog,
  "learnhivneg",
  "learnhivpos",
  "DK_village_number"
)


analysis_sample <- analysis_all[
  stats::complete.cases(
    analysis_all[
      ,
      model_vars,
      drop = FALSE
    ]
  ),
  ,
  drop = FALSE
]


row.names(analysis_sample) <- NULL


if (anyNA(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` contains missing values.",
    call. = FALSE
  )
}


if (anyDuplicated(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` is not unique in the Table 2 sample.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 9. Exact Table 2 OLS formula
# ------------------------------------------------------------------------------

rhs_vars <- c(
  demog,
  "learnhivneg",
  "learnhivpos"
)


original_formula <- stats::reformulate(
  termlabels = rhs_vars,
  response = "alive2018"
)


# ------------------------------------------------------------------------------
# 10. ORIGINAL REPLICATION — OLS companion model
# ------------------------------------------------------------------------------

original_fit <- stats::lm(
  formula = original_formula,
  data = analysis_sample,
  na.action = stats::na.fail
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
      "` is absent from original_fit."
    ),
    call. = FALSE
  )
}


beta_original <- unname(
  original_coef[[TARGET]]
)


if (
  length(beta_original) != 1L ||
  !is.finite(beta_original)
) {
  
  stop(
    "Could not recover a finite original target coefficient.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 11. Validate ORIGINAL OLS replication
# ------------------------------------------------------------------------------

if (N_original != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    04_SBN-HIWTO.R\n\n",
      
      "Problem:\n",
      "    Table 2 2018 OLS specification does not reproduce expected N.\n\n",
      
      "Expected:\n",
      "    N = ", EXPECTED_N, "\n",
      "    beta rounded to ",
      EXPECTED_BETA_DIGITS,
      " decimals = ",
      format(
        EXPECTED_BETA_REPORTED,
        nsmall = EXPECTED_BETA_DIGITS
      ),
      "\n\n",
      
      "Obtained:\n",
      "    N = ", N_original, "\n",
      "    beta = ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "Likely cause:\n",
      "    outcome missingness, demographic controls, learning-status ",
      "variables, or village-cluster sample mismatch."
    ),
    call. = FALSE
  )
}


beta_original_reported <- round(
  beta_original,
  digits = EXPECTED_BETA_DIGITS
)


if (
  abs(
    beta_original_reported -
    EXPECTED_BETA_REPORTED
  ) > 1e-12
) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    04_SBN-HIWTO.R\n\n",
      
      "Problem:\n",
      "    Table 2 OLS coefficient does not reproduce the retained ",
      "three-decimal result.\n\n",
      
      "Expected:\n",
      "    beta rounded to ",
      EXPECTED_BETA_DIGITS,
      " decimals = ",
      format(
        EXPECTED_BETA_REPORTED,
        nsmall = EXPECTED_BETA_DIGITS
      ),
      "\n\n",
      
      "Obtained:\n",
      "    beta full precision = ",
      format(
        beta_original,
        digits = 16
      ),
      "\n",
      
      "    beta rounded to ",
      EXPECTED_BETA_DIGITS,
      " decimals = ",
      format(
        beta_original_reported,
        nsmall = EXPECTED_BETA_DIGITS
      )
    ),
    call. = FALSE
  )
}


message(
  "Original Paper 4 Table 2 OLS replication check PASSED."
)


message(
  "IMPORTANT: this is the OLS companion specification, NOT the IV-GMM estimate."
)


message(
  "N_original = ",
  N_original
)


message(
  "beta_original = ",
  format(
    beta_original,
    digits = 16
  )
)


message(
  "beta_original rounded to 3 decimals = ",
  format(
    beta_original_reported,
    nsmall = EXPECTED_BETA_DIGITS
  )
)


# ------------------------------------------------------------------------------
# 12. MIS-compatible representation
# ------------------------------------------------------------------------------

# This specification is already ordinary OLS.
mis_formula <- original_formula


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
      "` is absent from mis_fit."
    ),
    call. = FALSE
  )
}


beta_mis <- unname(
  mis_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 13. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  
  stop(
    paste0(
      "MIS model N differs from original OLS model N.\n",
      "Original N = ", N_original, "\n",
      "MIS N = ", N_mis
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
      "MIS-compatible OLS does not reproduce the original slope.\n",
      "Original beta = ",
      format(beta_original, digits = 16),
      "\n",
      "MIS beta = ",
      format(beta_mis, digits = 16),
      "\n",
      "Absolute difference = ",
      format(
        abs(beta_mis - beta_original),
        scientific = TRUE
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 14. Full-rank check
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
      "MIS model matrix is not full rank.\n",
      "Rank = ", rank_mis, "\n",
      "Columns = ", ncol(X_mis)
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
  "absolute beta difference = ",
  format(
    abs(beta_mis - beta_original),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 15. Exact OLS refit function
# ------------------------------------------------------------------------------

paper_refit <- function(
    data,
    spec
) {
  
  stats::lm(
    formula = original_formula,
    data = data,
    na.action = stats::na.fail
  )
}


# ------------------------------------------------------------------------------
# 16. Define MIS audit specification
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
  
  # The retained Table 2 file reports only three decimal places.
  #
  # We independently validated -0.157 above.
  # Do not fabricate unavailable full precision here.
  expected_beta = NULL,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 17. MIS SEARCH + EXACT REFIT
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 18. Standardized aliases
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
# 19. Save standardized outputs
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
# 20. Ensure influential-ID output contains model_row_position
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
# 21. Standard MIS figure
# ------------------------------------------------------------------------------

plot_mis_audit(
  audit = audit_result,
  
  output_file = file.path(
    OUTPUT_DIR,
    "fig_mis_audit.pdf"
  ),
  
  title = paste0(
    "Ciancio et al.: 2018 Survival — HIV-positive Information (OLS)"
  )
)


# ------------------------------------------------------------------------------
# 22. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 4 OLS MIS audit complete")
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
  "Outcome: alive2018"
)


message(
  "Target: ",
  TARGET
)


message(
  "Estimator audited: OLS companion specification"
)


message(
  "Preferred IV-GMM estimator audited: NO"
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