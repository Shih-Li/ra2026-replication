# ==============================================================================
# audit/scripts/01_RSPitHUMM.R
#
# Paper:
#   Riley, Emma (2023)
#   "Resisting Social Pressure in the Household Using Mobile Money:
#    Experimental Evidence on Microenterprise Investment in Uganda"
#
# MIS estimand audited here:
#   Table 1 primary outcome: capital
#   Target coefficient: treatment3 ("Mobile Disburse")
#
# Original estimator:
#   fixest::feols(
#     capital ~ capital_base + treatment2 + treatment3 | strata_fixed_base,
#     vcov = "hetero",
#     fixef.rm = "none"
#   )
#
# MIS-compatible representation:
#   lm(
#     capital ~ capital_base + treatment2 + treatment3 +
#       factor(strata_fixed_base)
#   )
#
# Required validation:
#   N = 2639
#   beta(treatment3) = 69.2071674949056
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   sign = +1 and -1
#   exact paper-estimator refit after every deletion set
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Locate repository
# ------------------------------------------------------------------------------

options(stringsAsFactors = FALSE, scipen = 999)


find_audit_root <- function() {
  
  # Normal case: opened audit/audit.Rproj and sourced this script.
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
  
  # Otherwise infer location from the sourced script.
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
      "Open audit/audit.Rproj and then run:\n",
      "source(\"scripts/01_RSPitHUMM.R\")"
    ),
    call. = FALSE
  )
}


AUDIT_ROOT <- find_audit_root()
REPO_ROOT  <- dirname(AUDIT_ROOT)

PAPER_ROOT <- file.path(
  REPO_ROOT,
  "01_RSPitHUMM"
)

DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "survey_data.dta"
)

OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "01_RSPitHUMM"
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
    "Required analysis-ready dataset does not exist: ",
    DATA_FILE,
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 1. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "haven",
  "fixest",
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
      "Install required package(s) first: ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2. Load shared MIS audit machinery
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
# 3. Paper-specific constants
# ------------------------------------------------------------------------------

STUDY_ID <- "01_RSPitHUMM"

ESTIMAND_ID <- "table1_capital_mobile_disburse"

TARGET <- "treatment3"

EXPECTED_N <- 2639L

EXPECTED_BETA <- 69.2071674949056

BETA_TOLERANCE <- 1e-8

MAX_REMOVAL_FRACTION <- 0.05


# Replicate the Stata/fixest small-sample convention used by Paper 1.
STATA_SSC <- fixest::ssc(
  K.adj = TRUE,
  K.fixef = "full",
  K.exact = TRUE
)


# ------------------------------------------------------------------------------
# 4. Load the analysis-ready source data
# ------------------------------------------------------------------------------

survey_all <- haven::read_dta(
  DATA_FILE
)


# Permanent audit identifier:
# row position in the frozen author-supplied analysis-ready survey_data.dta.
#
# This is created BEFORE any sample restriction so that MIS row positions
# can always be mapped back to the source dataset.
if ("source_row_id" %in% names(survey_all)) {
  stop(
    paste0(
      "`source_row_id` already exists in survey_data.dta. ",
      "Inspect the dataset before proceeding."
    ),
    call. = FALSE
  )
}

survey_all$source_row_id <- seq_len(
  nrow(survey_all)
)


# ------------------------------------------------------------------------------
# 5. Reconstruct the original Table 1 analysis sample
# ------------------------------------------------------------------------------

required_vars <- c(
  "consent",
  "capital",
  "capital_base",
  "treatment2",
  "treatment3",
  "strata_fixed_base",
  "source_row_id"
)

missing_vars <- setdiff(
  required_vars,
  names(survey_all)
)

if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required variable(s): ",
      paste(missing_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}


# Main analysis first restricts to respondents with endline consent.
survey <- survey_all[
  !is.na(survey_all$consent) &
    survey_all$consent == 1,
  ,
  drop = FALSE
]


# Exact variables used by the target specification.
model_vars <- c(
  "capital",
  "capital_base",
  "treatment2",
  "treatment3",
  "strata_fixed_base"
)


# Explicitly identify the exact estimation sample.
analysis_sample <- survey[
  stats::complete.cases(
    survey[, model_vars, drop = FALSE]
  ),
  ,
  drop = FALSE
]

row.names(analysis_sample) <- NULL


if (anyDuplicated(analysis_sample$source_row_id)) {
  stop(
    "`source_row_id` is not unique in the estimation sample.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 6. ORIGINAL REPLICATION
# ------------------------------------------------------------------------------

original_fit <- fixest::feols(
  capital ~
    capital_base +
    treatment2 +
    treatment3 |
    strata_fixed_base,
  
  data = analysis_sample,
  
  vcov = "hetero",
  
  ssc = STATA_SSC,
  
  # Required to reproduce Stata areg sample behavior.
  fixef.rm = "none",
  
  notes = FALSE,
  warn = TRUE
)


N_original <- as.integer(
  stats::nobs(original_fit)
)

beta_original <- unname(
  stats::coef(original_fit)[[TARGET]]
)


if (
  length(beta_original) != 1L ||
  !is.finite(beta_original)
) {
  stop(
    "Could not recover the original target coefficient.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. Validate original replication against known Table 1 result
# ------------------------------------------------------------------------------

if (N_original != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    01_RSPitHUMM.R\n\n",
      "Problem:\n",
      "    Original Paper 1 estimator does not reproduce expected N.\n\n",
      "Expected:\n",
      "    N = ", EXPECTED_N, "\n",
      "    beta = ", format(EXPECTED_BETA, digits = 16), "\n\n",
      "Obtained:\n",
      "    N = ", N_original, "\n",
      "    beta = ", format(beta_original, digits = 16), "\n\n",
      "Likely cause:\n",
      "    sample restriction / missingness / variable mismatch."
    ),
    call. = FALSE
  )
}


if (
  abs(beta_original - EXPECTED_BETA) >
  BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    01_RSPitHUMM.R\n\n",
      "Problem:\n",
      "    Original Paper 1 estimator does not reproduce expected beta.\n\n",
      "Expected:\n",
      "    N = ", EXPECTED_N, "\n",
      "    beta = ", format(EXPECTED_BETA, digits = 16), "\n\n",
      "Obtained:\n",
      "    N = ", N_original, "\n",
      "    beta = ", format(beta_original, digits = 16), "\n\n",
      "Likely cause:\n",
      "    fixed effects / sample / formula / variable transformation mismatch."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8. MIS-compatible OLS representation
# ------------------------------------------------------------------------------

mis_formula <- stats::as.formula(
  capital ~
    capital_base +
    treatment2 +
    treatment3 +
    factor(strata_fixed_base)
)


mis_fit <- stats::lm(
  formula = mis_formula,
  data = analysis_sample,
  na.action = stats::na.fail
)


N_mis <- as.integer(
  stats::nobs(mis_fit)
)

beta_mis <- unname(
  stats::coef(mis_fit)[[TARGET]]
)


# ------------------------------------------------------------------------------
# 9. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    01_RSPitHUMM.R\n\n",
      "Problem:\n",
      "    MIS-compatible lm does not use the original estimator's N.\n\n",
      "Expected:\n",
      "    N = ", N_original, "\n",
      "    beta = ", format(beta_original, digits = 16), "\n\n",
      "Obtained:\n",
      "    N = ", N_mis, "\n",
      "    beta = ", format(beta_mis, digits = 16), "\n\n",
      "Likely cause:\n",
      "    fixed-effect representation / missingness / formula mismatch."
    ),
    call. = FALSE
  )
}


if (
  !is.finite(beta_mis) ||
  abs(beta_mis - beta_original) >
  BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    01_RSPitHUMM.R\n\n",
      "Problem:\n",
      "    MIS-compatible lm does not reproduce the original target slope.\n\n",
      "Expected:\n",
      "    N = ", N_original, "\n",
      "    beta = ", format(beta_original, digits = 16), "\n\n",
      "Obtained:\n",
      "    N = ", N_mis, "\n",
      "    beta = ", format(beta_mis, digits = 16), "\n\n",
      "Likely cause:\n",
      "    explicit fixed-effect dummy representation differs from ",
      "the original absorbed-FE model."
    ),
    call. = FALSE
  )
}


X_mis <- stats::model.matrix(
  mis_fit
)

if (qr(X_mis)$rank < ncol(X_mis)) {
  
  stop(
    paste0(
      "MIS model matrix is not full rank: rank = ",
      qr(X_mis)$rank,
      ", columns = ",
      ncol(X_mis),
      "."
    ),
    call. = FALSE
  )
}


message(
  "Baseline validation PASSED."
)

message(
  "N_original = ",
  N_original
)

message(
  "beta_original = ",
  format(beta_original, digits = 16)
)

message(
  "beta_mis = ",
  format(beta_mis, digits = 16)
)

message(
  "absolute difference = ",
  format(
    abs(beta_mis - beta_original),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 10. Original-estimator function for exact deletion refits
# ------------------------------------------------------------------------------

paper_refit <- function(
    data,
    spec
) {
  
  fixest::feols(
    capital ~
      capital_base +
      treatment2 +
      treatment3 |
      strata_fixed_base,
    
    data = data,
    
    vcov = "hetero",
    
    ssc = STATA_SSC,
    
    fixef.rm = "none",
    
    notes = FALSE,
    warn = TRUE
  )
}


# ------------------------------------------------------------------------------
# 11. Define the MIS audit specification
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
  
  expected_beta = EXPECTED_BETA,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  # Explicitly enforce the instruction's 5% protocol,
  # regardless of any default inside audit_validate.R.
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 12. MIS SEARCH + EXACT REFIT
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 13. Add instruction-compatible output aliases
#
# The shared engine currently uses internal names such as beta_mis and
# delta_mis. Keep those internal fields, but also add the standardized names
# requested by the empirical-audit instruction.
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


# Baseline aliases required by the audit specification.
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


# Summary alias.
audit_result$summary$N <-
  audit_result$summary$n


# ------------------------------------------------------------------------------
# 14. Save standardized outputs
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
# 15. Ensure influential-ID output contains model_row_position
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
# 16. Standard MIS figure
# ------------------------------------------------------------------------------

plot_mis_audit(
  audit = audit_result,
  
  output_file = file.path(
    OUTPUT_DIR,
    "fig_mis_audit.pdf"
  ),
  
  title = paste0(
    "Riley (2023): Capital — Mobile Disburse"
  )
)


# ------------------------------------------------------------------------------
# 17. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 1 MIS audit complete")
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
  "Target: ",
  TARGET
)

message(
  "N: ",
  EXPECTED_N
)

message(
  "Original beta: ",
  format(beta_original, digits = 16)
)

message(
  "Maximum k audited: ",
  max(audit_result$path$k)
)

message(
  "Maximum removal fraction: ",
  format(
    max(audit_result$path$removal_fraction),
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