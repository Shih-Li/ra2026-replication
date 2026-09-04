# ==============================================================================
# audit/scripts/02_LMDIVUiSL.R
#
# Paper:
#   Meriggi et al.
#   "Last-mile delivery increases vaccine uptake in Sierra Leone"
#
# MIS estimand audited here:
#   Table 1, column (1)
#   Outcome: vaccinated_endline
#   Target coefficient: treat_dtd
#   Nuisance treatment coefficient: treat_small
#
# Original estimator:
#   fixest::feols(
#     vaccinated_endline ~ treat_dtd + treat_small | grpID,
#     vcov = ~community_code,
#     ...
#   )
#
# MIS-compatible representation:
#   lm(
#     vaccinated_endline ~
#       treat_dtd +
#       treat_small +
#       factor(grpID)
#   )
#
# Published / replication-output validation:
#   N = 12096
#   beta(treat_dtd), rounded to 3 decimals = 0.293
#
# Important precision note:
#   The repository's retained Table 1 output reports the coefficient only to
#   three decimals. Therefore this script:
#
#   1. validates the original estimator against N = 12096 and beta = 0.293
#      at the reported three-decimal precision;
#   2. validates the MIS-compatible lm against the full-precision coefficient
#      recovered from the original fixest estimator using tolerance 1e-8.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   sign = +1 and -1
#   exact original-estimator refit after every deletion set
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
  
  # Normal case:
  # audit/audit.Rproj is open and this script is sourced from audit/.
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
  
  
  # Otherwise try to infer the audit directory from this sourced script.
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
      'source("scripts/02_LMDIVUiSL.R")'
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
  "02_LMDIVUiSL"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "individual_level.dta"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "02_LMDIVUiSL"
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
# 2. Required packages
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
      paste(
        missing_packages,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 3. Load shared MIS audit machinery
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

STUDY_ID <- "02_LMDIVUiSL"


ESTIMAND_ID <- "table1_col1_door_to_door"


TARGET <- "treat_dtd"


EXPECTED_N <- 12096L


# The retained replication table reports only three decimal places.
EXPECTED_BETA_REPORTED <- 0.293

EXPECTED_BETA_DIGITS <- 3L


# Exact validation tolerance used for:
#
#     MIS-compatible lm
#
# versus:
#
#     original fixest estimator
#
BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# Paper 2 replication small-sample convention.
#
# This reproduces the configuration used in:
#     02_LMDIVUiSL/code/01_setup.R
#
# These settings matter for inference. The MIS target itself is the
# coefficient, but exact refits should preserve the original estimator.
STATA_SSC <- fixest::ssc(
  K.adj = TRUE,
  K.fixef = "full",
  K.exact = TRUE,
  G.adj = TRUE,
  G.df = "min",
  t.df = "min"
)


# ------------------------------------------------------------------------------
# 5. Load analysis-ready source data
# ------------------------------------------------------------------------------

individual_all <- haven::read_dta(
  DATA_FILE
)


# Permanent observation identifier:
#
# row number in the frozen author-supplied analysis-ready
# individual_level.dta file.
#
# IMPORTANT:
# create this BEFORE any filtering.
if ("source_row_id" %in% names(individual_all)) {
  
  stop(
    paste0(
      "`source_row_id` already exists in individual_level.dta. ",
      "Inspect the dataset before proceeding."
    ),
    call. = FALSE
  )
}


individual_all$source_row_id <- seq_len(
  nrow(individual_all)
)


# ------------------------------------------------------------------------------
# 6. Check required variables
# ------------------------------------------------------------------------------

required_vars <- c(
  "above18",
  "in_census",
  "treatment",
  "vaccinated_endline",
  "grpID",
  "community_code",
  "source_row_id"
)


missing_vars <- setdiff(
  required_vars,
  names(individual_all)
)


if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. Reconstruct Table 1, column (1) analysis population
# ------------------------------------------------------------------------------

# Paper 2 Table 1 first restricts to:
#
#     above18 == 1
#     in_census == 1
#
# Do NOT remove influential observations, outliers, or leverage points.
analysis_population <- individual_all[
  !is.na(individual_all$above18) &
    individual_all$above18 == 1 &
    !is.na(individual_all$in_census) &
    individual_all$in_census == 1,
  ,
  drop = FALSE
]


# Reproduce treatment indicators from the replication workflow.
#
# treatment == 0 : control
# treatment == 1 : door-to-door
# treatment == 2 : small-group
analysis_population$treat_dtd <- as.numeric(
  analysis_population$treatment == 1
)

analysis_population$treat_small <- as.numeric(
  analysis_population$treatment == 2
)


# ------------------------------------------------------------------------------
# 8. Freeze the exact estimation sample
# ------------------------------------------------------------------------------

# The original fit_fe_model() helper constructs complete cases using:
#
#   outcome
#   regressors
#   fixed effect
#   cluster variable
#
# Therefore include community_code in the exact sample definition even though
# clustering does not change the coefficient itself.
model_vars <- c(
  "vaccinated_endline",
  "treat_dtd",
  "treat_small",
  "grpID",
  "community_code"
)


analysis_sample <- analysis_population[
  stats::complete.cases(
    analysis_population[
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
    "`source_row_id` is not unique in the estimation sample.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 9. ORIGINAL REPLICATION
# ------------------------------------------------------------------------------

# Table 1 column (1):
#
#   vaccinated_endline
#       ~ treat_dtd
#       + treat_small
#       | grpID
#
# clustered by community_code.
original_fit <- fixest::feols(
  vaccinated_endline ~
    treat_dtd +
    treat_small |
    grpID,
  
  data = analysis_sample,
  
  vcov = ~community_code,
  
  ssc = STATA_SSC,
  
  # Preserve the original replication's FE sample behavior.
  fixef.rm = "none",
  
  notes = FALSE,
  warn = TRUE
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
# 10. Validate ORIGINAL REPLICATION against retained Paper 2 result
# ------------------------------------------------------------------------------

# Exact expected N is available from the retained replication output.
if (N_original != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    02_LMDIVUiSL.R\n\n",
      
      "Problem:\n",
      "    Original Paper 2 estimator does not reproduce expected N.\n\n",
      
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
      "    above18/in_census restriction, missingness, treatment coding, ",
      "fixed effects, or cluster-variable sample mismatch."
    ),
    call. = FALSE
  )
}


# The stored replication table reports beta only to three decimal places.
#
# Therefore validate that independently available published/replication value
# at exactly the precision supplied by the repository.
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
      "    02_LMDIVUiSL.R\n\n",
      
      "Problem:\n",
      "    Original Paper 2 estimator does not reproduce the retained ",
      "Table 1 coefficient at its reported precision.\n\n",
      
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
      ),
      "\n\n",
      
      "Likely cause:\n",
      "    treatment coding / fixed effects / sample / missingness mismatch."
    ),
    call. = FALSE
  )
}


message(
  "Original Paper 2 replication check PASSED."
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
# 11. MIS-compatible OLS representation
# ------------------------------------------------------------------------------

# Absorbed grpID fixed effects are represented explicitly as dummy variables.
#
# treat_small remains in the model as a nuisance treatment coefficient.
mis_formula <- stats::as.formula(
  vaccinated_endline ~
    treat_dtd +
    treat_small +
    factor(grpID)
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
      "` is absent from mis_fit."
    ),
    call. = FALSE
  )
}


beta_mis <- unname(
  mis_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 12. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    02_LMDIVUiSL.R\n\n",
      
      "Problem:\n",
      "    MIS-compatible lm does not use the original estimator's N.\n\n",
      
      "Expected:\n",
      "    N = ", N_original, "\n",
      "    beta = ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "Obtained:\n",
      "    N = ", N_mis, "\n",
      "    beta = ",
      format(
        beta_mis,
        digits = 16
      ),
      "\n\n",
      
      "Likely cause:\n",
      "    fixed-effect representation / missingness / formula mismatch."
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
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    02_LMDIVUiSL.R\n\n",
      
      "Problem:\n",
      "    MIS-compatible lm does not reproduce the original target slope.\n\n",
      
      "Expected:\n",
      "    N = ", N_original, "\n",
      "    beta = ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "Obtained:\n",
      "    N = ", N_mis, "\n",
      "    beta = ",
      format(
        beta_mis,
        digits = 16
      ),
      "\n",
      
      "    absolute difference = ",
      format(
        abs(
          beta_mis -
            beta_original
        ),
        scientific = TRUE
      ),
      "\n\n",
      
      "Likely cause:\n",
      "    explicit grpID dummy representation differs from the ",
      "original absorbed fixed-effect model."
    ),
    call. = FALSE
  )
}


# Required full-rank check.
X_mis <- stats::model.matrix(
  mis_fit
)


rank_mis <- qr(
  X_mis
)$rank


if (rank_mis < ncol(X_mis)) {
  
  stop(
    paste0(
      "MIS model matrix is not full rank: rank = ",
      rank_mis,
      ", columns = ",
      ncol(X_mis),
      "."
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
    abs(
      beta_mis -
        beta_original
    ),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 13. Original-estimator function for exact deletion refits
# ------------------------------------------------------------------------------

paper_refit <- function(
    data,
    spec
) {
  
  fixest::feols(
    vaccinated_endline ~
      treat_dtd +
      treat_small |
      grpID,
    
    data = data,
    
    vcov = ~community_code,
    
    ssc = STATA_SSC,
    
    fixef.rm = "none",
    
    notes = FALSE,
    warn = TRUE
  )
}


# ------------------------------------------------------------------------------
# 14. Define the MIS audit specification
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
  
  # IMPORTANT:
  #
  # Do not invent an unavailable full-precision expected coefficient.
  #
  # We already independently validated beta_original against the repository's
  # retained 0.293 value above.
  #
  # prepare_mis_audit() will still require the MIS lm and original fixest
  # estimator to agree at BETA_TOLERANCE.
  expected_beta = NULL,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  # Explicitly enforce the instruction's 5% deletion protocol.
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 15. MIS SEARCH + EXACT REFIT
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 16. Add instruction-compatible output aliases
# ------------------------------------------------------------------------------

# The shared engine uses some internal names that differ slightly from the
# empirical-audit instruction. Preserve the internal fields and add the
# standardized aliases required by the instruction.


# ---- path output --------------------------------------------------------------

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


# ---- baseline output ----------------------------------------------------------

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


# ---- summary output -----------------------------------------------------------

audit_result$summary$N <-
  audit_result$summary$n


# ------------------------------------------------------------------------------
# 17. Save standardized outputs
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
# 18. Ensure influential-ID output contains model_row_position
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
# 19. Standard MIS figure
# ------------------------------------------------------------------------------

plot_mis_audit(
  audit = audit_result,
  
  output_file = file.path(
    OUTPUT_DIR,
    "fig_mis_audit.pdf"
  ),
  
  title = paste0(
    "Meriggi et al.: Vaccination — Door-to-Door Treatment"
  )
)


# ------------------------------------------------------------------------------
# 20. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 2 MIS audit complete")
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
  "MIS/original absolute difference: ",
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