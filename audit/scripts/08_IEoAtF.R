# ==============================================================================
# audit/scripts/08_IEoAtF.R
#
# Paper:
#   Cai and Szeidl
#   "Indirect Effects of Access to Finance"
#
# MIS estimand:
#   Table 3 — Main firm-level effects
#
# Outcome:
#   lnpart5revenue
#
# Target:
#   inter4post
#
# Interpretation:
#   inter4post = post * treatratio_comp
#   => post-treatment exposure to the share of competitors treated
#
# Other substantive coefficient retained:
#   interpost = post * type
#   => own treatment effect
#
# Original specification:
#
#   lnpart5revenue ~
#       post +
#       interpost +
#       inter4post
#       | firmid
#
# Cluster:
#   survey_town
#
# Current R replication benchmark:
#   beta(inter4post) = -0.0855461058717665
#   reported N       = 8612
#
# IMPORTANT FIXED-EFFECT N NOTE
# ------------------------------------------------------------------------------
# The Paper 8 replication documents that fixest can remove singleton FE groups,
# while the Stata-style reported N counts the complete estimation sample.
#
# Therefore:
#
#   - the audit sample is the complete Table-3 sample (N = 8612);
#   - MIS represents firm fixed effects explicitly with factor(firmid);
#   - exact deletion refits use the Paper-8 fixest FE estimator;
#   - paper_refit() returns the exact target coefficient directly so the audit
#     engine does not confuse fixest singleton removal with observation deletion.
#
# Baseline coefficient equivalence must hold within 1e-8 before MIS runs.
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
    dir.exists(
      file.path(
        wd,
        "function"
      )
    )
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
      dirname(
        script_file
      )
    )
    
    
    if (
      basename(candidate) == "audit" &&
      dir.exists(
        file.path(
          candidate,
          "function"
        )
      )
    ) {
      
      return(candidate)
    }
  }
  
  
  stop(
    paste0(
      "Could not locate audit project.\n",
      "Open audit/audit.Rproj and run:\n",
      'source("scripts/08_IEoAtF.R")'
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
  "08_IEoAtF"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "loanmain.dta"
)


BENCHMARK_FILE <- file.path(
  PAPER_ROOT,
  "results",
  "tables",
  "R-Table3.csv"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "08_IEoAtF"
)


if (!dir.exists(PAPER_ROOT)) {
  
  stop(
    "Paper folder does not exist: ",
    PAPER_ROOT,
    call. = FALSE
  )
}


for (f in c(
  DATA_FILE,
  BENCHMARK_FILE
)) {
  
  if (!file.exists(f)) {
    
    stop(
      "Required Paper 8 file does not exist:\n",
      f,
      call. = FALSE
    )
  }
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "haven",
  "dplyr",
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
# 4. Load Paper 8 replication helper in isolated environment
# ------------------------------------------------------------------------------

paper_env <- new.env(
  parent = globalenv()
)


# Prevent 02_helpers.R from sourcing its own project setup.
paper_env$paths <- list(
  loanmain = DATA_FILE
)


source(
  file.path(
    PAPER_ROOT,
    "code",
    "02_helpers.R"
  ),
  local = paper_env
)


# ------------------------------------------------------------------------------
# 5. Paper-specific constants
# ------------------------------------------------------------------------------

STUDY_ID <- "08_IEoAtF"


ESTIMAND_ID <-
  "table3_log_sales_competitor_treatment_spillover"


OUTCOME <- "lnpart5revenue"


TARGET <- "inter4post"


RHS <- c(
  "post",
  "interpost",
  "inter4post"
)


FE_VAR <- "firmid"


CLUSTER_VAR <- "survey_town"


BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 6. Prepare Paper 8 analysis-ready data
# ------------------------------------------------------------------------------

# This calls the same preparation routine used by code/04_main_effects_robustness.R.
d <- paper_env$prepare_loanmain()


d <- as.data.frame(
  d
)


# prepare_loanmain() does not intentionally change the observation unit.
# Keep a stable row identifier before imposing the Table 3 estimation sample.
if ("source_row_id" %in% names(d)) {
  
  stop(
    "`source_row_id` already exists in Paper 8 data.",
    call. = FALSE
  )
}


d$source_row_id <- seq_len(
  nrow(d)
)


# ------------------------------------------------------------------------------
# 7. Required variables
# ------------------------------------------------------------------------------

required_vars <- c(
  OUTCOME,
  RHS,
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
      "Missing required Paper 8 variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8. Read independent Table 3 benchmark
# ------------------------------------------------------------------------------

benchmark <- utils::read.csv(
  BENCHMARK_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


benchmark_row <- benchmark[
  benchmark$model == "Table3_lnpart5revenue" &
    benchmark$outcome == OUTCOME &
    benchmark$term == TARGET,
  ,
  drop = FALSE
]


if (nrow(benchmark_row) != 1L) {
  
  stop(
    paste0(
      "Expected exactly one Table 3 benchmark row for ",
      OUTCOME,
      " / ",
      TARGET,
      ", but found ",
      nrow(benchmark_row),
      "."
    ),
    call. = FALSE
  )
}


EXPECTED_BETA <- as.numeric(
  benchmark_row$estimate[[1]]
)


EXPECTED_N <- as.integer(
  benchmark_row$n[[1]]
)


if (
  !is.finite(EXPECTED_BETA) ||
  is.na(EXPECTED_N)
) {
  
  stop(
    "Could not recover Paper 8 benchmark beta/N.",
    call. = FALSE
  )
}


message(
  "Expected Paper 8 beta = ",
  format(
    EXPECTED_BETA,
    digits = 16
  )
)


message(
  "Expected reported N = ",
  EXPECTED_N
)


# ------------------------------------------------------------------------------
# 9. ORIGINAL PAPER 8 Table 3 estimator
# ------------------------------------------------------------------------------

original_fit <- paper_env$fit_ols(
  data = d,
  
  outcome = OUTCOME,
  
  terms = RHS,
  
  cluster = CLUSTER_VAR,
  
  fe = FE_VAR
)


original_coef <- stats::coef(
  original_fit
)


if (!TARGET %in% names(original_coef)) {
  
  stop(
    paste0(
      "Target coefficient `",
      TARGET,
      "` is absent from original Paper 8 model."
    ),
    call. = FALSE
  )
}


beta_original <- unname(
  original_coef[[TARGET]]
)


# Paper-8 helper stores a Stata-style complete-sample N separately from
# fixest::nobs(), because fixest may remove singleton FE groups.
N_fixest <- as.integer(
  stats::nobs(
    original_fit
  )
)


N_reported <- attr(
  original_fit,
  "ra_report_n"
)


if (
  is.null(N_reported) ||
  !is.finite(N_reported)
) {
  
  stop(
    "Paper 8 original model does not contain `ra_report_n`.",
    call. = FALSE
  )
}


N_reported <- as.integer(
  N_reported
)


# ------------------------------------------------------------------------------
# 10. Validate original replication
# ------------------------------------------------------------------------------

if (N_reported != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    08_IEoAtF.R\n\n",
      
      "Problem:\n",
      "    Paper 8 Table 3 does not reproduce benchmark reported N.\n\n",
      
      "Expected:\n",
      "    N = ",
      EXPECTED_N,
      "\n\n",
      
      "Obtained:\n",
      "    reported N = ",
      N_reported,
      "\n",
      "    fixest nobs = ",
      N_fixest
    ),
    call. = FALSE
  )
}


if (
  !is.finite(beta_original) ||
  abs(
    beta_original -
    EXPECTED_BETA
  ) > BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "\n",
      "Original Paper 8 Table 3 coefficient does not reproduce ",
      "R-Table3.csv.\n\n",
      
      "Expected beta:\n",
      "    ",
      format(
        EXPECTED_BETA,
        digits = 16
      ),
      "\n\n",
      
      "Obtained beta:\n",
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
            EXPECTED_BETA
        ),
        scientific = TRUE
      )
    ),
    call. = FALSE
  )
}


message(
  "Original Paper 8 replication check PASSED."
)


message(
  "Reported N = ",
  N_reported
)


message(
  "fixest nobs = ",
  N_fixest
)


message(
  "beta_original = ",
  format(
    beta_original,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 11. Freeze exact Table 3 complete-case sample
# ------------------------------------------------------------------------------

model_vars <- unique(
  c(
    OUTCOME,
    RHS,
    FE_VAR,
    CLUSTER_VAR
  )
)


sample_keep <- stats::complete.cases(
  d[
    ,
    model_vars,
    drop = FALSE
  ]
)


analysis_sample <- d[
  sample_keep,
  ,
  drop = FALSE
]


row.names(
  analysis_sample
) <- NULL


if (nrow(analysis_sample) != EXPECTED_N) {
  
  stop(
    paste0(
      "Frozen Table 3 complete-case sample does not equal benchmark N.\n",
      "Expected N = ",
      EXPECTED_N,
      "\n",
      "Frozen N = ",
      nrow(analysis_sample)
    ),
    call. = FALSE
  )
}


if (anyNA(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` contains missing values.",
    call. = FALSE
  )
}


if (anyDuplicated(analysis_sample$source_row_id)) {
  
  stop(
    "`source_row_id` is not unique in Paper 8 audit sample.",
    call. = FALSE
  )
}


message(
  "Frozen Paper 8 audit sample contains ",
  nrow(analysis_sample),
  " observations."
)


# ------------------------------------------------------------------------------
# 12. MIS-compatible explicit firm-FE model
# ------------------------------------------------------------------------------

# The cluster-robust VCE affects inference only, not the OLS coefficient.
#
# Absorbed firm fixed effects are represented explicitly as factor(firmid).
mis_formula <- stats::as.formula(
  paste0(
    OUTCOME,
    " ~ ",
    paste(
      c(
        RHS,
        paste0(
          "factor(",
          FE_VAR,
          ")"
        )
      ),
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
  stats::nobs(
    mis_fit
  )
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
# 13. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != EXPECTED_N) {
  
  stop(
    paste0(
      "MIS model does not use the complete Table 3 sample.\n",
      "Expected N = ",
      EXPECTED_N,
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
      "MIS explicit-firm-FE model does not reproduce ",
      "the original Paper 8 slope.\n\n",
      
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
      ),
      "\n\n",
      
      "Do not run MIS until this discrepancy is resolved."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 14. Required full-rank check
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
      "MIS model matrix is rank deficient.\n",
      "Rank = ",
      rank_mis,
      "\n",
      "Columns = ",
      ncol(X_mis)
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
# 15. Exact Paper 8 estimator refit after deletion
# ------------------------------------------------------------------------------

paper_refit <- function(
    data,
    spec
) {
  
  fit <- paper_env$fit_ols(
    data = data,
    
    outcome = OUTCOME,
    
    terms = RHS,
    
    cluster = CLUSTER_VAR,
    
    fe = FE_VAR
  )
  
  
  b <- stats::coef(
    fit
  )
  
  
  if (!TARGET %in% names(b)) {
    
    stop(
      paste0(
        "Target `",
        TARGET,
        "` is absent after Paper 8 refit."
      )
    )
  }
  
  
  beta <- unname(
    b[[TARGET]]
  )
  
  
  if (!is.finite(beta)) {
    
    stop(
      "Paper 8 exact refit returned a non-finite target coefficient."
    )
  }
  
  
  # Return the exact coefficient directly.
  #
  # This is intentional because fixest may remove singleton FE groups and
  # therefore report nobs smaller than the complete Table-3 sample even though
  # the FE slope is algebraically the same.
  beta
}


# Baseline exact-refit sanity check.
beta_refit_baseline <- paper_refit(
  analysis_sample,
  NULL
)


if (
  abs(
    beta_refit_baseline -
    beta_original
  ) > BETA_TOLERANCE
) {
  
  stop(
    paste0(
      "Exact Paper 8 refit does not reproduce baseline beta.\n",
      "Original = ",
      format(
        beta_original,
        digits = 16
      ),
      "\n",
      "Refit = ",
      format(
        beta_refit_baseline,
        digits = 16
      )
    ),
    call. = FALSE
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
  
  expected_beta = EXPECTED_BETA,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 17. MIS SEARCH + EXACT FE REFITS
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 18. Add standardized output aliases
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
    "Cai & Szeidl: Log Sales — Competitor Treatment Spillover"
  )
)


# ------------------------------------------------------------------------------
# 22. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 8 MIS audit complete")
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
  "Reported Table 3 N: ",
  EXPECTED_N
)


message(
  "Original fixest nobs: ",
  N_fixest
)


message(
  "Original beta: ",
  format(
    beta_original,
    digits = 16
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
  "Baseline absolute difference: ",
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