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
# 19. Create output directory
# ------------------------------------------------------------------------------

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# Study-specific diagnostic: FE structure after MIS deletion
# ============================================================================== 

parse_mis_ids_numeric <- function(x) {
  
  # In the in-memory audit object, mis_ids is already
  # a vector containing one ID per selected observation.
  if (length(x) > 1L) {
    return(
      as.numeric(x)
    )
  }
  
  
  # Also support the semicolon-delimited representation
  # used in the saved CSV output.
  x <- as.character(x)
  
  
  if (
    length(x) == 0L ||
    is.na(x) ||
    !nzchar(x)
  ) {
    return(
      numeric(0)
    )
  }
  
  
  as.numeric(
    trimws(
      strsplit(
        x,
        ";",
        fixed = TRUE
      )[[1]]
    )
  )
}

cross_k <- as.integer(
  audit_result$summary$min_k_cross_zero[[1]]
)


cross_direction <- as.character(
  audit_result$summary$direction_cross_zero[[1]]
)


cross_row <- audit_result$path[
  audit_result$path$k == cross_k &
    as.character(
      audit_result$path$direction
    ) == cross_direction,
  ,
  drop = FALSE
]


if (nrow(cross_row) != 1L) {
  stop(
    "Could not uniquely identify Paper 8 first zero-crossing row.",
    call. = FALSE
  )
}


selected_ids <- parse_mis_ids_numeric(
  cross_row$mis_ids[[1]]
)


if (
  length(selected_ids) != cross_k ||
  anyDuplicated(selected_ids)
) {
  stop(
    "Paper 8 zero-crossing MIS IDs do not match k.",
    call. = FALSE
  )
}

selected_flag <- (
  analysis_sample$source_row_id %in%
    selected_ids
)


selected_data <- analysis_sample[
  selected_flag,
  ,
  drop = FALSE
]


remaining_data <- analysis_sample[
  !selected_flag,
  ,
  drop = FALSE
]


firm_before <- table(
  as.character(
    analysis_sample[[FE_VAR]]
  )
)


firm_after <- table(
  as.character(
    remaining_data[[FE_VAR]]
  )
)


selected_by_firm <- table(
  as.character(
    selected_data[[FE_VAR]]
  )
)


baseline_singleton_firms <- names(
  firm_before[
    firm_before == 1L
  ]
)


selected_from_baseline_singletons <- sum(
  as.character(
    selected_data[[FE_VAR]]
  ) %in%
    baseline_singleton_firms
)


after_names <- names(
  firm_after
)


before_for_remaining <- firm_before[
  after_names
]


new_singleton_firms <- after_names[
  firm_after == 1L &
    before_for_remaining > 1L
]


selected_firm_names <- names(
  selected_by_firm
)


n_before_selected_firms <- as.integer(
  firm_before[
    selected_firm_names
  ]
)


n_after_selected_firms <- vapply(
  selected_firm_names,
  function(f) {
    
    if (f %in% names(firm_after)) {
      as.integer(
        firm_after[[f]]
      )
    } else {
      0L
    }
  },
  integer(1)
)


selected_firm_diagnostic <- data.frame(
  firmid =
    selected_firm_names,
  
  n_before =
    n_before_selected_firms,
  
  n_selected =
    as.integer(
      selected_by_firm
    ),
  
  n_after =
    n_after_selected_firms,
  
  became_singleton =
    n_after_selected_firms == 1L &
    n_before_selected_firms > 1L,
  
  fully_deleted =
    n_after_selected_firms == 0L,
  
  stringsAsFactors = FALSE
)


utils::write.csv(
  selected_firm_diagnostic,
  file.path(
    OUTPUT_DIR,
    "audit_zero_crossing_firm_diagnostics.csv"
  ),
  row.names = FALSE,
  na = ""
)

cross_refit <- paper_env$fit_ols(
  data = remaining_data,
  
  outcome = OUTCOME,
  
  terms = RHS,
  
  cluster = CLUSTER_VAR,
  
  fe = FE_VAR
)


cross_beta_check <- unname(
  stats::coef(
    cross_refit
  )[[TARGET]]
)


cross_fixest_n <- as.integer(
  stats::nobs(
    cross_refit
  )
)


cross_reported_n <- attr(
  cross_refit,
  "ra_report_n"
)


baseline_auto_removed <- (
  nrow(
    analysis_sample
  ) -
    N_fixest
)


post_delete_auto_removed <- (
  nrow(
    remaining_data
  ) -
    cross_fixest_n
)


fe_crossing_summary <- data.frame(
  k =
    cross_k,
  
  direction =
    cross_direction,
  
  selected_observations =
    length(
      selected_ids
    ),
  
  selected_firms =
    length(
      unique(
        selected_data[[FE_VAR]]
      )
    ),
  
  firms_with_multiple_selected_observations =
    sum(
      selected_by_firm > 1L
    ),
  
  maximum_selected_from_one_firm =
    max(
      selected_by_firm
    ),
  
  selected_baseline_singletons =
    selected_from_baseline_singletons,
  
  newly_created_singleton_firms =
    length(
      new_singleton_firms
    ),
  
  baseline_complete_n =
    nrow(
      analysis_sample
    ),
  
  baseline_fixest_n =
    N_fixest,
  
  remaining_complete_n =
    nrow(
      remaining_data
    ),
  
  refit_fixest_n =
    cross_fixest_n,
  
  refit_reported_n =
    cross_reported_n,
  
  baseline_auto_removed =
    baseline_auto_removed,
  
  post_delete_auto_removed =
    post_delete_auto_removed,
  
  additional_auto_removed_after_selected_deletion =
    post_delete_auto_removed -
    baseline_auto_removed,
  
  effective_fixest_sample_change =
    N_fixest -
    cross_fixest_n,
  
  beta_before =
    beta_original,
  
  beta_after_audit =
    cross_row$beta_after[[1]],
  
  beta_after_check =
    cross_beta_check,
  
  beta_check_difference =
    cross_beta_check -
    cross_row$beta_after[[1]],
  
  stringsAsFactors = FALSE
)


utils::write.csv(
  fe_crossing_summary,
  file.path(
    OUTPUT_DIR,
    "audit_zero_crossing_fe_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)

display_path <- audit_result$path[
  as.character(
    audit_result$path$direction
  ) == cross_direction &
    audit_result$path$k <= 40,
  ,
  drop = FALSE
]


fe_path_diagnostic <- lapply(
  seq_len(
    nrow(
      display_path
    )
  ),
  function(j) {
    
    ids <- parse_mis_ids_numeric(
      display_path$mis_ids[[j]]
    )
    
    
    selected <- analysis_sample[
      analysis_sample$source_row_id %in% ids,
      ,
      drop = FALSE
    ]
    
    
    remaining <- analysis_sample[
      !analysis_sample$source_row_id %in% ids,
      ,
      drop = FALSE
    ]
    
    
    after_counts <- table(
      as.character(
        remaining[[FE_VAR]]
      )
    )
    
    
    before_match <- firm_before[
      names(
        after_counts
      )
    ]
    
    
    selected_counts <- table(
      as.character(
        selected[[FE_VAR]]
      )
    )
    
    
    newly_singleton <- sum(
      after_counts == 1L &
        before_match > 1L
    )
    
    
    data.frame(
      k =
        display_path$k[[j]],
      
      direction =
        as.character(
          display_path$direction[[j]]
        ),
      
      selected_observations =
        length(
          ids
        ),
      
      selected_firms =
        length(
          selected_counts
        ),
      
      firms_with_multiple_selected =
        sum(
          selected_counts > 1L
        ),
      
      maximum_selected_from_one_firm =
        max(
          selected_counts
        ),
      
      newly_created_singleton_firms =
        newly_singleton,
      
      stringsAsFactors = FALSE
    )
  }
)


fe_path_diagnostic <- do.call(
  rbind,
  fe_path_diagnostic
)


utils::write.csv(
  fe_path_diagnostic,
  file.path(
    OUTPUT_DIR,
    "audit_display_path_fe_diagnostics.csv"
  ),
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------------------------
# 20. Save standardized outputs
# ------------------------------------------------------------------------------

saved_files <- save_mis_audit(
  audit = audit_result,
  output_dir = OUTPUT_DIR,
  prefix = "audit"
)


# ------------------------------------------------------------------------------
# 21. Ensure influential-ID output contains model_row_position
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
# 22. Standard MIS figure
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
# 23. Final console report
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