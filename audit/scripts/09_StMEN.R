# ==============================================================================
# audit/scripts/09_StMEN.R
#
# Paper:
#   "Selecting the Most Effective Nudge"
#
# MIS estimand:
#   Post-LASSO / pooled policy model for shots_per_dollar
#   Target:
#     POOLED_gossip2XSMS2ORgossip3XSMS2ORgossip2XSMS3ORgossip3XSMS3
#
# Original estimator:
#   estimatr::lm_robust()
#   weighted OLS with village_population weights
#   CR0 standard errors clustered by id_sc
#
# IMPORTANT MODEL-SELECTION NOTE
# ------------------------------------------------------------------------------
# The published workflow first performs backward selection and automated policy
# pooling on the full analysis sample, then estimates the post-LASSO WLS model.
#
# This MIS audit reconstructs that workflow ONCE on the validated full sample,
# freezes the resulting post-LASSO formula, and audits the target coefficient in
# that fixed published specification. Re-running variable selection/pooling after
# each deletion would change the estimand and is not the fixed linear coefficient
# problem solved by dinkelbach_topk_lm().
#
# IMPORTANT WEIGHTING NOTE
# ------------------------------------------------------------------------------
# The shared Dinkelbach MIS routine is an OLS routine. The paper's target model
# is fixed-weight WLS, so this script uses the exact algebraic transformation
#
#       y* = sqrt(w) y
#       X* = sqrt(w) X
#
# and fits y* = X* beta with no ordinary lm intercept, because the transformed
# intercept sqrt(w) * 1 is already an explicit column of X*.
#
# Deleting a transformed row is therefore exactly equivalent to deleting the
# corresponding original WLS observation, provided village_population remains
# fixed. Every selected deletion set is then re-estimated with the ORIGINAL
# estimatr::lm_robust weighted estimator.
#
# Baseline validation requires:
#   1. the reconstructed post-LASSO WLS target equals the stored replication
#      benchmark within 1e-8;
#   2. transformed OLS N equals original weighted-estimator N;
#   3. transformed OLS target equals original weighted target within 1e-8;
#   4. the transformed MIS design is full rank;
#   5. the exact baseline refit reproduces the original weighted target.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both directions
#   exact weighted refit after every selected deletion set
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. General options
# ------------------------------------------------------------------------------

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ------------------------------------------------------------------------------
# 1. Locate audit project and Paper 9 files
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
    
    candidate <- dirname(dirname(script_file))
    
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
      'source("scripts/09_StMEN.R")'
    ),
    call. = FALSE
  )
}


AUDIT_ROOT <- find_audit_root()
REPO_ROOT <- dirname(AUDIT_ROOT)

# The replication repository uses `9_StMEN`, not `09_StMEN`.
PAPER_ROOT <- file.path(
  REPO_ROOT,
  "9_StMEN"
)

PAPER_FUNCTIONS_DIR <- file.path(
  PAPER_ROOT,
  "code",
  "functions"
)

DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "processed",
  "Tablet_VillageXMonth_Costs.csv"
)

CREATE_SP_FILE <- file.path(
  PAPER_FUNCTIONS_DIR,
  "create_sp_variables.R"
)

POOLING_FILE <- file.path(
  PAPER_FUNCTIONS_DIR,
  "pooling_functions.R"
)

OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "09_StMEN"
)

for (f in c(
  DATA_FILE,
  CREATE_SP_FILE,
  POOLING_FILE
)) {
  if (!file.exists(f)) {
    stop(
      "Required StMEN replication file does not exist:\n",
      f,
      call. = FALSE
    )
  }
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "data.table",
  "dplyr",
  "stringr",
  "estimatr",
  "lme4",
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

# The paper helper scripts use `%>%` and stringr functions without namespaces.
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})


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

STUDY_ID <- "09_StMEN"

ESTIMAND_ID <- paste0(
  "shots_per_dollar_postlasso_",
  "POOLED_gossip2XSMS2ORgossip3XSMS2ORgossip2XSMS3ORgossip3XSMS3"
)

OUTCOME <- "shots_per_dollar"

TARGET_ORIGINAL <- paste0(
  "POOLED_gossip2XSMS2ORgossip3XSMS2OR",
  "gossip2XSMS3ORgossip3XSMS3"
)

WEIGHT_VAR <- "village_population"
CLUSTER_VAR <- "id_sc"

P_VALUE_CUTOFF <- 5e-13
BETA_TOLERANCE <- 1e-8
MAX_REMOVAL_FRACTION <- 0.05

# Persisted deterministic sample-0 post-LASSO coefficient in:
# 9_StMEN/data/intermediate/Bootstrap_simulations_data_shots_per_dollar.csv
#
# This is the raw WLS coefficient, not the nonlinear winner-adjusted ATE from
# shots_per_dollar_best_policy_WC_adjusted.txt.
EXPECTED_BETA <- 0.00401595794827882


# ------------------------------------------------------------------------------
# 5. Load processed analysis data + permanent observation IDs
# ------------------------------------------------------------------------------

villagexmonth_all <- data.table::fread(
  DATA_FILE,
  header = TRUE,
  sep = ",",
  data.table = FALSE
)

if ("source_row_id" %in% names(villagexmonth_all)) {
  stop(
    "`source_row_id` already exists in Tablet_VillageXMonth_Costs.csv.",
    call. = FALSE
  )
}

# Stable ID is assigned before any sample restriction.
villagexmonth_all$source_row_id <- seq_len(
  nrow(villagexmonth_all)
)

required_filter_vars <- c(
  "seedsrisk",
  "first_implementation"
)

missing_filter_vars <- setdiff(
  required_filter_vars,
  names(villagexmonth_all)
)

if (length(missing_filter_vars) > 0L) {
  stop(
    paste0(
      "Missing StMEN sample-selection variable(s): ",
      paste(missing_filter_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 6. Reproduce the paper's analysis-sample restrictions
# ------------------------------------------------------------------------------

# Paper 02_main_analysis.R:
#   seedsrisk == 1
#   first_implementation == 1
villagexmonth_level <- villagexmonth_all %>%
  dplyr::filter(seedsrisk == 1) %>%
  dplyr::filter(first_implementation == 1)

villagexmonth_level <- as.data.frame(
  villagexmonth_level
)

if (nrow(villagexmonth_level) == 0L) {
  stop(
    "The StMEN sample restrictions leave zero observations.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. Reproduce paper-specific policy variables and district-time FE dummies
# ------------------------------------------------------------------------------

# This source file is intentionally executed in the current environment because
# the paper helper modifies `villagexmonth_level` directly.
source(
  CREATE_SP_FILE,
  local = TRUE
)

# Exact district-time FE construction from 02_main_analysis.R.
villagexmonth_level <- villagexmonth_level %>%
  dplyr::group_by(
    id_district,
    created_year,
    created_month
  ) %>%
  dplyr::mutate(
    fes = dplyr::cur_group_id()
  ) %>%
  dplyr::ungroup()

fes_dummies <- data.frame(
  lme4::dummy(
    villagexmonth_level$fes
  )
)

if (ncol(fes_dummies) == 0L) {
  stop(
    "District-time FE dummy construction produced zero columns.",
    call. = FALSE
  )
}

villagexmonth_level <- cbind(
  villagexmonth_level,
  fes_dummies
)


# ------------------------------------------------------------------------------
# 8. Reproduce the paper's backward-selection step
# ------------------------------------------------------------------------------

variables <- grep(
  "^SP",
  names(villagexmonth_level),
  value = TRUE
)

variables <- variables[
  variables != "SP_noSeedXnoIncentiveXnoReminder"
]

if (length(variables) == 0L) {
  stop(
    "No StMEN SP policy variables were created.",
    call. = FALSE
  )
}

variables_expanded <- c(
  variables,
  colnames(fes_dummies)
)

current_variables <- variables_expanded

deselect_list <- character(0)
deselect_pval <- numeric(0)

make_selection_formula <- function(rhs) {
  if (length(rhs) == 0L) {
    stop(
      "Backward selection removed every regressor.",
      call. = FALSE
    )
  }
  
  stats::as.formula(
    paste0(
      OUTCOME,
      "~",
      paste0(rhs, collapse = "+")
    )
  )
}

current_sp_formula <- make_selection_formula(
  current_variables
)

current_model_ols <- estimatr::lm_robust(
  formula = current_sp_formula,
  data = villagexmonth_level,
  weights = village_population,
  se_type = "classical"
)

current_max_pval <- max(
  current_model_ols$p.value
)

if (!is.finite(current_max_pval)) {
  stop(
    "Initial StMEN backward-selection p-values are not finite.",
    call. = FALSE
  )
}

selection_iteration <- 0L
selection_iteration_limit <- length(current_variables) + 5L

while (current_max_pval > P_VALUE_CUTOFF) {
  selection_iteration <- selection_iteration + 1L
  
  if (selection_iteration > selection_iteration_limit) {
    stop(
      "Backward-selection iteration guard triggered.",
      call. = FALSE
    )
  }
  
  deselect_idx <- which.max(
    current_model_ols$p.value
  )
  
  deselect_name <- names(
    current_model_ols$p.value[deselect_idx]
  )
  
  deselect_value <- unname(
    current_model_ols$p.value[deselect_idx]
  )
  
  if (
    length(deselect_name) != 1L ||
    !nzchar(deselect_name) ||
    !is.finite(deselect_value)
  ) {
    stop(
      "Could not identify a finite backward-selection p-value.",
      call. = FALSE
    )
  }
  
  if (!deselect_name %in% current_variables) {
    stop(
      paste0(
        "Backward selection attempted to drop `",
        deselect_name,
        "`, which is not in current_variables. ",
        "This would make the paper loop non-progressing."
      ),
      call. = FALSE
    )
  }
  
  deselect_list <- c(
    deselect_list,
    deselect_name
  )
  
  deselect_pval <- c(
    deselect_pval,
    deselect_value
  )
  
  current_variables <- current_variables[
    current_variables != deselect_name
  ]
  
  current_sp_formula <- make_selection_formula(
    current_variables
  )
  
  current_model_ols <- estimatr::lm_robust(
    formula = current_sp_formula,
    data = villagexmonth_level,
    weights = village_population,
    se_type = "classical"
  )
  
  current_max_pval <- max(
    current_model_ols$p.value
  )
  
  if (!is.finite(current_max_pval)) {
    stop(
      paste0(
        "Backward-selection p-values became non-finite after removing `",
        deselect_name,
        "`."
      ),
      call. = FALSE
    )
  }
}

support_SP <- variables_expanded[
  !(variables_expanded %in% deselect_list)
]

# Mirror the paper's safeguard against selecting an intercept term.
to_remove <- grep(
  pattern = "ntercept",
  x = support_SP
)

if (length(to_remove) > 0L) {
  support_SP <- support_SP[-to_remove]
}

fes_chosen <- grep(
  "^X",
  support_SP,
  value = TRUE
)

support_SP_policies <- grep(
  "^X",
  support_SP,
  value = TRUE,
  invert = TRUE
)

if (length(support_SP_policies) == 0L) {
  stop(
    "Backward selection retained no StMEN policy variables.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 9. Reproduce automated policy pooling
# ------------------------------------------------------------------------------

source(
  POOLING_FILE,
  local = TRUE
)

if (!exists("get_relevant_sp_in_tp", mode = "function")) {
  stop(
    "pooling_functions.R did not define get_relevant_sp_in_tp().",
    call. = FALSE
  )
}

if (!exists("add_pooled_policies_tp", mode = "function")) {
  stop(
    "pooling_functions.R did not define add_pooled_policies_tp().",
    call. = FALSE
  )
}

treatment_profiles <- list(
  c("random", "noIncentive", "noReminder"),
  c("trusted", "noIncentive", "noReminder"),
  c("gossip", "noIncentive", "noReminder"),
  
  c("noSeed", "slope", "noReminder"),
  c("noSeed", "flat", "noReminder"),
  
  c("noSeed", "noIncentive", "SMS"),
  
  c("random", "slope", "noReminder"),
  c("trusted", "slope", "noReminder"),
  c("gossip", "slope", "noReminder"),
  
  c("random", "flat", "noReminder"),
  c("trusted", "flat", "noReminder"),
  c("gossip", "flat", "noReminder"),
  
  c("random", "noIncentive", "SMS"),
  c("trusted", "noIncentive", "SMS"),
  c("gossip", "noIncentive", "SMS"),
  
  c("noSeed", "slope", "SMS"),
  c("noSeed", "flat", "SMS"),
  
  c("random", "slope", "SMS"),
  c("trusted", "slope", "SMS"),
  c("gossip", "slope", "SMS"),
  
  c("random", "flat", "SMS"),
  c("trusted", "flat", "SMS"),
  c("gossip", "flat", "SMS")
)

final_data <- villagexmonth_level

for (tp_name in treatment_profiles) {
  tp_support <- get_relevant_sp_in_tp(
    support_SP,
    tp_name
  )
  
  final_data <- add_pooled_policies_tp(
    final_data,
    tp_support,
    tp_name
  )
}

pooled_policies <- grep(
  "^POOLED_",
  colnames(final_data),
  value = TRUE
)

if (length(pooled_policies) == 0L) {
  stop(
    "Automated pooling produced no POOLED_ policy columns.",
    call. = FALSE
  )
}

if (!TARGET_ORIGINAL %in% pooled_policies) {
  stop(
    paste0(
      "Published target policy is absent after pooling:\n",
      TARGET_ORIGINAL
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 10. Freeze the exact published post-LASSO specification and sample
# ------------------------------------------------------------------------------

variables_pooled_expanded <- c(
  pooled_policies,
  fes_chosen
)

formula_pl <- stats::as.formula(
  paste0(
    OUTCOME,
    "~",
    paste0(
      variables_pooled_expanded,
      collapse = "+"
    )
  )
)

required_model_vars <- unique(
  c(
    all.vars(formula_pl),
    WEIGHT_VAR,
    CLUSTER_VAR,
    "source_row_id"
  )
)

missing_model_vars <- setdiff(
  required_model_vars,
  names(final_data)
)

if (length(missing_model_vars) > 0L) {
  stop(
    paste0(
      "Missing final StMEN model variable(s): ",
      paste(missing_model_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

sample_keep <- stats::complete.cases(
  final_data[
    ,
    required_model_vars,
    drop = FALSE
  ]
)

weights_numeric <- as.numeric(
  final_data[[WEIGHT_VAR]]
)

sample_keep <- (
  sample_keep &
    is.finite(weights_numeric) &
    weights_numeric > 0
)

analysis_sample <- final_data[
  sample_keep,
  ,
  drop = FALSE
]

row.names(analysis_sample) <- NULL

if (nrow(analysis_sample) == 0L) {
  stop(
    "The frozen post-LASSO WLS estimation sample is empty.",
    call. = FALSE
  )
}

if (anyNA(analysis_sample$source_row_id)) {
  stop(
    "`source_row_id` contains missing values in the estimation sample.",
    call. = FALSE
  )
}

if (anyDuplicated(analysis_sample$source_row_id)) {
  stop(
    "`source_row_id` is not unique in the estimation sample.",
    call. = FALSE
  )
}

message(
  "Frozen StMEN post-LASSO estimation sample contains ",
  nrow(analysis_sample),
  " observations."
)


# ------------------------------------------------------------------------------
# 11. ORIGINAL REPLICATION — post-LASSO weighted OLS
# ------------------------------------------------------------------------------

original_fit <- estimatr::lm_robust(
  formula = formula_pl,
  data = analysis_sample,
  clusters = id_sc,
  weights = village_population,
  se_type = "CR0"
)

N_original <- as.integer(
  stats::nobs(
    original_fit
  )
)

original_coef <- stats::coef(
  original_fit
)

if (!TARGET_ORIGINAL %in% names(original_coef)) {
  stop(
    paste0(
      "Target coefficient `",
      TARGET_ORIGINAL,
      "` is absent from original_fit."
    ),
    call. = FALSE
  )
}

if (any(!is.finite(original_coef))) {
  bad_coef <- names(original_coef)[
    !is.finite(original_coef)
  ]
  
  stop(
    paste0(
      "Original post-LASSO WLS has non-finite coefficient(s): ",
      paste(bad_coef, collapse = ", ")
    ),
    call. = FALSE
  )
}

beta_original <- unname(
  original_coef[[TARGET_ORIGINAL]]
)

if (N_original != nrow(analysis_sample)) {
  stop(
    paste0(
      "Original lm_robust changed the frozen sample.\n",
      "Frozen rows = ",
      nrow(analysis_sample),
      "\n",
      "lm_robust N = ",
      N_original
    ),
    call. = FALSE
  )
}

# The repository does not persist total nobs(model_pl) in the deterministic
# target-output file. The authoritative expected N is therefore the exact N from
# the reconstructed original estimator, which the MIS representation and all
# baseline refits must reproduce exactly.
EXPECTED_N <- N_original


# ------------------------------------------------------------------------------
# 12. Validate ORIGINAL REPLICATION against stored coefficient benchmark
# ------------------------------------------------------------------------------

if (
  !is.finite(beta_original) ||
  abs(beta_original - EXPECTED_BETA) > BETA_TOLERANCE
) {
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    09_StMEN.R\n\n",
      "Problem:\n",
      "    Reconstructed StMEN post-LASSO weighted coefficient does not ",
      "match the stored sample-0 replication benchmark.\n\n",
      "Expected:\n",
      "    N = ",
      EXPECTED_N,
      "\n",
      "    beta = ",
      format(EXPECTED_BETA, digits = 16),
      "\n\n",
      "Obtained:\n",
      "    N = ",
      N_original,
      "\n",
      "    beta = ",
      format(beta_original, digits = 16),
      "\n\n",
      "Likely cause:\n",
      "    sample restriction, FE dummy construction, backward-selection ",
      "path, pooling, or formula mismatch."
    ),
    call. = FALSE
  )
}

# Confirm that the stored target is also the best pooled policy in this model.
policy_effects <- original_coef[
  pooled_policies
]

best_policy_name <- names(
  policy_effects
)[
  which.max(policy_effects)
]

if (!identical(best_policy_name, TARGET_ORIGINAL)) {
  stop(
    paste0(
      "Reconstructed best pooled policy differs from the stored target.\n",
      "Expected best policy: ",
      TARGET_ORIGINAL,
      "\n",
      "Obtained best policy: ",
      best_policy_name
    ),
    call. = FALSE
  )
}

message(
  "Original StMEN post-LASSO weighted replication check PASSED."
)

message(
  "N_original = ",
  N_original
)

message(
  "beta_original = ",
  format(beta_original, digits = 16)
)


# ------------------------------------------------------------------------------
# 13. Construct exact OLS-equivalent representation of fixed-weight WLS
# ------------------------------------------------------------------------------

X_full <- stats::model.matrix(
  formula_pl,
  data = analysis_sample
)

retained_coef_names <- names(
  original_coef
)

missing_design_columns <- setdiff(
  retained_coef_names,
  colnames(X_full)
)

if (length(missing_design_columns) > 0L) {
  stop(
    paste0(
      "Could not map original lm_robust coefficient(s) to model.matrix(): ",
      paste(missing_design_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

X <- X_full[
  ,
  retained_coef_names,
  drop = FALSE
]

if (!TARGET_ORIGINAL %in% colnames(X)) {
  stop(
    "Target column is absent from the retained WLS design matrix.",
    call. = FALSE
  )
}

sqrt_w <- sqrt(
  as.numeric(
    analysis_sample[[WEIGHT_VAR]]
  )
)

y_original <- as.numeric(
  analysis_sample[[OUTCOME]]
)

y_star <- sqrt_w * y_original
X_star <- X * sqrt_w

# Give transformed columns simple, unique, syntactic names.
mis_column_names <- paste0(
  "mis_",
  make.names(
    colnames(X),
    unique = TRUE
  )
)

colnames(X_star) <- mis_column_names

target_position_original <- match(
  TARGET_ORIGINAL,
  colnames(X)
)

TARGET_MIS <- mis_column_names[
  target_position_original
]

analysis_sample$mis_y <- y_star

for (j in seq_along(mis_column_names)) {
  analysis_sample[[mis_column_names[[j]]]] <- X_star[, j]
}

# No ordinary lm intercept: sqrt(w) * 1 is already in X_star.
mis_formula <- stats::reformulate(
  termlabels = mis_column_names,
  response = "mis_y",
  intercept = FALSE
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

if (!TARGET_MIS %in% names(mis_coef)) {
  stop(
    "Transformed WLS target is absent from the MIS lm.",
    call. = FALSE
  )
}

beta_mis <- unname(
  mis_coef[[TARGET_MIS]]
)


# ------------------------------------------------------------------------------
# 14. Baseline WLS -> transformed OLS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    09_StMEN.R\n\n",
      "Problem:\n",
      "    Transformed OLS changes the validated estimation sample.\n\n",
      "Expected:\n",
      "    N = ",
      N_original,
      "\n",
      "    beta = ",
      format(beta_original, digits = 16),
      "\n\n",
      "Obtained:\n",
      "    N = ",
      N_mis,
      "\n",
      "    beta = ",
      format(beta_mis, digits = 16),
      "\n\n",
      "Likely cause:\n",
      "    transformed design/sample mismatch."
    ),
    call. = FALSE
  )
}

if (
  !is.finite(beta_mis) ||
  abs(beta_mis - beta_original) > BETA_TOLERANCE
) {
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    09_StMEN.R\n\n",
      "Problem:\n",
      "    Fixed-weight WLS -> transformed OLS coefficient validation FAILED.\n\n",
      "Expected:\n",
      "    N = ",
      N_original,
      "\n",
      "    beta = ",
      format(beta_original, digits = 16),
      "\n\n",
      "Obtained:\n",
      "    N = ",
      N_mis,
      "\n",
      "    beta = ",
      format(beta_mis, digits = 16),
      "\n\n",
      "Likely cause:\n",
      "    weight transformation, intercept handling, or retained-design ",
      "column mismatch.\n\n",
      "Do not run MIS until this is resolved."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 15. Full-rank check
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
      "Transformed MIS design is rank deficient.\n",
      "Rank = ",
      rank_mis,
      "\n",
      "Columns = ",
      ncol(X_mis),
      "\n",
      "Do not run MIS."
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
  format(beta_mis, digits = 16)
)

message(
  "weighted/MIS absolute beta difference = ",
  format(
    abs(beta_mis - beta_original),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 16. Exact ORIGINAL weighted refit after deletion
# ------------------------------------------------------------------------------

paper_refit <- function(
    data,
    spec
) {
  estimatr::lm_robust(
    formula = formula_pl,
    data = data,
    clusters = id_sc,
    weights = village_population,
    se_type = "CR0"
  )
}

# Baseline exact-refit sanity check.
refit_baseline <- paper_refit(
  analysis_sample,
  NULL
)

if (stats::nobs(refit_baseline) != N_original) {
  stop(
    "Exact weighted baseline refit does not reproduce original N.",
    call. = FALSE
  )
}

refit_coef <- stats::coef(
  refit_baseline
)

if (!TARGET_ORIGINAL %in% names(refit_coef)) {
  stop(
    "Exact weighted baseline refit does not contain the target coefficient.",
    call. = FALSE
  )
}

beta_refit_baseline <- unname(
  refit_coef[[TARGET_ORIGINAL]]
)

if (
  !is.finite(beta_refit_baseline) ||
  abs(beta_refit_baseline - beta_original) > BETA_TOLERANCE
) {
  stop(
    "Exact weighted baseline refit does not reproduce original beta.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 17. Define MIS audit specification
# ------------------------------------------------------------------------------

spec <- audit_spec(
  study_id = STUDY_ID,
  
  estimand_id = ESTIMAND_ID,
  
  data = analysis_sample,
  
  mis_formula = mis_formula,
  
  # Target coefficient in transformed OLS model.
  target = TARGET_MIS,
  
  id_var = "source_row_id",
  
  # Exact original weighted estimator.
  refit_fn = paper_refit,
  
  # Target coefficient in original weighted estimator.
  refit_target = TARGET_ORIGINAL,
  
  expected_beta = EXPECTED_BETA,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 18. MIS SEARCH + EXACT WEIGHTED REFITS
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 19. Restore substantive target label + attach paper-specific specification
# ------------------------------------------------------------------------------

# Internally the MIS target has a transformed column name.
# Externally report the paper's substantive pooled-policy coefficient name.
audit_result$path$target <- TARGET_ORIGINAL

audit_result$baseline$target <- TARGET_ORIGINAL

audit_result$summary$target <- TARGET_ORIGINAL

# Standard instruction-compatible aliases used by the existing audit scripts.
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

# Preserve the paper-specific reconstruction choices in audit.rds.
audit_result$specification <- list(
  paper_root = PAPER_ROOT,
  data_file = DATA_FILE,
  source_analysis = file.path(PAPER_ROOT, "code", "02_main_analysis.R"),
  outcome = OUTCOME,
  target = TARGET_ORIGINAL,
  estimator = "estimatr::lm_robust fixed-weight WLS",
  weights = WEIGHT_VAR,
  cluster = CLUSTER_VAR,
  se_type = "CR0",
  p_value_cutoff = P_VALUE_CUTOFF,
  sample_restriction = "seedsrisk == 1 & first_implementation == 1",
  selected_policy_variables = support_SP_policies,
  selected_fe_dummies = fes_chosen,
  pooled_policies = pooled_policies,
  frozen_formula = paste(
    deparse(formula_pl),
    collapse = " "
  ),
  expected_beta = EXPECTED_BETA,
  expected_n = EXPECTED_N,
  beta_tolerance = BETA_TOLERANCE,
  max_removal_fraction = MAX_REMOVAL_FRACTION,
  weighting_adapter = "sqrt(w) y and sqrt(w) X exact WLS-to-OLS transform"
)


# ------------------------------------------------------------------------------
# 20. Save standardized outputs
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
    "StMEN: Shots per dollar — best pooled policy"
  )
)


# ------------------------------------------------------------------------------
# 23. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 9 StMEN MIS audit complete")
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
  TARGET_ORIGINAL
)

message(
  "Estimator: estimatr::lm_robust weighted OLS"
)

message(
  "Weight: ",
  WEIGHT_VAR
)

message(
  "Cluster: ",
  CLUSTER_VAR
)

message(
  "N: ",
  EXPECTED_N
)

message(
  "Original weighted beta: ",
  format(beta_original, digits = 16)
)

message(
  "Stored beta benchmark: ",
  format(EXPECTED_BETA, digits = 16)
)

message(
  "Transformed OLS beta: ",
  format(beta_mis, digits = 16)
)

message(
  "Absolute baseline difference: ",
  format(
    abs(beta_mis - beta_original),
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