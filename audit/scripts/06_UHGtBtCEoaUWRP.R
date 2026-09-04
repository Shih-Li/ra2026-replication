# ==============================================================================
# audit/scripts/06_UHGtBtCEoaUWRP.R
#
# Paper:
#   "Using Household Grants to Benchmark the Cost Effectiveness
#    of a USAID Workforce Readiness Program"
#
# MIS estimand:
#   Main ITT
#   Outcome: bn_employed
#   Target:  treat_HD
#
# Original estimator:
#
#   Weighted OLS:
#
#     bn_employed ~
#       treatment arms +
#       Lbn_employed +
#       selected controls +
#       randomization-block indicators
#
#   weights = attr_wgt
#   cluster = hhid
#   sample  = round == 1
#
# IMPORTANT WEIGHTING NOTE
# ------------------------------------------------------------------------------
# The shared MIS routine is an OLS routine.
#
# The paper's estimator is fixed-weight WLS. We therefore use the exact
# algebraic transformation:
#
#       y* = sqrt(w) y
#       X* = sqrt(w) X
#
# and fit:
#
#       y* = X* beta
#
# with no ordinary intercept, because sqrt(w) * 1 is explicitly included
# as a transformed intercept column.
#
# Deleting one transformed row is exactly equivalent to deleting the
# corresponding original WLS observation, provided the attrition weights
# themselves remain fixed.
#
# Every selected deletion set is then re-estimated using the ORIGINAL
# weighted fixest estimator.
#
# Baseline validation requires:
#
#   1. current R replication matches its stored benchmark;
#   2. transformed OLS N equals weighted estimator N;
#   3. transformed OLS target equals weighted target within 1e-8.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both directions
#   exact weighted refit after each deletion set
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
      "Could not locate the audit project.\n",
      "Open audit/audit.Rproj and run:\n",
      'source("scripts/06_UHGtBtCEoaUWRP.R")'
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
  "06_UHGtBtCEoaUWRP"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "HD_panel_clean.dta"
)


CONTROLS_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "intermediate",
  "HD_controls.csv"
)


BENCHMARK_FILE <- file.path(
  PAPER_ROOT,
  "results",
  "tables",
  "R-itt_fullspec_primary.csv"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "06_UHGtBtCEoaUWRP"
)


for (f in c(
  DATA_FILE,
  CONTROLS_FILE,
  BENCHMARK_FILE
)) {
  
  if (!file.exists(f)) {
    
    stop(
      "Required Paper 6 file does not exist:\n",
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
  "readr",
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

STUDY_ID <- "06_UHGtBtCEoaUWRP"


ESTIMAND_ID <- "itt_fullspec_bn_employed_treat_HD"


OUTCOME <- "bn_employed"


TARGET_ORIGINAL <- "treat_HD"


WEIGHT_VAR <- "attr_wgt"


CLUSTER_VAR <- "hhid"


BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 5. Load source analysis data
# ------------------------------------------------------------------------------

panel_all <- haven::read_dta(
  DATA_FILE
)


panel_all <- as.data.frame(
  panel_all
)


# Stable ID created BEFORE any sample restriction.
if ("source_row_id" %in% names(panel_all)) {
  
  stop(
    "`source_row_id` already exists in HD_panel_clean.dta.",
    call. = FALSE
  )
}


panel_all$source_row_id <- seq_len(
  nrow(panel_all)
)


# ------------------------------------------------------------------------------
# 6. Read selected controls
# ------------------------------------------------------------------------------

controls_table <- readr::read_csv(
  CONTROLS_FILE,
  show_col_types = FALSE
)


if (!"controls" %in% names(controls_table)) {
  
  stop(
    "HD_controls.csv must contain a column named `controls`.",
    call. = FALSE
  )
}


selected_controls <- unique(
  stats::na.omit(
    as.character(
      controls_table$controls
    )
  )
)


# ------------------------------------------------------------------------------
# 7. Reconstruct the main ITT specification
# ------------------------------------------------------------------------------

if (!"round" %in% names(panel_all)) {
  
  stop(
    "Variable `round` is absent from HD_panel_clean.dta.",
    call. = FALSE
  )
}


# The original ITT uses round == 1.
d <- panel_all[
  !is.na(panel_all$round) &
    panel_all$round == 1,
  ,
  drop = FALSE
]


# Treatment variables used in the main ITT.
treatment_candidates <- c(
  "treat_HD",
  "treat_GD_lower",
  "treat_GD_middle",
  "treat_GD_upper",
  "treat_GD_huge",
  "treat_combined"
)


treatment_vars <- treatment_candidates[
  treatment_candidates %in% names(d)
]


if (!TARGET_ORIGINAL %in% treatment_vars) {
  
  stop(
    "Target treatment `treat_HD` is absent from the Paper 6 data.",
    call. = FALSE
  )
}


# Original randomization-block indicators.
block_vars <- grep(
  "^_Iblock",
  names(d),
  value = TRUE
)


# Outcome-specific lag.
lag_outcome <- paste0(
  "L",
  OUTCOME
)


# Mimic nonempty_vars() from the Paper 6 replication:
# retain only existing variables with at least one nonmissing observation.
nonempty_vars_local <- function(data, vars) {
  
  vars <- unique(
    as.character(vars)
  )
  
  
  vars <- vars[
    vars %in% names(data)
  ]
  
  
  vars[
    vapply(
      vars,
      function(v) {
        any(
          !is.na(
            data[[v]]
          )
        )
      },
      logical(1)
    )
  ]
}


rhs_terms <- nonempty_vars_local(
  d,
  c(
    treatment_vars,
    lag_outcome,
    selected_controls,
    block_vars
  )
)


# ------------------------------------------------------------------------------
# 8. Safe formula construction
# ------------------------------------------------------------------------------

quote_formula_name <- function(x) {
  
  vapply(
    as.character(x),
    function(z) {
      
      if (
        identical(
          make.names(z),
          z
        )
      ) {
        
        z
        
      } else {
        
        paste0(
          "`",
          z,
          "`"
        )
      }
    },
    character(1)
  )
}


original_formula <- stats::as.formula(
  paste(
    quote_formula_name(
      OUTCOME
    ),
    "~",
    paste(
      quote_formula_name(
        rhs_terms
      ),
      collapse = " + "
    )
  )
)


# ------------------------------------------------------------------------------
# 9. Freeze exact weighted estimation sample
# ------------------------------------------------------------------------------

required_model_vars <- unique(
  c(
    OUTCOME,
    rhs_terms,
    WEIGHT_VAR,
    CLUSTER_VAR
  )
)


missing_vars <- setdiff(
  required_model_vars,
  names(d)
)


if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required Paper 6 variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


sample_keep <- stats::complete.cases(
  d[
    ,
    required_model_vars,
    drop = FALSE
  ]
)


sample_keep <- (
  sample_keep &
    is.finite(
      as.numeric(
        d[[WEIGHT_VAR]]
      )
    ) &
    as.numeric(
      d[[WEIGHT_VAR]]
    ) > 0
)


analysis_sample <- d[
  sample_keep,
  ,
  drop = FALSE
]


row.names(
  analysis_sample
) <- NULL


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


message(
  "Frozen Paper 6 estimation sample contains ",
  nrow(analysis_sample),
  " observations."
)


# ------------------------------------------------------------------------------
# 10. Read independent current-replication benchmark
# ------------------------------------------------------------------------------

benchmark <- readr::read_csv(
  BENCHMARK_FILE,
  show_col_types = FALSE
)


benchmark_row <- benchmark[
  benchmark$outcome == OUTCOME &
    benchmark$term == TARGET_ORIGINAL,
  ,
  drop = FALSE
]


if (nrow(benchmark_row) != 1L) {
  
  stop(
    paste0(
      "Expected exactly one benchmark row for ",
      OUTCOME,
      " / ",
      TARGET_ORIGINAL,
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
    "Could not recover Paper 6 benchmark beta/N.",
    call. = FALSE
  )
}


message(
  "Expected beta = ",
  format(
    EXPECTED_BETA,
    digits = 16
  )
)


message(
  "Expected N = ",
  EXPECTED_N
)


# ------------------------------------------------------------------------------
# 11. ORIGINAL REPLICATION — weighted OLS
# ------------------------------------------------------------------------------

original_fit <- fixest::feols(
  fml = original_formula,
  data = analysis_sample,
  weights = ~attr_wgt,
  vcov = ~hhid,
  notes = FALSE,
  warn = FALSE
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


beta_original <- unname(
  original_coef[[TARGET_ORIGINAL]]
)


# ------------------------------------------------------------------------------
# 12. Validate ORIGINAL REPLICATION
# ------------------------------------------------------------------------------

if (N_original != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    06_UHGtBtCEoaUWRP.R\n\n",
      
      "Problem:\n",
      "    Original weighted ITT does not reproduce benchmark N.\n\n",
      
      "Expected N:\n",
      "    ",
      EXPECTED_N,
      "\n\n",
      
      "Obtained N:\n",
      "    ",
      N_original
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
      "Original Paper 6 weighted ITT does not reproduce ",
      "R-itt_fullspec_primary.csv.\n\n",
      
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
  "Original Paper 6 weighted replication check PASSED."
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


# ------------------------------------------------------------------------------
# 13. Construct exact OLS-equivalent representation of WLS
# ------------------------------------------------------------------------------

# Full unweighted design matrix corresponding to the original specification.
X_full <- stats::model.matrix(
  original_formula,
  data = analysis_sample
)


# fixest may remove a collinear nuisance regressor.
#
# Keep exactly the coefficient columns retained by the original estimator.
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
      "Could not map original fixest coefficient(s) to model.matrix(): ",
      paste(
        missing_design_columns,
        collapse = ", "
      )
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
    "Target column is absent from retained WLS design.",
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


y_star <- sqrt_w *
  y_original


X_star <- X *
  sqrt_w


# Give transformed columns simple syntactic names.
mis_column_names <- paste0(
  "mis_",
  make.names(
    colnames(X),
    unique = TRUE
  )
)


colnames(
  X_star
) <- mis_column_names


target_position_original <- match(
  TARGET_ORIGINAL,
  colnames(X)
)


TARGET_MIS <- mis_column_names[
  target_position_original
]


# Add transformed response/design to the SAME data frame.
#
# Original columns remain available for exact weighted refits.
analysis_sample$mis_y <- y_star


for (j in seq_along(mis_column_names)) {
  
  analysis_sample[[mis_column_names[[j]]]] <- X_star[, j]
}


# No standard lm intercept:
# transformed sqrt(w) intercept is already one of the X_star columns.
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
    "Transformed WLS target is absent from MIS lm.",
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
      "Transformed OLS changes the sample.\n",
      "Weighted N = ",
      N_original,
      "\n",
      "Transformed N = ",
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
      "Weighted-OLS transformation validation FAILED.\n\n",
      
      "Original weighted beta:\n",
      "    ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "Transformed OLS beta:\n",
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
  "weighted/MIS absolute beta difference = ",
  format(
    abs(
      beta_mis -
        beta_original
    ),
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
  
  fixest::feols(
    fml = original_formula,
    data = data,
    weights = ~attr_wgt,
    vcov = ~hhid,
    notes = FALSE,
    warn = FALSE
  )
}


# Baseline refit sanity check.
refit_baseline <- paper_refit(
  analysis_sample,
  NULL
)


beta_refit_baseline <- unname(
  stats::coef(
    refit_baseline
  )[[TARGET_ORIGINAL]]
)


if (
  !is.finite(beta_refit_baseline) ||
  abs(
    beta_refit_baseline -
    beta_original
  ) > BETA_TOLERANCE
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
# 19. Restore substantive target label + standardized aliases
# ------------------------------------------------------------------------------

# Internally the MIS target has a transformed column name.
# Externally report the paper's substantive coefficient name.
audit_result$path$target <- TARGET_ORIGINAL


audit_result$baseline$target <- TARGET_ORIGINAL


audit_result$summary$target <- TARGET_ORIGINAL


# Standard instruction-compatible aliases.
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
    "Huguka Dukore: Employment — HD Treatment"
  )
)


# ------------------------------------------------------------------------------
# 23. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 6 MIS audit complete")
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
  "Estimator: weighted OLS, attr_wgt"
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
  format(
    beta_original,
    digits = 16
  )
)


message(
  "Transformed OLS beta: ",
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