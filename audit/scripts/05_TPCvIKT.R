# ==============================================================================
# audit/scripts/05_TPCvIKT.R
#
# Paper:
#   Cunha (2014)
#   "Testing Paternalism: Cash versus In-Kind Transfers"
#
# MIS estimand audited here:
#
#   Aggregate FOOD consumption
#   Validated NO-VILLAGE-CONTROLS specification
#
# Target causal contrast:
#
#   ATT(In-kind) - ATT_EQ(Cash)
#
# where ATT_EQ(Cash) scales the implemented cash treatment to the monetary
# value of the in-kind basket.
#
# IMPORTANT SCOPE NOTE
# ------------------------------------------------------------------------------
# The current R replication documents remaining discrepancies in the fully
# controlled aggregate-consumption specification.
#
# In contrast, cons_agg_no_ctrls reproduces the original aggregate consumption
# results and is therefore used here as the validated Paper 5 specification.
#
# This script must NOT be described as an audit of the unresolved controlled
# specification.
#
# Original no-controls regression:
#
#   pc_exp_food ~
#     ik +
#     cash +
#     fu +
#     ik_fu +
#     cash_fu
#
# clustered at:
#
#   id_loc
#
# Original contrast:
#
#   beta_ik_fu - s * beta_cash_fu
#
# where:
#
#   s = mean baseline cash-group PAL basket value / 150
#
# Reparameterized MIS regression:
#
#   pc_exp_food ~
#     ik +
#     cash +
#     fu +
#     ik_minus_eq_cash +
#     eq_cash_basis
#
# with:
#
#   ik_minus_eq_cash = ik_fu
#   eq_cash_basis     = s * ik_fu + cash_fu
#
# Therefore:
#
#   coef(ik_minus_eq_cash)
#     = beta_ik_fu - s * beta_cash_fu
#     = ATT(In-kind) - ATT_EQ(Cash)
#
# The transformation changes only the parameterization, not the model span,
# fitted values, residuals, or estimation sample.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both directions
#   exact clustered OLS refit after every selected deletion set
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
      "Could not locate the audit project.\n",
      "Open audit/audit.Rproj and run:\n",
      'source("scripts/05_TPCvIKT.R")'
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
  "05_TPCvIKT"
)


DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "processed",
  "hh_analysis.rds"
)


BENCHMARK_FILE <- file.path(
  PAPER_ROOT,
  "output",
  "cons_agg_no_ctrls.xlsx"
)


OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "05_TPCvIKT"
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
      "Required Paper 5 analysis-ready dataset does not exist:\n",
      DATA_FILE,
      "\n\n",
      "Run the Paper 5 replication first."
    ),
    call. = FALSE
  )
}


if (!file.exists(BENCHMARK_FILE)) {
  
  stop(
    paste0(
      "Required Paper 5 replication benchmark does not exist:\n",
      BENCHMARK_FILE,
      "\n\n",
      "Run the Paper 5 analysis first so cons_agg_no_ctrls.xlsx ",
      "is regenerated."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "fixest",
  "readxl",
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

STUDY_ID <- "05_TPCvIKT"


ESTIMAND_ID <-
  "food_no_controls_inkind_minus_equal_value_cash"


TARGET <- "ik_minus_eq_cash"


OUTCOME <- "pc_exp_food"


# Published food-consumption regression sample.
EXPECTED_N <- 10985L


BETA_TOLERANCE <- 1e-8


MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 5. Load analysis-ready household-wave data
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
# 6. Required variables
# ------------------------------------------------------------------------------

required_vars <- c(
  "cve_viv",
  "etapa",
  "id_loc",
  "special_etapa1",
  "pr_ik",
  "ik",
  "cash",
  "fu",
  "ik_fu",
  "cash_fu",
  OUTCOME
)


missing_vars <- setdiff(
  required_vars,
  names(analysis_all)
)


if (length(missing_vars) > 0L) {
  
  stop(
    paste0(
      "Missing required Paper 5 variable(s): ",
      paste(
        missing_vars,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. Permanent observation ID
# ------------------------------------------------------------------------------

# Regression observations are household-wave observations.
#
# Create the permanent identifier BEFORE imposing the estimation sample.
analysis_all$observation_id <- paste0(
  as.character(
    analysis_all$cve_viv
  ),
  "__wave_",
  as.character(
    analysis_all$etapa
  )
)


# ------------------------------------------------------------------------------
# 8. Original Paper 5 sample restriction
# ------------------------------------------------------------------------------

# Original replication:
#
#   good <- is.na(special_etapa1) | special_etapa1 != 1
#
# This deliberately retains missing special_etapa1 because Stata treats
# missing != 1 as true in the original workflow.
good <- (
  is.na(
    analysis_all$special_etapa1
  ) |
    analysis_all$special_etapa1 != 1
)


# ------------------------------------------------------------------------------
# 9. Reconstruct equal-valued cash scaling
# ------------------------------------------------------------------------------

safe_mean <- function(x) {
  
  if (
    !any(
      is.finite(x),
      na.rm = TRUE
    )
  ) {
    
    return(
      NA_real_
    )
  }
  
  
  mean(
    x,
    na.rm = TRUE
  )
}


baseline_cash <- (
  analysis_all$etapa == 1 &
    analysis_all$cash == 1 &
    good
)


PR_IK_CASH <- safe_mean(
  analysis_all$pr_ik[
    baseline_cash
  ]
)


if (
  !is.finite(PR_IK_CASH) ||
  PR_IK_CASH <= 0
) {
  
  stop(
    "Could not reconstruct the baseline cash-group PAL basket value.",
    call. = FALSE
  )
}


# The implemented cash transfer was 150 pesos.
CASH_SCALE <- PR_IK_CASH / 150


message(
  "Baseline cash-group PAL basket value = ",
  format(
    PR_IK_CASH,
    digits = 16
  )
)


message(
  "Equal-value cash scale = ",
  format(
    CASH_SCALE,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 10. Read independent replication benchmark
# ------------------------------------------------------------------------------

benchmark <- readxl::read_excel(
  BENCHMARK_FILE,
  sheet = "tests"
)


required_benchmark_cols <- c(
  "outcome",
  "att_ik_minus_eq_cash"
)


missing_benchmark_cols <- setdiff(
  required_benchmark_cols,
  names(benchmark)
)


if (length(missing_benchmark_cols) > 0L) {
  
  stop(
    paste0(
      "Benchmark file is missing required column(s): ",
      paste(
        missing_benchmark_cols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


benchmark_row <- benchmark[
  benchmark$outcome == "exp_food",
  ,
  drop = FALSE
]


if (nrow(benchmark_row) != 1L) {
  
  stop(
    paste0(
      "Expected exactly one `exp_food` row in ",
      "cons_agg_no_ctrls.xlsx / tests, but found ",
      nrow(benchmark_row),
      "."
    ),
    call. = FALSE
  )
}


EXPECTED_BETA <- as.numeric(
  benchmark_row$att_ik_minus_eq_cash[[1]]
)


if (!is.finite(EXPECTED_BETA)) {
  
  stop(
    "Could not recover the expected Paper 5 treatment contrast.",
    call. = FALSE
  )
}


message(
  "Expected replication contrast = ",
  format(
    EXPECTED_BETA,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 11. Reconstruct exact regression sample
# ------------------------------------------------------------------------------

# fit_clustered() uses the outcome, all RHS variables and id_loc when
# determining the original e(sample).
model_vars <- c(
  OUTCOME,
  "ik",
  "cash",
  "fu",
  "ik_fu",
  "cash_fu",
  "id_loc"
)


is_valid_model_value <- function(v) {
  
  if (inherits(v, "haven_labelled")) {
    
    v <- haven::zap_labels(v)
  }
  
  
  if (
    is.numeric(v) ||
    is.integer(v)
  ) {
    
    return(
      !is.na(v) &
        is.finite(
          as.numeric(v)
        )
    )
  }
  
  
  if (is.logical(v)) {
    
    return(
      !is.na(v)
    )
  }
  
  
  if (is.character(v)) {
    
    n_bytes <- nchar(
      v,
      type = "bytes",
      allowNA = TRUE,
      keepNA = TRUE
    )
    
    
    return(
      !is.na(v) &
        !is.na(n_bytes) &
        n_bytes > 0L
    )
  }
  
  
  if (is.factor(v)) {
    
    return(
      !is.na(v)
    )
  }
  
  
  !is.na(v)
}


sample_keep <- good


for (v in model_vars) {
  
  sample_keep <- (
    sample_keep &
      is_valid_model_value(
        analysis_all[[v]]
      )
  )
}


analysis_sample <- analysis_all[
  which(
    sample_keep %in% TRUE
  ),
  ,
  drop = FALSE
]


row.names(analysis_sample) <- NULL


if (anyNA(analysis_sample$observation_id)) {
  
  stop(
    "Permanent observation IDs contain missing values.",
    call. = FALSE
  )
}


if (anyDuplicated(analysis_sample$observation_id)) {
  
  stop(
    paste0(
      "`cve_viv` + `etapa` is not unique in the estimation sample. ",
      "Inspect the household-wave structure before MIS."
    ),
    call. = FALSE
  )
}


message(
  "Frozen Paper 5 estimation sample contains ",
  nrow(analysis_sample),
  " household-wave observations."
)


# ------------------------------------------------------------------------------
# 12. ORIGINAL REPLICATION
# ------------------------------------------------------------------------------

original_formula <- stats::as.formula(
  pc_exp_food ~
    ik +
    cash +
    fu +
    ik_fu +
    cash_fu
)


original_fit <- fixest::feols(
  fml = original_formula,
  data = analysis_sample,
  cluster = ~id_loc,
  warn = FALSE,
  notes = FALSE
)


N_original <- as.integer(
  stats::nobs(
    original_fit
  )
)


original_coef <- stats::coef(
  original_fit
)


required_original_coef <- c(
  "ik_fu",
  "cash_fu"
)


missing_original_coef <- setdiff(
  required_original_coef,
  names(original_coef)
)


if (length(missing_original_coef) > 0L) {
  
  stop(
    paste0(
      "Original model is missing coefficient(s): ",
      paste(
        missing_original_coef,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


BETA_IK <- unname(
  original_coef[["ik_fu"]]
)


BETA_CASH <- unname(
  original_coef[["cash_fu"]]
)


# Main audit target:
#
# ATT(IK) - ATT_EQ(Cash)
beta_original <- (
  BETA_IK -
    CASH_SCALE *
    BETA_CASH
)


# ------------------------------------------------------------------------------
# 13. Validate ORIGINAL REPLICATION
# ------------------------------------------------------------------------------

if (N_original != EXPECTED_N) {
  
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    05_TPCvIKT.R\n\n",
      
      "Problem:\n",
      "    Paper 5 food-consumption model does not reproduce expected N.\n\n",
      
      "Expected:\n",
      "    N = ",
      EXPECTED_N,
      "\n\n",
      
      "Obtained:\n",
      "    N = ",
      N_original,
      "\n\n",
      
      "Likely cause:\n",
      "    household-wave sample restriction, outcome missingness, ",
      "special_etapa1 handling, or locality-ID filtering."
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
      "Folder:\n",
      "    audit/scripts/\n\n",
      
      "File:\n",
      "    05_TPCvIKT.R\n\n",
      
      "Problem:\n",
      "    Original regression does not reproduce ",
      "cons_agg_no_ctrls.xlsx.\n\n",
      
      "Expected target:\n",
      "    ",
      format(
        EXPECTED_BETA,
        digits = 16
      ),
      "\n\n",
      
      "Obtained target:\n",
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
      ),
      "\n\n",
      
      "Do not run MIS until this discrepancy is resolved."
    ),
    call. = FALSE
  )
}


message(
  "Original Paper 5 replication check PASSED."
)


message(
  "N_original = ",
  N_original
)


message(
  "beta_ik_fu = ",
  format(
    BETA_IK,
    digits = 16
  )
)


message(
  "beta_cash_fu = ",
  format(
    BETA_CASH,
    digits = 16
  )
)


message(
  "ATT(IK) - ATT_EQ(Cash) = ",
  format(
    beta_original,
    digits = 16
  )
)


# ------------------------------------------------------------------------------
# 14. Reparameterize the original OLS model for MIS
# ------------------------------------------------------------------------------

# Let:
#
#   x1 = ik_fu
#   x2 = cash_fu
#
# Original interaction contribution:
#
#   beta_1*x1 + beta_2*x2
#
# Define:
#
#   z1 = x1
#   z2 = CASH_SCALE*x1 + x2
#
# Then:
#
#   beta_1*x1 + beta_2*x2
#
# = (beta_1 - CASH_SCALE*beta_2)*z1
#   + beta_2*z2
#
# Hence coefficient(z1) is exactly the desired equal-valued contrast.

analysis_sample$ik_minus_eq_cash <-
  analysis_sample$ik_fu


analysis_sample$eq_cash_basis <- (
  CASH_SCALE *
    analysis_sample$ik_fu +
    analysis_sample$cash_fu
)


mis_formula <- stats::as.formula(
  pc_exp_food ~
    ik +
    cash +
    fu +
    ik_minus_eq_cash +
    eq_cash_basis
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
      "` is absent from the MIS-compatible model."
    ),
    call. = FALSE
  )
}


beta_mis <- unname(
  mis_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 15. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  
  stop(
    paste0(
      "MIS-compatible model changes the estimation sample.\n",
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
      "MIS reparameterization validation FAILED.\n\n",
      
      "Original contrast:\n",
      "    ",
      format(
        beta_original,
        digits = 16
      ),
      "\n\n",
      
      "MIS coefficient:\n",
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
# 16. Full-rank check
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
# 17. Exact original-estimator refit after deletion
# ------------------------------------------------------------------------------

# This is an exact algebraic reparameterization of the original clustered OLS
# regression.
#
# The target coefficient is therefore the original paper contrast:
#
#   ATT(IK) - ATT_EQ(Cash)
#
# after the selected observations have been removed.

paper_refit <- function(
    data,
    spec
) {
  
  fixest::feols(
    fml = mis_formula,
    data = data,
    cluster = ~id_loc,
    warn = FALSE,
    notes = FALSE
  )
}


# Additional exact baseline refit check.
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
    "Exact-refit parameterization does not reproduce the original contrast.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 18. Define MIS audit specification
# ------------------------------------------------------------------------------

spec <- audit_spec(
  study_id = STUDY_ID,
  
  estimand_id = ESTIMAND_ID,
  
  data = analysis_sample,
  
  mis_formula = mis_formula,
  
  target = TARGET,
  
  id_var = "observation_id",
  
  refit_fn = paper_refit,
  
  refit_target = TARGET,
  
  expected_beta = EXPECTED_BETA,
  
  expected_n = EXPECTED_N,
  
  beta_tolerance = BETA_TOLERANCE,
  
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 19. MIS SEARCH + EXACT REFIT
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 20. Standardized output aliases
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
# 21. Save standardized outputs
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
# 22. Ensure influential-ID output contains model_row_position
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
# 23. Paper-5-specific MIS figure
# ------------------------------------------------------------------------------

plot_data <- audit_result$path[
  audit_result$path$valid_refit &
    is.finite(audit_result$path$k) &
    is.finite(audit_result$path$beta_after) &
    is.finite(audit_result$path$delta_beta),
  ,
  drop = FALSE
]


plot_data$direction <- factor(
  plot_data$direction,
  levels = c(
    "Decrease",
    "Increase"
  )
)


N_plot <- EXPECTED_N


max_k <- max(
  plot_data$k,
  na.rm = TRUE
)


beta0 <- audit_result$baseline$beta_original


zero_threshold <- -beta0


# First exact-refit zero crossing.
zero_rows <- plot_data[
  beta0 * plot_data$beta_after <= 0,
  ,
  drop = FALSE
]


if (nrow(zero_rows) > 0L) {
  
  zero_rows <- zero_rows[
    order(zero_rows$k),
    ,
    drop = FALSE
  ]
  
  
  k_zero <- zero_rows$k[1]
  
  
  pct_zero <- 100 *
    k_zero /
    N_plot
  
} else {
  
  k_zero <- NA_integer_
  
  
  pct_zero <- NA_real_
}


# Clean regular bottom-axis ticks.
k_breaks <- pretty(
  c(
    0,
    max_k
  ),
  n = 6
)


k_breaks <- k_breaks[
  k_breaks >= 0 &
    k_breaks <= max_k
]


# Clean regular percentage ticks.
pct_max <- 100 *
  max_k /
  N_plot


pct_breaks <- pretty(
  c(
    0,
    pct_max
  ),
  n = 5
)


pct_breaks <- pct_breaks[
  pct_breaks >= 0 &
    pct_breaks <= pct_max
]


# Put the special zero-crossing information in the caption instead of
# forcing k = 1 / 0.0091% onto the axes.
zero_caption <- if (
  is.finite(k_zero)
) {
  
  paste0(
    "First zero crossing: k = ",
    k_zero,
    " (",
    format(
      pct_zero,
      digits = 3,
      trim = TRUE
    ),
    "%).  ",
    "Zero-crossing threshold: Δβ = ",
    format(
      zero_threshold,
      digits = 4,
      trim = TRUE
    ),
    "."
  )
  
} else {
  
  "No zero crossing within the audited 5% removal range."
}


p05 <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = k,
    y = delta_beta,
    colour = direction,
    group = direction
  )
) +
  
  # Exact-refit MIS paths
  ggplot2::geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  
  # No change in coefficient.
  #
  # For Paper 5 we do NOT separately draw the Δβ = -beta0 line because
  # 0.886 is visually indistinguishable from zero on this very wide
  # vertical scale. Its exact value is reported in the caption instead.
  ggplot2::geom_hline(
    yintercept = 0,
    colour = "grey40",
    linewidth = 0.55
  ) +
  
  # Same colors as the shared MIS figure.
  ggplot2::scale_colour_manual(
    values = c(
      "Decrease" = "#ed7a72",
      "Increase" = "#76d6c6"
    ),
    drop = FALSE
  ) +
  
  # Bottom: removals
  # Top: removal fraction
  #
  # Use only regular ticks; do not force k_zero or pct_zero onto an axis.
  ggplot2::scale_x_continuous(
    name = "Removals (k)",
    
    limits = c(
      0,
      max_k
    ),
    
    breaks = k_breaks,
    
    sec.axis = ggplot2::sec_axis(
      transform = ~ . / N_plot * 100,
      
      name = "Removal fraction (%)",
      
      breaks = pct_breaks,
      
      labels = function(x) {
        format(
          x,
          trim = TRUE,
          scientific = FALSE
        )
      }
    ),
    
    expand = ggplot2::expansion(
      mult = c(
        0,
        0.01
      )
    )
  ) +
  
  ggplot2::scale_y_continuous(
    name = expression(
      Delta * beta[k]
    ),
    
    expand = ggplot2::expansion(
      mult = c(
        0.04,
        0.04
      )
    )
  ) +
  
  ggplot2::labs(
    colour = "Direction",
    caption = zero_caption
  ) +
  
  ggplot2::theme_classic(
    base_size = 12
  ) +
  
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    axis.title.x.top =
      ggplot2::element_text(
        margin = ggplot2::margin(
          b = 6
        )
      ),
    
    axis.text.x.top =
      ggplot2::element_text(
        margin = ggplot2::margin(
          b = 3
        )
      ),
    
    plot.caption =
      ggplot2::element_text(
        hjust = 0
      )
  )


# Preserve the x marks for non-nested MIS events.
non_nested <- plot_data[
  !is.na(plot_data$nested) &
    plot_data$nested == FALSE,
  ,
  drop = FALSE
]


if (nrow(non_nested) > 0L) {
  
  p05 <- p05 +
    
    ggplot2::geom_point(
      data = non_nested,
      
      ggplot2::aes(
        x = k,
        y = delta_beta,
        colour = direction
      ),
      
      shape = 4,
      size = 1.8,
      stroke = 0.7,
      
      inherit.aes = FALSE,
      show.legend = FALSE
    )
}


ggplot2::ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "fig_mis_audit.pdf"
  ),
  
  plot = p05,
  
  width = 7,
  height = 5.5
)


# ------------------------------------------------------------------------------
# 24. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 5 MIS audit complete")
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
  "Specification: validated no-village-controls OLS"
)


message(
  "Target: ATT(IK) - ATT_EQ(Cash)"
)


message(
  "N: ",
  EXPECTED_N
)


message(
  "Cash equal-value scaling factor: ",
  format(
    CASH_SCALE,
    digits = 16
  )
)


message(
  "Original target: ",
  format(
    beta_original,
    digits = 16
  )
)


message(
  "MIS-compatible target: ",
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