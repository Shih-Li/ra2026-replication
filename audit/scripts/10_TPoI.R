# ==============================================================================
# audit/scripts/10_TPoI.R
#
# Paper:
#   Westberg, Skjeflo, and Kallbekken (2025)
#   "The Power of Information: A Survey Experiment on Public Support for
#    Electricity Price Compensation Schemes"
#
# MIS estimand audited here:
#   Table A5 — Linear-probability robustness model for price-subsidy support
#
# Outcome:
#   support_priceB
#     1 = partially / strongly in favor of the price subsidy
#     0 = neutral / partially opposed / strongly opposed
#
# Target:
#   T1 = Distribution-information treatment
#
# Original linear specification:
#   lm(support_priceB ~ T1 + T2 + T3)
#
# Inference in the paper/replication:
#   HC1 heteroskedasticity-robust standard errors
#
# IMPORTANT NONLINEAR-PRIMARY-MODEL NOTE
# ------------------------------------------------------------------------------
# The headline Table 2 estimator is MASS::polr(), an ordered-logit model.
# The shared MIS engine in audit/function/ is explicitly an OLS/lm deletion
# engine (dinkelbach_topk_lm). Therefore this audit uses the paper's reported
# Table A5 linear-probability robustness specification, which is the linear
# analogue of the same randomized treatment contrast.
#
# T1 is chosen because it isolates the distribution-information treatment,
# which is the paper's central information result. T2 is incentives-only and
# T3 combines the distribution and incentives information.
#
# Independent/display benchmark from the validated R-TableA5.html:
#   beta(T1) = -0.117  (displayed to 3 decimals)
#   HC1 SE    =  0.029  (displayed to 3 decimals)
#   N         = 1819
#
# The saved HTML table does not retain a full-precision coefficient. The script
# therefore first requires the reconstructed estimator to match the saved
# benchmark at its displayed precision and N; only after that check does it
# freeze the exact lm coefficient as EXPECTED_BETA for the MIS engine.
#
# MIS protocol:
#   k = 1, ..., floor(0.05 * N)
#   both directions
#   exact OLS refit after every selected deletion set
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
      "Could not locate audit project.\n",
      "Open audit/audit.Rproj and then run:\n",
      'source("scripts/10_TPoI.R")'
    ),
    call. = FALSE
  )
}


AUDIT_ROOT <- find_audit_root()
REPO_ROOT <- dirname(AUDIT_ROOT)

PAPER_ROOT <- file.path(
  REPO_ROOT,
  "10_TPoI"
)

DATA_FILE <- file.path(
  PAPER_ROOT,
  "data",
  "source",
  "cleaned",
  "surveydata.rds"
)

BENCHMARK_FILE <- file.path(
  PAPER_ROOT,
  "results",
  "tables",
  "R-TableA5.html"
)

OUTPUT_DIR <- file.path(
  AUDIT_ROOT,
  "output",
  "10_TPoI"
)

if (!dir.exists(PAPER_ROOT)) {
  stop(
    "Paper folder does not exist: ",
    PAPER_ROOT,
    call. = FALSE
  )
}

for (f in c(DATA_FILE, BENCHMARK_FILE)) {
  if (!file.exists(f)) {
    stop(
      "Required Paper 10 file does not exist:\n",
      f,
      call. = FALSE
    )
  }
}


# ------------------------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "sandwich",
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
      paste(missing_packages, collapse = ", ")
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

STUDY_ID <- "10_TPoI"

ESTIMAND_ID <-
  "tableA5_price_support_distribution_information"

OUTCOME <- "support_priceB"
TARGET <- "T1"
RHS <- c("T1", "T2", "T3")

EXPECTED_N <- 1819L
EXPECTED_BETA_DISPLAY <- -0.117
EXPECTED_SE_DISPLAY <- 0.029
DISPLAY_DIGITS <- 3L

BETA_TOLERANCE <- 1e-8
MAX_REMOVAL_FRACTION <- 0.05


# ------------------------------------------------------------------------------
# 5. Load author-prepared analysis data and reconstruct treatment/sample
# ------------------------------------------------------------------------------

full_data <- readRDS(
  DATA_FILE
)

full_data <- as.data.frame(
  full_data
)

if ("source_row_id" %in% names(full_data)) {
  stop(
    "`source_row_id` already exists in surveydata.rds.",
    call. = FALSE
  )
}

# Stable audit identifier: row in the frozen author-provided surveydata.rds.
full_data$source_row_id <- seq_len(
  nrow(full_data)
)

required_source_vars <- c(
  "gruppe",
  "Qattention",
  "Qattention2",
  "Q13_1",
  "source_row_id"
)

missing_source_vars <- setdiff(
  required_source_vars,
  names(full_data)
)

if (length(missing_source_vars) > 0L) {
  stop(
    paste0(
      "Missing required Paper 10 source variable(s): ",
      paste(missing_source_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Match code/02_data_prep_quality.R exactly.
full_data$C <- ifelse(
  full_data$gruppe == 4,
  1,
  0
)

full_data$T1 <- ifelse(
  full_data$gruppe == 1,
  1,
  0
)

full_data$T2 <- ifelse(
  full_data$gruppe == 2,
  1,
  0
)

full_data$T3 <- ifelse(
  full_data$gruppe == 3,
  1,
  0
)

full_data$att <- ifelse(
  full_data$Qattention == 2 &
    full_data$Qattention2 == 4,
  1,
  0
)

# Match `data <- subset(full_data, att == 1)` while handling NA explicitly.
data <- full_data[
  !is.na(full_data$att) &
    full_data$att == 1,
  ,
  drop = FALSE
]

row.names(data) <- NULL


# ------------------------------------------------------------------------------
# 6. Reconstruct Table A5 binary price-support outcome
# ------------------------------------------------------------------------------

# Match code/04_primary_hypotheses.R exactly:
#   Q13_1 in {4, 5} -> 1
#   Q13_1 in {1, 2, 3} -> 0
#   all other responses -> NA
#
data[[OUTCOME]] <- ifelse(
  data$Q13_1 %in% c(4, 5),
  1,
  ifelse(
    data$Q13_1 %in% c(1, 2, 3),
    0,
    NA_real_
  )
)

required_model_vars <- c(
  OUTCOME,
  RHS,
  "source_row_id"
)

missing_model_vars <- setdiff(
  required_model_vars,
  names(data)
)

if (length(missing_model_vars) > 0L) {
  stop(
    paste0(
      "Missing required Table A5 variable(s): ",
      paste(missing_model_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Basic treatment-coding checks.
treatment_matrix <- as.matrix(
  data[, RHS, drop = FALSE]
)

if (anyNA(treatment_matrix)) {
  stop(
    "Treatment indicators contain missing values after reconstruction.",
    call. = FALSE
  )
}

if (any(!treatment_matrix %in% c(0, 1))) {
  stop(
    "Treatment indicators are not binary 0/1 variables.",
    call. = FALSE
  )
}

if (any(rowSums(treatment_matrix) > 1)) {
  stop(
    "Treatment indicators are not mutually exclusive.",
    call. = FALSE
  )
}

if (!any(rowSums(treatment_matrix) == 0)) {
  stop(
    "No no-information control observations remain in the attention-check sample.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 7. ORIGINAL PAPER 10 Table A5 estimator
# ------------------------------------------------------------------------------

original_formula <- stats::as.formula(
  paste0(
    OUTCOME,
    " ~ ",
    paste(RHS, collapse = " + ")
  )
)

original_fit <- stats::lm(
  formula = original_formula,
  data = data,
  na.action = stats::na.omit
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
      "` is absent from the Table A5 model."
    ),
    call. = FALSE
  )
}

beta_original <- unname(
  original_coef[[TARGET]]
)

if (!is.finite(beta_original)) {
  stop(
    "Original Table A5 target coefficient is not finite.",
    call. = FALSE
  )
}

hc1_vcov <- sandwich::vcovHC(
  original_fit,
  type = "HC1"
)

hc1_se <- sqrt(
  diag(hc1_vcov)
)

if (!TARGET %in% names(hc1_se)) {
  stop(
    paste0(
      "Target HC1 standard error `",
      TARGET,
      "` is absent."
    ),
    call. = FALSE
  )
}

se_original <- unname(
  hc1_se[[TARGET]]
)


# ------------------------------------------------------------------------------
# 8. Validate against the directly validated Table A5 artifact
# ------------------------------------------------------------------------------

# The replication summary records R-TableA5.html as byte-for-byte identical to
# the original saved HTML table. The artifact reports the T1 coefficient and
# HC1 SE to three decimals and N exactly.
if (N_original != EXPECTED_N) {
  stop(
    paste0(
      "\n",
      "Folder:\n",
      "    audit/scripts/\n\n",
      "File:\n",
      "    10_TPoI.R\n\n",
      "Problem:\n",
      "    Paper 10 Table A5 does not reproduce benchmark N.\n\n",
      "Expected:\n",
      "    N = ", EXPECTED_N, "\n\n",
      "Obtained:\n",
      "    N = ", N_original
    ),
    call. = FALSE
  )
}

beta_display <- sprintf(
  paste0("%.", DISPLAY_DIGITS, "f"),
  beta_original
)

expected_beta_display <- sprintf(
  paste0("%.", DISPLAY_DIGITS, "f"),
  EXPECTED_BETA_DISPLAY
)

if (!identical(beta_display, expected_beta_display)) {
  stop(
    paste0(
      "\n",
      "Paper 10 Table A5 target coefficient does not reproduce ",
      "the validated displayed benchmark.\n\n",
      "Expected displayed beta:\n",
      "    ", expected_beta_display, "\n\n",
      "Obtained full-precision beta:\n",
      "    ", format(beta_original, digits = 16), "\n",
      "Obtained displayed beta:\n",
      "    ", beta_display
    ),
    call. = FALSE
  )
}

se_display <- sprintf(
  paste0("%.", DISPLAY_DIGITS, "f"),
  se_original
)

expected_se_display <- sprintf(
  paste0("%.", DISPLAY_DIGITS, "f"),
  EXPECTED_SE_DISPLAY
)

if (!identical(se_display, expected_se_display)) {
  stop(
    paste0(
      "\n",
      "Paper 10 Table A5 HC1 standard error does not reproduce ",
      "the validated displayed benchmark.\n\n",
      "Expected displayed HC1 SE:\n",
      "    ", expected_se_display, "\n\n",
      "Obtained full-precision HC1 SE:\n",
      "    ", format(se_original, digits = 16), "\n",
      "Obtained displayed HC1 SE:\n",
      "    ", se_display
    ),
    call. = FALSE
  )
}

message(
  "Original Paper 10 Table A5 replication check PASSED."
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
  "HC1 SE = ",
  format(se_original, digits = 16)
)


# ------------------------------------------------------------------------------
# 9. Freeze exact Table A5 estimation sample
# ------------------------------------------------------------------------------

model_vars <- c(
  OUTCOME,
  RHS
)

sample_keep <- stats::complete.cases(
  data[, model_vars, drop = FALSE]
)

analysis_sample <- data[
  sample_keep,
  ,
  drop = FALSE
]

row.names(analysis_sample) <- NULL

if (nrow(analysis_sample) != EXPECTED_N) {
  stop(
    paste0(
      "Frozen Table A5 sample does not equal benchmark N.\n",
      "Expected N = ", EXPECTED_N, "\n",
      "Frozen N = ", nrow(analysis_sample)
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
    "`source_row_id` is not unique in the Paper 10 audit sample.",
    call. = FALSE
  )
}

message(
  "Frozen Paper 10 audit sample contains ",
  nrow(analysis_sample),
  " observations."
)


# ------------------------------------------------------------------------------
# 10. MIS-compatible model
# ------------------------------------------------------------------------------

# Table A5 is already OLS. The HC1 VCE changes inference only, not the slope,
# so the MIS-compatible model is exactly the paper's coefficient estimator.
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
      "` is absent from MIS-compatible lm."
    ),
    call. = FALSE
  )
}

beta_mis <- unname(
  mis_coef[[TARGET]]
)


# ------------------------------------------------------------------------------
# 11. Baseline MIS validation
# ------------------------------------------------------------------------------

if (N_mis != N_original) {
  stop(
    paste0(
      "MIS model does not use the original Table A5 sample.\n",
      "Original N = ", N_original, "\n",
      "MIS N = ", N_mis
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
      "MIS-compatible lm does not reproduce the original Table A5 slope.\n",
      "Original beta = ", format(beta_original, digits = 16), "\n",
      "MIS beta = ", format(beta_mis, digits = 16), "\n",
      "Absolute difference = ",
      format(abs(beta_mis - beta_original), scientific = TRUE)
    ),
    call. = FALSE
  )
}

X_mis <- stats::model.matrix(
  mis_fit
)

rank_mis <- qr(X_mis)$rank

if (rank_mis < ncol(X_mis)) {
  stop(
    paste0(
      "MIS model matrix is rank deficient.\n",
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
  "beta_mis = ",
  format(beta_mis, digits = 16)
)

message(
  "original/MIS absolute beta difference = ",
  format(
    abs(beta_mis - beta_original),
    scientific = TRUE
  )
)


# ------------------------------------------------------------------------------
# 12. Exact Paper 10 estimator refit after deletion
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

# Baseline exact-refit sanity check.
baseline_refit <- paper_refit(
  analysis_sample,
  NULL
)

beta_refit_baseline <- unname(
  stats::coef(baseline_refit)[[TARGET]]
)

if (
  !is.finite(beta_refit_baseline) ||
  abs(beta_refit_baseline - beta_original) > BETA_TOLERANCE
) {
  stop(
    paste0(
      "Exact Paper 10 refit does not reproduce baseline beta.\n",
      "Original = ", format(beta_original, digits = 16), "\n",
      "Refit = ", format(beta_refit_baseline, digits = 16)
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 13. Freeze exact benchmark after independent displayed-result validation
# ------------------------------------------------------------------------------

# No full-precision coefficient is stored in the validated Table A5 HTML.
# After the N / displayed-beta / displayed-HC1-SE checks above pass, freeze the
# exact coefficient returned by the verified paper estimator for the audit.
EXPECTED_BETA <- beta_original


# ------------------------------------------------------------------------------
# 14. Define MIS audit specification
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
  
  # Explicitly enforce the audit protocol's 5% deletion path.
  max_fraction = MAX_REMOVAL_FRACTION
)


# ------------------------------------------------------------------------------
# 15. MIS SEARCH + EXACT OLS REFITS
# ------------------------------------------------------------------------------

audit_result <- run_mis_audit(
  spec = spec,
  verbose = TRUE
)


# ------------------------------------------------------------------------------
# 16. Add standardized output aliases
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
    "Westberg et al.: Price-Subsidy Support — Distribution Information"
  )
)


# ------------------------------------------------------------------------------
# 20. Final console report
# ------------------------------------------------------------------------------

message("")
message("============================================================")
message("Paper 10 MIS audit complete")
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
  TARGET,
  " (Distribution information)"
)

message(
  "Table A5 N: ",
  EXPECTED_N
)

message(
  "Original beta: ",
  format(beta_original, digits = 16)
)

message(
  "Original HC1 SE: ",
  format(se_original, digits = 16)
)

message(
  "Validated displayed beta / SE: ",
  beta_display,
  " / ",
  se_display
)

message(
  "MIS-compatible beta: ",
  format(beta_mis, digits = 16)
)

message(
  "Baseline absolute difference: ",
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