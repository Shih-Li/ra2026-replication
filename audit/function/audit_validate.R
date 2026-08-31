# ==============================================================================
# audit/function/audit_validate.R
#
# Purpose:
#   Define and validate a standardized empirical MIS audit specification.
#
# Main public functions:
#   audit_spec()
#   prepare_mis_audit()
#
# Philosophy:
#   - Input data should already be analysis-ready / processed.
#   - Do NOT remove outliers or high-leverage observations for MIS.
#   - Only make the data technically compatible with the MIS OLS engine.
#   - The MIS-compatible lm must reproduce the target coefficient from the
#     original replication estimator before the audit is allowed to proceed.
# ==============================================================================


# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.audit_stop <- function(...) {
  stop(..., call. = FALSE)
}


.audit_assert_string <- function(x, name) {
  
  ok <- (
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      nzchar(x)
  )
  
  if (!ok) {
    .audit_stop(
      "`", name, "` must be one non-empty character string."
    )
  }
  
  invisible(TRUE)
}


.audit_assert_number <- function(
    x,
    name,
    lower = -Inf,
    upper = Inf
) {
  
  ok <- (
    is.numeric(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      is.finite(x) &&
      x >= lower &&
      x <= upper
  )
  
  if (!ok) {
    .audit_stop(
      "`", name, "` must be one finite number in [",
      lower, ", ", upper, "]."
    )
  }
  
  invisible(TRUE)
}


.audit_default_refit <- function(data, spec) {
  
  stats::lm(
    formula = spec$mis_formula,
    data = data,
    na.action = stats::na.fail
  )
}


audit_extract_beta <- function(object, target) {
  
  # Allow a refit function to directly return one numeric coefficient.
  if (
    is.numeric(object) &&
    length(object) == 1L &&
    is.finite(object)
  ) {
    return(as.numeric(object))
  }
  
  # Allow a named numeric vector.
  if (is.numeric(object) && !is.null(names(object))) {
    
    if (target %in% names(object)) {
      
      beta <- object[[target]]
      
      if (length(beta) == 1L && is.finite(beta)) {
        return(as.numeric(beta))
      }
    }
  }
  
  # Standard model object.
  b <- tryCatch(
    stats::coef(object),
    error = function(e) NULL
  )
  
  if (is.null(b)) {
    .audit_stop(
      "Could not extract coefficients from the refitted model."
    )
  }
  
  if (!target %in% names(b)) {
    .audit_stop(
      "Target coefficient `",
      target,
      "` is absent from the refitted model."
    )
  }
  
  beta <- unname(b[[target]])
  
  if (
    length(beta) != 1L ||
    !is.finite(beta)
  ) {
    .audit_stop(
      "Target coefficient `",
      target,
      "` is not finite after refitting."
    )
  }
  
  as.numeric(beta)
}


audit_extract_nobs <- function(object) {
  
  n <- tryCatch(
    stats::nobs(object),
    error = function(e) NA_real_
  )
  
  if (
    length(n) != 1L ||
    !is.finite(n)
  ) {
    return(NA_integer_)
  }
  
  as.integer(n)
}


# ------------------------------------------------------------------------------
# Audit specification constructor
# ------------------------------------------------------------------------------

audit_spec <- function(
    study_id,
    estimand_id,
    data,
    mis_formula,
    target,
    id_var = NULL,
    refit_fn = NULL,
    refit_target = target,
    expected_beta = NULL,
    expected_n = NULL,
    beta_tolerance = 1e-8,
    k_grid = NULL,
    max_fraction = 0.10,
    run_greedy = TRUE
) {
  
  list(
    study_id = study_id,
    estimand_id = estimand_id,
    
    data = data,
    
    # OLS representation used by MIS.
    mis_formula = mis_formula,
    
    # Target coefficient in MIS model.
    target = target,
    
    # Stable observation identifier.
    id_var = id_var,
    
    # Function used for exact re-estimation after deletion.
    #
    # Required interface:
    #   refit_fn(data, spec)
    #
    # It may return:
    #   - lm
    #   - fixest model
    #   - another model supporting coef()
    #   - a numeric target coefficient
    refit_fn = refit_fn,
    
    # Name of target coefficient in the original estimator.
    refit_target = refit_target,
    
    # Replication checks.
    expected_beta = expected_beta,
    expected_n = expected_n,
    beta_tolerance = beta_tolerance,
    
    # Deletion path.
    k_grid = k_grid,
    max_fraction = max_fraction,
    
    run_greedy = run_greedy
  )
}


# ------------------------------------------------------------------------------
# Validate specification
# ------------------------------------------------------------------------------

audit_validate_spec <- function(spec) {
  
  if (!is.list(spec)) {
    .audit_stop("`spec` must be a list created by audit_spec().")
  }
  
  required <- c(
    "study_id",
    "estimand_id",
    "data",
    "mis_formula",
    "target"
  )
  
  missing <- setdiff(
    required,
    names(spec)
  )
  
  if (length(missing) > 0L) {
    .audit_stop(
      "Audit specification is missing: ",
      paste(missing, collapse = ", ")
    )
  }
  
  .audit_assert_string(
    spec$study_id,
    "study_id"
  )
  
  .audit_assert_string(
    spec$estimand_id,
    "estimand_id"
  )
  
  if (!is.data.frame(spec$data)) {
    .audit_stop(
      "`data` must be a data.frame."
    )
  }
  
  if (nrow(spec$data) < 3L) {
    .audit_stop(
      "Audit data contain fewer than three observations."
    )
  }
  
  if (!inherits(spec$mis_formula, "formula")) {
    .audit_stop(
      "`mis_formula` must be an R formula."
    )
  }
  
  .audit_assert_string(
    spec$target,
    "target"
  )
  
  if (is.null(spec$refit_target)) {
    spec$refit_target <- spec$target
  }
  
  .audit_assert_string(
    spec$refit_target,
    "refit_target"
  )
  
  if (!is.null(spec$id_var)) {
    
    .audit_assert_string(
      spec$id_var,
      "id_var"
    )
    
    if (!spec$id_var %in% names(spec$data)) {
      .audit_stop(
        "ID variable `",
        spec$id_var,
        "` does not exist in the audit data."
      )
    }
  }
  
  if (!is.null(spec$refit_fn) &&
      !is.function(spec$refit_fn)) {
    
    .audit_stop(
      "`refit_fn` must be NULL or a function."
    )
  }
  
  if (is.null(spec$beta_tolerance)) {
    spec$beta_tolerance <- 1e-8
  }
  
  .audit_assert_number(
    spec$beta_tolerance,
    "beta_tolerance",
    lower = 0
  )
  
  if (is.null(spec$max_fraction)) {
    spec$max_fraction <- 0.10
  }
  
  .audit_assert_number(
    spec$max_fraction,
    "max_fraction",
    lower = .Machine$double.eps,
    upper = 0.99
  )
  
  if (is.null(spec$run_greedy)) {
    spec$run_greedy <- TRUE
  }
  
  if (
    !is.logical(spec$run_greedy) ||
    length(spec$run_greedy) != 1L ||
    is.na(spec$run_greedy)
  ) {
    .audit_stop(
      "`run_greedy` must be TRUE or FALSE."
    )
  }
  
  if (!is.null(spec$expected_beta)) {
    
    .audit_assert_number(
      spec$expected_beta,
      "expected_beta"
    )
  }
  
  if (!is.null(spec$expected_n)) {
    
    if (
      !is.numeric(spec$expected_n) ||
      length(spec$expected_n) != 1L ||
      is.na(spec$expected_n) ||
      spec$expected_n < 1 ||
      spec$expected_n != as.integer(spec$expected_n)
    ) {
      .audit_stop(
        "`expected_n` must be a positive integer."
      )
    }
    
    spec$expected_n <- as.integer(
      spec$expected_n
    )
  }
  
  if (!is.null(spec$k_grid)) {
    
    if (
      !is.numeric(spec$k_grid) ||
      length(spec$k_grid) < 1L ||
      anyNA(spec$k_grid) ||
      any(!is.finite(spec$k_grid)) ||
      any(spec$k_grid != as.integer(spec$k_grid)) ||
      any(spec$k_grid < 1L)
    ) {
      .audit_stop(
        "`k_grid` must contain positive integer values."
      )
    }
    
    spec$k_grid <- sort(
      unique(
        as.integer(spec$k_grid)
      )
    )
  }
  
  spec
}


# ------------------------------------------------------------------------------
# Prepare MIS-compatible model and validate against original estimator
# ------------------------------------------------------------------------------

prepare_mis_audit <- function(
    spec,
    verbose = TRUE
) {
  
  spec <- audit_validate_spec(spec)
  
  dat <- spec$data
  
  n_input <- nrow(dat)
  
  
  # --------------------------------------------------------------------------
  # Stable observation ID
  # --------------------------------------------------------------------------
  
  if (is.null(spec$id_var)) {
    
    id_var <- ".audit_id"
    
    if (id_var %in% names(dat)) {
      .audit_stop(
        "Column `.audit_id` already exists. ",
        "Supply it explicitly as `id_var`, or rename it."
      )
    }
    
    dat[[id_var]] <- seq_len(
      nrow(dat)
    )
    
  } else {
    
    id_var <- spec$id_var
  }
  
  
  ids <- dat[[id_var]]
  
  if (anyNA(ids)) {
    .audit_stop(
      "Audit IDs contain missing values."
    )
  }
  
  if (anyDuplicated(ids)) {
    .audit_stop(
      "Audit IDs must uniquely identify observations."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Determine the exact complete-case sample implied by MIS formula
  # --------------------------------------------------------------------------
  
  mf_all <- tryCatch(
    stats::model.frame(
      formula = spec$mis_formula,
      data = dat,
      na.action = stats::na.pass,
      drop.unused.levels = TRUE
    ),
    error = function(e) {
      .audit_stop(
        "Could not construct MIS model frame: ",
        conditionMessage(e)
      )
    }
  )
  
  keep <- stats::complete.cases(
    mf_all
  )
  
  n_dropped_incomplete <- sum(!keep)
  
  if (
    n_dropped_incomplete > 0L &&
    isTRUE(verbose)
  ) {
    message(
      "MIS preparation removed ",
      n_dropped_incomplete,
      " observation(s) because the MIS formula is incomplete."
    )
  }
  
  dat <- dat[
    keep,
    ,
    drop = FALSE
  ]
  
  row.names(dat) <- NULL
  
  
  # --------------------------------------------------------------------------
  # Fit MIS-compatible OLS
  # --------------------------------------------------------------------------
  
  mis_model <- tryCatch(
    stats::lm(
      formula = spec$mis_formula,
      data = dat,
      na.action = stats::na.fail
    ),
    error = function(e) {
      .audit_stop(
        "MIS-compatible lm failed: ",
        conditionMessage(e)
      )
    }
  )
  
  X <- stats::model.matrix(
    mis_model
  )
  
  y <- stats::model.response(
    stats::model.frame(
      mis_model
    )
  )
  
  if (
    !is.numeric(y) ||
    anyNA(y) ||
    any(!is.finite(y))
  ) {
    .audit_stop(
      "MIS requires a finite numeric response."
    )
  }
  
  if (
    !is.numeric(X) ||
    anyNA(X) ||
    any(!is.finite(X))
  ) {
    .audit_stop(
      "MIS model matrix contains non-finite values."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Target coefficient
  # --------------------------------------------------------------------------
  
  target_pos <- match(
    spec$target,
    colnames(X)
  )
  
  if (is.na(target_pos)) {
    .audit_stop(
      "Target coefficient `",
      spec$target,
      "` is not present in the MIS model matrix.\nAvailable coefficients: ",
      paste(
        colnames(X),
        collapse = ", "
      )
    )
  }
  
  if (
    identical(
      colnames(X)[target_pos],
      "(Intercept)"
    )
  ) {
    .audit_stop(
      "The intercept cannot be the MIS target."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Rank / dimensions
  # --------------------------------------------------------------------------
  
  n <- nrow(X)
  p <- ncol(X)
  
  rank_x <- qr(X)$rank
  
  if (rank_x < p) {
    .audit_stop(
      "MIS model matrix is rank deficient: rank = ",
      rank_x,
      ", columns = ",
      p,
      "."
    )
  }
  
  if (n <= p + 1L) {
    .audit_stop(
      "Too few residual degrees of freedom for the MIS audit."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # MIS baseline coefficient
  # --------------------------------------------------------------------------
  
  beta_mis <- audit_extract_beta(
    mis_model,
    spec$target
  )
  
  
  # --------------------------------------------------------------------------
  # Re-estimate with the original paper estimator
  # --------------------------------------------------------------------------
  
  refit_fn <- spec$refit_fn
  
  if (is.null(refit_fn)) {
    refit_fn <- .audit_default_refit
  }
  
  ref_model <- tryCatch(
    refit_fn(
      dat,
      spec
    ),
    error = function(e) {
      .audit_stop(
        "Original-estimator baseline refit failed: ",
        conditionMessage(e)
      )
    }
  )
  
  beta_reference <- audit_extract_beta(
    ref_model,
    spec$refit_target
  )
  
  n_reference <- audit_extract_nobs(
    ref_model
  )
  
  if (
    !is.na(n_reference) &&
    n_reference != n
  ) {
    .audit_stop(
      "The original estimator uses ",
      n_reference,
      " observations, but the MIS model uses ",
      n,
      ". The samples must match before MIS is run."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Validation against expected replication result
  # --------------------------------------------------------------------------
  
  tol <- spec$beta_tolerance
  
  if (
    abs(beta_mis - beta_reference) >
    tol
  ) {
    .audit_stop(
      sprintf(
        paste0(
          "MIS-compatible lm does not reproduce the original estimator.\n",
          "Original estimator: %.12f\n",
          "MIS lm:            %.12f\n",
          "Difference:        %.12g\n",
          "Tolerance:         %.12g"
        ),
        beta_reference,
        beta_mis,
        beta_mis - beta_reference,
        tol
      )
    )
  }
  
  if (!is.null(spec$expected_beta)) {
    
    if (
      abs(
        beta_reference -
        spec$expected_beta
      ) > tol
    ) {
      .audit_stop(
        sprintf(
          paste0(
            "Original estimator does not reproduce expected coefficient.\n",
            "Expected: %.12f\n",
            "Obtained: %.12f"
          ),
          spec$expected_beta,
          beta_reference
        )
      )
    }
  }
  
  if (
    !is.null(spec$expected_n) &&
    n != spec$expected_n
  ) {
    .audit_stop(
      "Expected N = ",
      spec$expected_n,
      ", but audit sample N = ",
      n,
      "."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Return prepared object
  # --------------------------------------------------------------------------
  
  out <- list(
    spec = spec,
    
    data = dat,
    
    id_var = id_var,
    
    mis_model = mis_model,
    
    target_pos = as.integer(
      target_pos
    ),
    
    n = as.integer(n),
    p = as.integer(p),
    
    beta_mis = beta_mis,
    beta_reference = beta_reference,
    
    n_input = as.integer(
      n_input
    ),
    
    n_dropped_incomplete = as.integer(
      n_dropped_incomplete
    )
  )
  
  class(out) <- c(
    "mis_audit_prepared",
    "list"
  )
  
  if (isTRUE(verbose)) {
    
    message(
      "Audit validation passed: ",
      spec$study_id,
      " / ",
      spec$estimand_id
    )
    
    message(
      "  N = ",
      n,
      ", p = ",
      p
    )
    
    message(
      "  target = ",
      spec$target
    )
    
    message(
      sprintf(
        "  beta = %.10f",
        beta_reference
      )
    )
  }
  
  out
}