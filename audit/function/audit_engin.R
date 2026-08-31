# ==============================================================================
# audit/function/audit_engine.R
#
# Purpose:
#   Run a standardized empirical MIS audit.
#
# Requires:
#   audit_validate.R
#   dinkelbach_topk_lm()
#   fast_sens_topk() if run_greedy = TRUE
#
# Default protocol:
#   k = 1, ..., floor(0.05 * N)
#   subject to the safety cap N - p - 1.
#
# For each k:
#   - exact/main MIS increasing target slope
#   - exact/main MIS decreasing target slope
#   - fast/greedy benchmark in both directions
#   - exact re-estimation with the paper's estimator
#   - observation IDs
#   - nestedness
#   - MIS/greedy Jaccard overlap
# ==============================================================================


.audit_engine_require <- function(
    name
) {
  
  if (
    !exists(
      name,
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Required function `",
      name,
      "` is not available.",
      call. = FALSE
    )
  }
  
  get(
    name,
    mode = "function",
    inherits = TRUE
  )
}


.audit_extract_indices <- function(
    result,
    k,
    n,
    method
) {
  
  if (
    is.list(result) &&
    !is.null(result$indices)
  ) {
    result <- result$indices
  }
  
  if (
    !is.numeric(result) ||
    length(result) != k ||
    anyNA(result) ||
    any(!is.finite(result)) ||
    any(result != as.integer(result))
  ) {
    stop(
      method,
      " returned an invalid deletion set for k = ",
      k,
      ".",
      call. = FALSE
    )
  }
  
  idx <- as.integer(
    result
  )
  
  if (
    any(idx < 1L) ||
    any(idx > n)
  ) {
    stop(
      method,
      " returned row indices outside the audit sample.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(idx)) {
    stop(
      method,
      " returned duplicate row indices.",
      call. = FALSE
    )
  }
  
  idx
}


audit_jaccard <- function(
    a,
    b
) {
  
  a <- unique(a)
  b <- unique(b)
  
  u <- union(a, b)
  
  if (length(u) == 0L) {
    return(
      NA_real_
    )
  }
  
  length(
    intersect(a, b)
  ) /
    length(u)
}


# ------------------------------------------------------------------------------
# k protocol
# ------------------------------------------------------------------------------

.audit_resolve_k_grid <- function(
    prepared
) {
  
  spec <- prepared$spec
  
  n <- prepared$n
  p <- prepared$p
  
  # Standard protocol target.
  fraction_cap <- floor(
    n * spec$max_fraction
  )
  
  # For very small samples, still allow k = 1 if algebraically admissible.
  fraction_cap <- max(
    1L,
    fraction_cap
  )
  
  # Keep at least one residual degree of freedom after deletion.
  df_cap <- n - p - 1L
  
  maximum_admissible <- min(
    fraction_cap,
    df_cap
  )
  
  if (maximum_admissible < 1L) {
    stop(
      "No admissible positive k for this model.",
      call. = FALSE
    )
  }
  
  if (is.null(spec$k_grid)) {
    
    return(
      seq_len(
        maximum_admissible
      )
    )
  }
  
  k_grid <- spec$k_grid
  
  if (any(k_grid > maximum_admissible)) {
    stop(
      "Requested k exceeds the admissible maximum of ",
      maximum_admissible,
      ".",
      call. = FALSE
    )
  }
  
  k_grid
}


# ------------------------------------------------------------------------------
# Exact paper-estimator refit after deletion
# ------------------------------------------------------------------------------

.audit_refit_after_delete <- function(
    prepared,
    delete_positions
) {
  
  spec <- prepared$spec
  
  delete_positions <- sort(
    unique(
      as.integer(
        delete_positions
      )
    )
  )
  
  dat_new <- prepared$data[
    -delete_positions,
    ,
    drop = FALSE
  ]
  
  expected_n <- nrow(
    dat_new
  )
  
  refit_fn <- spec$refit_fn
  
  if (is.null(refit_fn)) {
    refit_fn <- .audit_default_refit
  }
  
  tryCatch(
    {
      
      fit <- refit_fn(
        dat_new,
        spec
      )
      
      beta <- audit_extract_beta(
        fit,
        spec$refit_target
      )
      
      actual_n <- audit_extract_nobs(
        fit
      )
      
      if (
        !is.na(actual_n) &&
        actual_n != expected_n
      ) {
        stop(
          "Refit used ",
          actual_n,
          " observations instead of expected ",
          expected_n,
          "."
        )
      }
      
      list(
        valid = TRUE,
        beta = beta,
        n = if (
          is.na(actual_n)
        ) {
          expected_n
        } else {
          actual_n
        },
        error = NA_character_
      )
    },
    
    error = function(e) {
      
      list(
        valid = FALSE,
        beta = NA_real_,
        n = NA_integer_,
        error = conditionMessage(e)
      )
    }
  )
}


# ------------------------------------------------------------------------------
# Summary helpers
# ------------------------------------------------------------------------------

.audit_first_event <- function(
    path,
    condition
) {
  
  ok <- (
    condition &
      path$valid_mis &
      is.finite(path$k)
  )
  
  z <- path[
    ok,
    ,
    drop = FALSE
  ]
  
  if (nrow(z) == 0L) {
    
    return(
      list(
        k = NA_integer_,
        fraction = NA_real_,
        direction = NA_character_
      )
    )
  }
  
  z <- z[
    order(
      z$k,
      match(
        z$direction,
        c(
          "Increase",
          "Decrease"
        )
      )
    ),
    ,
    drop = FALSE
  ]
  
  list(
    k = as.integer(
      z$k[1]
    ),
    fraction = as.numeric(
      z$removal_fraction[1]
    ),
    direction = as.character(
      z$direction[1]
    )
  )
}


summarise_mis_audit <- function(
    audit
) {
  
  path <- audit$path
  
  beta0 <- audit$baseline$beta_original
  
  rel25 <- .audit_first_event(
    path,
    is.finite(path$relative_change_mis) &
      path$relative_change_mis >= 0.25
  )
  
  rel50 <- .audit_first_event(
    path,
    is.finite(path$relative_change_mis) &
      path$relative_change_mis >= 0.50
  )
  
  if (
    is.finite(beta0) &&
    abs(beta0) > .Machine$double.eps
  ) {
    
    cross_zero_condition <- (
      is.finite(path$beta_mis) &
        beta0 * path$beta_mis <= 0
    )
    
    sign_flip_condition <- (
      is.finite(path$beta_mis) &
        beta0 * path$beta_mis < 0
    )
    
  } else {
    
    cross_zero_condition <- rep(
      FALSE,
      nrow(path)
    )
    
    sign_flip_condition <- rep(
      FALSE,
      nrow(path)
    )
  }
  
  cross_zero <- .audit_first_event(
    path,
    cross_zero_condition
  )
  
  sign_flip <- .audit_first_event(
    path,
    sign_flip_condition
  )
  
  valid_rel <- path$relative_change_mis[
    path$valid_mis &
      is.finite(
        path$relative_change_mis
      )
  ]
  
  max_relative_change <- if (
    length(valid_rel) == 0L
  ) {
    NA_real_
  } else {
    max(valid_rel)
  }
  
  jac <- path$jaccard_greedy[
    is.finite(
      path$jaccard_greedy
    )
  ]
  
  mean_jaccard <- if (
    length(jac) == 0L
  ) {
    NA_real_
  } else {
    mean(jac)
  }
  
  min_jaccard <- if (
    length(jac) == 0L
  ) {
    NA_real_
  } else {
    min(jac)
  }
  
  non_nested_events <- sum(
    path$nested_mis == FALSE,
    na.rm = TRUE
  )
  
  data.frame(
    study_id =
      audit$baseline$study_id,
    
    estimand_id =
      audit$baseline$estimand_id,
    
    target =
      audit$baseline$target,
    
    n =
      audit$baseline$n,
    
    p =
      audit$baseline$p,
    
    beta_original =
      beta0,
    
    max_k =
      max(path$k),
    
    max_removal_fraction =
      max(path$removal_fraction),
    
    max_relative_change =
      max_relative_change,
    
    min_k_25pct =
      rel25$k,
    
    min_fraction_25pct =
      rel25$fraction,
    
    direction_25pct =
      rel25$direction,
    
    min_k_50pct =
      rel50$k,
    
    min_fraction_50pct =
      rel50$fraction,
    
    direction_50pct =
      rel50$direction,
    
    min_k_cross_zero =
      cross_zero$k,
    
    min_fraction_cross_zero =
      cross_zero$fraction,
    
    direction_cross_zero =
      cross_zero$direction,
    
    min_k_sign_flip =
      sign_flip$k,
    
    min_fraction_sign_flip =
      sign_flip$fraction,
    
    direction_sign_flip =
      sign_flip$direction,
    
    mean_jaccard_greedy =
      mean_jaccard,
    
    min_jaccard_greedy =
      min_jaccard,
    
    non_nested_events =
      non_nested_events,
    
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# Main audit
# ------------------------------------------------------------------------------

run_mis_audit <- function(
    spec,
    mis_fn = NULL,
    greedy_fn = NULL,
    verbose = TRUE
) {
  
  if (
    !exists(
      "prepare_mis_audit",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Source audit_validate.R before running the audit.",
      call. = FALSE
    )
  }
  
  prepared <- prepare_mis_audit(
    spec,
    verbose = verbose
  )
  
  if (is.null(mis_fn)) {
    mis_fn <- .audit_engine_require(
      "dinkelbach_topk_lm"
    )
  }
  
  if (
    prepared$spec$run_greedy &&
    is.null(greedy_fn)
  ) {
    greedy_fn <- .audit_engine_require(
      "fast_sens_topk"
    )
  }
  
  k_grid <- .audit_resolve_k_grid(
    prepared
  )
  
  mod <- prepared$mis_model
  
  target_pos <- prepared$target_pos
  
  n <- prepared$n
  
  beta0 <- prepared$beta_reference
  
  ids <- prepared$data[
    [prepared$id_var]
  ]
  
  rows <- list()
  counter <- 1L
  
  direction_values <- c(
    Increase = 1L,
    Decrease = -1L
  )
  
  for (
    direction_name in names(
      direction_values
    )
  ) {
    
    direction <- direction_values[
      [direction_name]
    ]
    
    previous_mis_set <- NULL
    
    for (k in k_grid) {
      
      if (isTRUE(verbose)) {
        message(
          prepared$spec$study_id,
          " | ",
          direction_name,
          " | k = ",
          k,
          "/",
          max(k_grid)
        )
      }
      
      
      # ----------------------------------------------------------------------
      # Main MIS
      # ----------------------------------------------------------------------
      
      mis_raw <- mis_fn(
        mod,
        pos = target_pos,
        sign = direction,
        k = k
      )
      
      mis_set <- .audit_extract_indices(
        result = mis_raw,
        k = k,
        n = n,
        method = "MIS"
      )
      
      mis_refit <- .audit_refit_after_delete(
        prepared,
        mis_set
      )
      
      beta_mis <- mis_refit$beta
      
      delta_mis <- if (
        is.finite(beta_mis)
      ) {
        beta_mis - beta0
      } else {
        NA_real_
      }
      
      abs_delta_mis <- if (
        is.finite(delta_mis)
      ) {
        abs(delta_mis)
      } else {
        NA_real_
      }
      
      relative_change_mis <- if (
        is.finite(delta_mis) &&
        abs(beta0) > .Machine$double.eps
      ) {
        abs(delta_mis) /
          abs(beta0)
      } else {
        NA_real_
      }
      
      
      # ----------------------------------------------------------------------
      # Nestedness relative to previous k in the path
      # ----------------------------------------------------------------------
      
      nested_mis <- if (
        is.null(previous_mis_set)
      ) {
        
        NA
        
      } else {
        
        all(
          previous_mis_set %in%
            mis_set
        )
      }
      
      
      # ----------------------------------------------------------------------
      # Fast/greedy benchmark
      # ----------------------------------------------------------------------
      
      greedy_set <- integer(0)
      
      greedy_refit <- list(
        valid = FALSE,
        beta = NA_real_,
        n = NA_integer_,
        error = NA_character_
      )
      
      beta_greedy <- NA_real_
      delta_greedy <- NA_real_
      abs_delta_greedy <- NA_real_
      jaccard <- NA_real_
      
      if (
        isTRUE(
          prepared$spec$run_greedy
        )
      ) {
        
        greedy_raw <- greedy_fn(
          mod,
          pos = target_pos,
          sign = direction,
          k = k
        )
        
        greedy_set <- .audit_extract_indices(
          result = greedy_raw,
          k = k,
          n = n,
          method = "Greedy"
        )
        
        greedy_refit <- .audit_refit_after_delete(
          prepared,
          greedy_set
        )
        
        beta_greedy <- greedy_refit$beta
        
        if (is.finite(beta_greedy)) {
          
          delta_greedy <-
            beta_greedy -
            beta0
          
          abs_delta_greedy <-
            abs(
              delta_greedy
            )
        }
        
        jaccard <- audit_jaccard(
          mis_set,
          greedy_set
        )
      }
      
      
      row <- data.frame(
        study_id =
          prepared$spec$study_id,
        
        estimand_id =
          prepared$spec$estimand_id,
        
        target =
          prepared$spec$target,
        
        k =
          as.integer(k),
        
        removal_fraction =
          k / n,
        
        direction =
          direction_name,
        
        beta_original =
          beta0,
        
        beta_mis =
          beta_mis,
        
        delta_mis =
          delta_mis,
        
        abs_delta_mis =
          abs_delta_mis,
        
        relative_change_mis =
          relative_change_mis,
        
        valid_mis =
          mis_refit$valid,
        
        refit_error_mis =
          mis_refit$error,
        
        beta_greedy =
          beta_greedy,
        
        delta_greedy =
          delta_greedy,
        
        abs_delta_greedy =
          abs_delta_greedy,
        
        valid_greedy =
          greedy_refit$valid,
        
        refit_error_greedy =
          greedy_refit$error,
        
        jaccard_greedy =
          jaccard,
        
        nested_mis =
          nested_mis,
        
        stringsAsFactors = FALSE
      )
      
      # Keep both row positions and stable original IDs.
      row$mis_positions <- I(
        list(
          mis_set
        )
      )
      
      row$mis_ids <- I(
        list(
          ids[
            mis_set
          ]
        )
      )
      
      row$greedy_positions <- I(
        list(
          greedy_set
        )
      )
      
      row$greedy_ids <- I(
        list(
          ids[
            greedy_set
          ]
        )
      )
      
      rows[[counter]] <- row
      counter <- counter + 1L
      
      previous_mis_set <- mis_set
    }
  }
  
  path <- do.call(
    rbind,
    rows
  )
  
  protocol_k_5pct <- floor(
    prepared$n * prepared$spec$max_fraction
  )
  
  baseline <- data.frame(
    study_id =
      prepared$spec$study_id,
    
    estimand_id =
      prepared$spec$estimand_id,
    
    target =
      prepared$spec$target,
    
    id_var =
      prepared$id_var,
    
    n_input =
      prepared$n_input,
    
    n_dropped_incomplete =
      prepared$n_dropped_incomplete,
    
    n =
      prepared$n,
    
    p =
      prepared$p,
    
    beta_original =
      prepared$beta_reference,
    
    beta_mis_model =
      prepared$beta_mis,
    
    beta_difference =
      prepared$beta_mis -
      prepared$beta_reference,
    
    protocol_max_fraction =
      prepared$spec$max_fraction,
    
    protocol_floor_fraction_n =
      protocol_k_5pct,
    
    actual_max_k =
      max(k_grid),
    
    stringsAsFactors = FALSE
  )
  
  audit <- list(
    baseline = baseline,
    path = path,
    
    specification = list(
      study_id =
        prepared$spec$study_id,
      
      estimand_id =
        prepared$spec$estimand_id,
      
      target =
        prepared$spec$target,
      
      refit_target =
        prepared$spec$refit_target,
      
      mis_formula =
        paste(
          deparse(
            prepared$spec$mis_formula
          ),
          collapse = " "
        ),
      
      max_fraction =
        prepared$spec$max_fraction,
      
      k_grid =
        k_grid,
      
      run_greedy =
        prepared$spec$run_greedy
    )
  )
  
  class(audit) <- c(
    "mis_audit",
    "list"
  )
  
  audit$summary <- summarise_mis_audit(
    audit
  )
  
  if (isTRUE(verbose)) {
    message(
      "MIS audit complete: ",
      prepared$spec$study_id,
      " / ",
      prepared$spec$estimand_id
    )
  }
  
  audit
}