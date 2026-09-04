# 03_helpers.R
# Shared helper functions for the Huguka Dukore R replication.

# ------------------------------------------------------------------
# Basic utilities
# ------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


# ------------------------------------------------------------------
# Read source data
# ------------------------------------------------------------------

read_panel <- function() {
  
  if (!file.exists(paths$panel)) {
    stop(
      "Missing required input: ",
      paths$panel
    )
  }
  
  haven::read_dta(
    paths$panel
  )
}


read_costs <- function() {
  
  if (!file.exists(paths$cost)) {
    stop(
      "Missing required input: ",
      paths$cost
    )
  }
  
  # Read the Costing sheet
  x <- readxl::read_excel(
    paths$cost,
    sheet = "Costing"
  )
  
  # --------------------------------------------------------------
  # Normalize Excel column names to match Stata's import behavior.
  #
  # Examples:
  # "Treatment Arm"            -> "treatmentarm"
  # "Cost per Study Household" -> "costperstudyhousehold"
  # --------------------------------------------------------------
  
  names(x) <- names(x) |>
    trimws() |>
    tolower() |>
    gsub(
      "[^a-z0-9]+",
      "",
      x = _
    )
  
  # Required columns used by the original Stata scripts
  required <- c(
    "treatmentarm",
    "costperstudyhousehold"
  )
  
  missing_cols <- setdiff(
    required,
    names(x)
  )
  
  if (length(missing_cols)) {
    stop(
      "HD_cost.xlsx / Costing sheet is missing required column(s): ",
      paste(
        missing_cols,
        collapse = ", "
      ),
      "\nDetected columns: ",
      paste(
        names(x),
        collapse = ", "
      )
    )
  }
  
  # Clean treatment-arm labels
  x$treatmentarm <- trimws(
    as.character(
      x$treatmentarm
    )
  )
  
  # Ensure costs are numeric
  x$costperstudyhousehold <- as.numeric(
    x$costperstudyhousehold
  )
  
  # Mean cost for each treatment arm
  arm_costs <- tapply(
    x$costperstudyhousehold,
    x$treatmentarm,
    mean,
    na.rm = TRUE
  )
  
  out <- as.list(
    arm_costs
  )
  
  # --------------------------------------------------------------
  # Construct GD_main exactly as in the original Stata workflow:
  #
  # (GD_lower + GD_middle + GD_upper) / 3
  # --------------------------------------------------------------
  
  arm_names_lower <- tolower(
    names(out)
  )
  
  gd_idx <- match(
    c(
      "gd_lower",
      "gd_middle",
      "gd_upper"
    ),
    arm_names_lower
  )
  
  if (any(is.na(gd_idx))) {
    stop(
      "Could not construct GD_main cost.\n",
      "Expected treatment arms GD_lower, GD_middle, and GD_upper.\n",
      "Detected treatment arms: ",
      paste(
        names(out),
        collapse = ", "
      )
    )
  }
  
  out$GD_main <- mean(
    as.numeric(
      unlist(
        out[gd_idx]
      )
    )
  )
  
  out
}

cost_value <- function(costs, key) {
  
  idx <- match(
    tolower(key),
    tolower(names(costs))
  )
  
  if (is.na(idx)) {
    stop(
      "Cost arm not found in HD_cost.xlsx: ",
      key
    )
  }
  
  as.numeric(
    costs[[idx]]
  )
}


# ------------------------------------------------------------------
# Variable utilities
# ------------------------------------------------------------------

block_vars <- function(data) {
  
  grep(
    "^_Iblock",
    names(data),
    value = TRUE
  )
}


existing_vars <- function(data, vars) {
  
  vars <- as.character(vars)
  
  unique(
    vars[
      vars %in% names(data)
    ]
  )
}


nonempty_vars <- function(data, vars) {
  
  vars <- existing_vars(
    data,
    vars
  )
  
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


rhs_controls <- function(required = TRUE) {
  
  if (!file.exists(paths$controls)) {
    
    if (required) {
      stop(
        "Missing generated controls file: ",
        paths$controls,
        "\nRun code/04_covariate_choice.R first."
      )
    }
    
    return(
      character()
    )
  }
  
  x <- readr::read_csv(
    paths$controls,
    show_col_types = FALSE
  )
  
  if (!"controls" %in% names(x)) {
    stop(
      "HD_controls.csv must contain a column named 'controls'."
    )
  }
  
  unique(
    stats::na.omit(
      x$controls
    )
  )
}


var_label <- function(data, v) {
  
  z <- attr(
    data[[v]],
    "label"
  )
  
  if (
    is.null(z) ||
    !nzchar(z)
  ) {
    v
  } else {
    z
  }
}


weighted_mean <- function(x, w = NULL) {
  
  if (is.null(w)) {
    return(
      mean(
        x,
        na.rm = TRUE
      )
    )
  }
  
  ok <- !is.na(x) &
    !is.na(w)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(
    x[ok],
    w[ok]
  )
}


safe_r2 <- function(model) {
  
  z <- tryCatch(
    fixest::fitstat(
      model,
      "r2"
    ),
    error = function(e) NULL
  )
  
  if (is.null(z)) {
    return(NA_real_)
  }
  
  as.numeric(
    z[[1]]
  )
}


# ------------------------------------------------------------------
# Safe formula construction
#
# Important:
# Stata variables such as _Iblock_2508 are valid in Stata but are
# non-syntactic names in R formulas. They must be wrapped in backticks.
# ------------------------------------------------------------------

quote_formula_var <- function(x) {
  
  x <- as.character(x)
  
  vapply(
    x,
    function(z) {
      
      if (
        is.na(z) ||
        !nzchar(z)
      ) {
        return(z)
      }
      
      # Already quoted
      if (
        startsWith(z, "`") &&
        endsWith(z, "`")
      ) {
        return(z)
      }
      
      # Normal syntactic R variable name
      if (
        identical(
          make.names(z),
          z
        )
      ) {
        return(z)
      }
      
      # Non-syntactic variable name
      paste0(
        "`",
        z,
        "`"
      )
    },
    character(1)
  )
}


make_formula <- function(
    outcome,
    terms = character()
) {
  
  terms <- as.character(
    terms
  )
  
  terms <- unique(
    terms[
      !is.na(terms) &
        nzchar(terms)
    ]
  )
  
  outcome_safe <- quote_formula_var(
    outcome
  )
  
  if (!length(terms)) {
    
    return(
      stats::as.formula(
        paste0(
          outcome_safe,
          " ~ 1"
        ),
        env = parent.frame()
      )
    )
  }
  
  terms_safe <- quote_formula_var(
    terms
  )
  
  rhs <- paste(
    terms_safe,
    collapse = " + "
  )
  
  stats::as.formula(
    paste(
      outcome_safe,
      "~",
      rhs
    ),
    env = parent.frame()
  )
}


make_one_sided_formula <- function(x) {
  
  x_safe <- quote_formula_var(
    x
  )
  
  stats::as.formula(
    paste0(
      "~ ",
      x_safe
    ),
    env = parent.frame()
  )
}


# ------------------------------------------------------------------
# Regression helper
# ------------------------------------------------------------------

fit_model <- function(
    data,
    outcome,
    terms,
    weight = NULL,
    cluster = NULL,
    subset = NULL
) {
  
  d <- data
  
  # --------------------------------------------------------------
  # Apply sample restriction
  # --------------------------------------------------------------
  
  if (!is.null(subset)) {
    
    subset <- as.logical(subset)
    
    if (length(subset) == 1L) {
      subset <- rep(subset, nrow(d))
    }
    
    if (length(subset) != nrow(d)) {
      stop(
        "subset must have length 1 or nrow(data)."
      )
    }
    
    subset[is.na(subset)] <- FALSE
    
    d <- d[
      subset,
      ,
      drop = FALSE
    ]
  }
  
  if (!outcome %in% names(d)) {
    stop(
      "Outcome not found: ",
      outcome
    )
  }
  
  terms <- nonempty_vars(
    d,
    terms
  )
  
  # --------------------------------------------------------------
  # Reproduce Stata's complete-case estimation sample explicitly
  # --------------------------------------------------------------
  
  required <- c(
    outcome,
    terms
  )
  
  if (
    !is.null(weight) &&
    weight %in% names(d)
  ) {
    required <- c(
      required,
      weight
    )
  }
  
  if (
    !is.null(cluster) &&
    cluster %in% names(d)
  ) {
    required <- c(
      required,
      cluster
    )
  }
  
  required <- unique(required)
  
  keep <- stats::complete.cases(
    d[
      ,
      required,
      drop = FALSE
    ]
  )
  
  if (
    !is.null(weight) &&
    weight %in% names(d)
  ) {
    
    keep <- keep &
      is.finite(
        as.numeric(d[[weight]])
      ) &
      as.numeric(d[[weight]]) > 0
  }
  
  dd <- d[
    keep,
    ,
    drop = FALSE
  ]
  
  if (!nrow(dd)) {
    stop(
      "No complete observations remain for outcome ",
      outcome,
      "."
    )
  }
  
  # --------------------------------------------------------------
  # Formula
  # --------------------------------------------------------------
  
  fml <- make_formula(
    outcome,
    terms
  )
  
  w <- NULL
  
  if (
    !is.null(weight) &&
    weight %in% names(dd)
  ) {
    
    w <- make_one_sided_formula(
      weight
    )
  }
  
  vc <- "iid"
  
  if (
    !is.null(cluster) &&
    cluster %in% names(dd)
  ) {
    
    vc <- make_one_sided_formula(
      cluster
    )
  }
  
  # --------------------------------------------------------------
  # Estimate model
  # --------------------------------------------------------------
  
  model <- fixest::feols(
    fml,
    data = dd,
    weights = w,
    vcov = vc,
    notes = FALSE,
    warn = FALSE
  )
  
  # --------------------------------------------------------------
  # Store Stata-style cluster degrees of freedom
  # --------------------------------------------------------------
  
  if (
    !is.null(cluster) &&
    cluster %in% names(dd)
  ) {
    
    n_clust <- length(
      unique(
        dd[[cluster]]
      )
    )
    
    attr(
      model,
      "stata_n_clust"
    ) <- n_clust
    
    attr(
      model,
      "stata_df_r"
    ) <- max(
      n_clust - 1L,
      1L
    )
    
  } else {
    
    attr(
      model,
      "stata_n_clust"
    ) <- NA_integer_
    
    attr(
      model,
      "stata_df_r"
    ) <- max(
      stats::nobs(model) -
        length(stats::coef(model)),
      1L
    )
  }
  
  model
}


# ------------------------------------------------------------------
# Extract regression results
# ------------------------------------------------------------------

coef_table <- function(model) {
  
  x <- as.data.frame(
    fixest::coeftable(
      model
    )
  )
  
  x$term <- rownames(x)
  
  rownames(x) <- NULL
  
  names(x)[1:4] <- c(
    "estimate",
    "std.error",
    "statistic",
    "p.value"
  )
  
  # --------------------------------------------------------------
  # Original Stata scripts:
  #
  # 2 * ttail(e(N_clust), abs(b / se))
  # --------------------------------------------------------------
  
  n_clust <- attr(
    model,
    "stata_n_clust"
  )
  
  if (
    !is.null(n_clust) &&
    length(n_clust) == 1L &&
    !is.na(n_clust) &&
    n_clust > 0
  ) {
    
    x$p.value <- 2 *
      stats::pt(
        abs(x$statistic),
        df = n_clust,
        lower.tail = FALSE
      )
  }
  
  x[
    ,
    c(
      "term",
      "estimate",
      "std.error",
      "statistic",
      "p.value"
    )
  ]
}


extract_terms <- function(model, terms) {
  
  ct <- coef_table(
    model
  )
  
  tibble::tibble(
    term = terms
  ) |>
    dplyr::left_join(
      ct,
      by = "term"
    )
}


linear_contrast <- function(
    model,
    weights,
    rhs = 0
) {
  
  b <- stats::coef(
    model
  )
  
  V <- stats::vcov(
    model
  )
  
  keep <- intersect(
    names(weights),
    names(b)
  )
  
  if (!length(keep)) {
    
    return(
      c(
        estimate = NA_real_,
        std.error = NA_real_,
        p.value = NA_real_
      )
    )
  }
  
  w <- weights[
    keep
  ]
  
  est <- sum(
    w * b[keep]
  ) - rhs
  
  se <- sqrt(
    as.numeric(
      t(w) %*%
        V[
          keep,
          keep,
          drop = FALSE
        ] %*%
        w
    )
  )
  
  df_r <- attr(
    model,
    "stata_df_r"
  )
  
  if (
    is.null(df_r) ||
    is.na(df_r)
  ) {
    df_r <- Inf
  }
  
  p <- if (
    is.finite(se) &&
    se > 0
  ) {
    
    2 *
      stats::pt(
        abs(est / se),
        df = df_r,
        lower.tail = FALSE
      )
    
  } else {
    
    NA_real_
  }
  
  c(
    estimate = est,
    std.error = se,
    p.value = p
  )
}


joint_zero_test <- function(
    model,
    vars
) {
  
  b <- stats::coef(
    model
  )
  
  V <- stats::vcov(
    model
  )
  
  vars <- intersect(
    vars,
    names(b)
  )
  
  q <- length(vars)
  
  if (!q) {
    return(NA_real_)
  }
  
  bb <- b[
    vars
  ]
  
  VV <- V[
    vars,
    vars,
    drop = FALSE
  ]
  
  wald <- tryCatch(
    as.numeric(
      t(bb) %*%
        solve(
          VV,
          bb
        )
    ),
    error = function(e) NA_real_
  )
  
  if (!is.finite(wald)) {
    return(NA_real_)
  }
  
  df_r <- attr(
    model,
    "stata_df_r"
  )
  
  if (
    is.null(df_r) ||
    is.na(df_r)
  ) {
    return(
      stats::pchisq(
        wald,
        df = q,
        lower.tail = FALSE
      )
    )
  }
  
  # Stata test after clustered regression is an F test
  fstat <- wald / q
  
  stats::pf(
    fstat,
    df1 = q,
    df2 = df_r,
    lower.tail = FALSE
  )
}


# ------------------------------------------------------------------
# Sharpened FDR q-values
# ------------------------------------------------------------------

sharpened_qvalues <- function(p) {
  
  shape <- dim(p)
  
  pv <- as.numeric(p)
  
  out <- rep(
    NA_real_,
    length(pv)
  )
  
  ok <- which(
    !is.na(pv)
  )
  
  if (!length(ok)) {
    
    if (is.null(shape)) {
      return(out)
    }
    
    return(
      array(
        out,
        dim = shape
      )
    )
  }
  
  # Sort p-values, as in the Stata implementation
  ord <- ok[
    order(
      pv[ok]
    )
  ]
  
  ps <- pv[
    ord
  ]
  
  m <- length(ps)
  
  ranks <- seq_len(m)
  
  qv <- rep(
    1,
    m
  )
  
  # --------------------------------------------------------------
  # BKY (2006) two-stage sharpened FDR procedure
  # evaluated on the same 0.001 grid used by the Stata routine.
  # --------------------------------------------------------------
  
  for (
    q in seq(
      1,
      0.001,
      by = -0.001
    )
  ) {
    
    # Stage 1
    q_adj <- q / (1 + q)
    
    critical1 <- q_adj *
      ranks /
      m
    
    rejected1 <- which(
      ps <= critical1
    )
    
    r1 <- if (
      length(rejected1)
    ) {
      max(rejected1)
    } else {
      0L
    }
    
    # Stage 2
    if (r1 >= m) {
      
      r2 <- m
      
    } else {
      
      q_2stage <- q_adj *
        (
          m /
            (m - r1)
        )
      
      critical2 <- q_2stage *
        ranks /
        m
      
      rejected2 <- which(
        ps <= critical2
      )
      
      r2 <- if (
        length(rejected2)
      ) {
        max(rejected2)
      } else {
        0L
      }
    }
    
    if (r2 > 0L) {
      
      qv[
        seq_len(r2)
      ] <- pmin(
        qv[
          seq_len(r2)
        ],
        q
      )
    }
  }
  
  out[
    ord
  ] <- qv
  
  if (is.null(shape)) {
    
    out
    
  } else {
    
    array(
      out,
      dim = shape
    )
  }
}


apply_q_by_family <- function(
    results,
    families
) {
  
  results$q.value <- NA_real_
  
  for (fam in names(families)) {
    
    idx <- which(
      results$outcome %in%
        families[[fam]]
    )
    
    if (length(idx)) {
      
      results$q.value[
        idx
      ] <- sharpened_qvalues(
        results$p.value[
          idx
        ]
      )
    }
  }
  
  results
}


# ------------------------------------------------------------------
# LaTeX utilities
# ------------------------------------------------------------------

tex_escape <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "\\\\",
    "\\\\textbackslash{}",
    x
  )
  
  x <- gsub(
    "([&_#%$])",
    "\\\\\\1",
    x,
    perl = TRUE
  )
  
  x <- gsub(
    "~",
    "\\\\textasciitilde{}",
    x,
    fixed = TRUE
  )
  
  x
}


# ------------------------------------------------------------------
# Result-file helpers
#
# All reported R results start with "R-".
# Generated intermediate data such as HD_controls.csv do not.
# ------------------------------------------------------------------

add_result_prefix <- function(filename) {
  
  filename <- basename(
    filename
  )
  
  prefix <- paths$result_prefix %||% "R-"
  
  if (
    startsWith(
      filename,
      prefix
    )
  ) {
    return(filename)
  }
  
  paste0(
    prefix,
    filename
  )
}


table_file <- function(filename) {
  
  file.path(
    paths$tables,
    add_result_prefix(
      filename
    )
  )
}


figure_file <- function(filename) {
  
  file.path(
    paths$figures,
    add_result_prefix(
      filename
    )
  )
}


write_simple_tex <- function(
    df,
    path,
    digits = 3,
    caption = NULL
) {
  
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  x <- df
  
  for (j in seq_along(x)) {
    
    if (is.numeric(x[[j]])) {
      
      x[[j]] <- ifelse(
        is.na(x[[j]]),
        "",
        formatC(
          x[[j]],
          digits = digits,
          format = "f"
        )
      )
      
    } else {
      
      x[[j]] <- tex_escape(
        x[[j]]
      )
    }
  }
  
  con <- file(
    path,
    open = "wt"
  )
  
  on.exit(
    close(con),
    add = TRUE
  )
  
  if (!is.null(caption)) {
    writeLines(
      paste0(
        "% ",
        caption
      ),
      con
    )
  }
  
  writeLines(
    paste0(
      "\\begin{tabular}{",
      paste(
        rep(
          "l",
          ncol(x)
        ),
        collapse = ""
      ),
      "}"
    ),
    con
  )
  
  writeLines(
    "\\toprule",
    con
  )
  
  writeLines(
    paste(
      tex_escape(
        names(x)
      ),
      collapse = " & "
    ),
    con
  )
  
  writeLines(
    " \\\\",
    con
  )
  
  writeLines(
    "\\midrule",
    con
  )
  
  for (i in seq_len(nrow(x))) {
    
    writeLines(
      paste0(
        paste(
          x[i, ],
          collapse = " & "
        ),
        " \\\\"
      ),
      con
    )
  }
  
  writeLines(
    "\\bottomrule",
    con
  )
  
  writeLines(
    "\\end{tabular}",
    con
  )
}


write_outputs <- function(
    df,
    tex_name,
    digits = 3
) {
  
  tex_path <- table_file(
    tex_name
  )
  
  csv_path <- sub(
    "\\.tex$",
    ".csv",
    tex_path
  )
  
  readr::write_csv(
    df,
    csv_path
  )
  
  write_simple_tex(
    df,
    tex_path,
    digits = digits
  )
  
  invisible(df)
}


save_plot <- function(
    p,
    filename,
    width = 7,
    height = 5
) {
  
  ggplot2::ggsave(
    filename = figure_file(
      filename
    ),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}