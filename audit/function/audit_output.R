# ==============================================================================
# audit/function/audit_output.R
#
# Purpose:
#   Save standardized MIS audit outputs for:
#
#   1. reproducibility / empirical audit:
#        - RDS
#        - CSV
#
#   2. paper writing:
#        - LaTeX tables for compact manuscript-facing outputs
#
# Required reproducibility outputs:
#   audit.rds
#   audit_baseline.csv
#   audit_path.csv
#   audit_summary.csv
#   audit_influential_ids.csv
#
# Paper-facing LaTeX outputs:
#   audit_baseline.tex
#   audit_summary.tex
#
# The full MIS path and influential-observation files remain CSV by default
# because they may contain many rows and are primarily audit records rather
# than manuscript tables.
#
# Public functions:
#   audit_path_for_csv()
#   audit_ids_long()
#   audit_write_tex_table()
#   save_mis_audit()
# ==============================================================================

# ------------------------------------------------------------------------------
# Flatten path for CSV
# ------------------------------------------------------------------------------

.audit_collapse_vector <- function(
    x
) {
  
  if (length(x) == 0L) {
    return("")
  }
  
  paste(
    as.character(x),
    collapse = ";"
  )
}


audit_path_for_csv <- function(
    audit
) {
  
  path <- audit$path
  
  list_columns <- c(
    "mis_positions",
    "mis_ids"
  )
  
  for (nm in list_columns) {
    
    if (nm %in% names(path)) {
      
      path[[nm]] <- vapply(
        path[[nm]],
        .audit_collapse_vector,
        character(1)
      )
    }
  }
  
  path
}


# ------------------------------------------------------------------------------
# Long observation-ID table
# ------------------------------------------------------------------------------

audit_ids_long <- function(
    audit
) {
  
  path <- audit$path
  
  rows <- list()
  z <- 1L
  
  for (i in seq_len(nrow(path))) {
    
    positions <- path$mis_positions[[i]]
    ids <- path$mis_ids[[i]]
    
    if (length(ids) == 0L) {
      next
    }
    
    rows[[z]] <- data.frame(
      study_id =
        path$study_id[i],
      
      estimand_id =
        path$estimand_id[i],
      
      target =
        path$target[i],
      
      k =
        path$k[i],
      
      removal_fraction =
        path$removal_fraction[i],
      
      direction =
        path$direction[i],
      
      position =
        as.integer(positions),
      
      observation_id =
        as.character(ids),
      
      stringsAsFactors = FALSE
    )
    
    z <- z + 1L
  }
  
  if (length(rows) == 0L) {
    
    return(
      data.frame(
        study_id = character(),
        estimand_id = character(),
        target = character(),
        k = integer(),
        removal_fraction = numeric(),
        direction = character(),
        position = integer(),
        observation_id = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  do.call(
    rbind,
    rows
  )
}

# ------------------------------------------------------------------------------
# LaTeX helpers
# ------------------------------------------------------------------------------

.audit_escape_latex <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  
  x
}


.audit_format_tex_value <- function(
    x,
    digits = 4
) {
  
  if (is.numeric(x)) {
    
    out <- ifelse(
      is.na(x),
      "",
      format(
        x,
        digits = digits,
        trim = TRUE,
        scientific = FALSE
      )
    )
    
    return(out)
  }
  
  x <- ifelse(
    is.na(x),
    "",
    as.character(x)
  )
  
  .audit_escape_latex(x)
}


audit_write_tex_table <- function(
    data,
    file,
    caption = NULL,
    label = NULL,
    digits = 4,
    font_size = "\\small"
) {
  
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame.",
      call. = FALSE
    )
  }
  
  
  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # --------------------------------------------------------------------------
  # Column alignment
  # --------------------------------------------------------------------------
  
  alignment <- vapply(
    data,
    function(x) {
      if (is.numeric(x)) "r" else "l"
    },
    character(1)
  )
  
  
  tabular_spec <- paste0(
    alignment,
    collapse = ""
  )
  
  
  # --------------------------------------------------------------------------
  # Header
  # --------------------------------------------------------------------------
  
  headers <- .audit_escape_latex(
    names(data)
  )
  
  
  header_line <- paste0(
    paste(
      headers,
      collapse = " & "
    ),
    " \\\\"
  )
  
  
  # --------------------------------------------------------------------------
  # Body
  # --------------------------------------------------------------------------
  
  body_lines <- vapply(
    seq_len(nrow(data)),
    function(i) {
      
      vals <- vapply(
        seq_along(data),
        function(j) {
          
          .audit_format_tex_value(
            data[[j]][i],
            digits = digits
          )
        },
        character(1)
      )
      
      
      paste0(
        paste(
          vals,
          collapse = " & "
        ),
        " \\\\"
      )
    },
    character(1)
  )
  
  
  # --------------------------------------------------------------------------
  # Complete flexible LaTeX float
  #
  # Intentionally:
  #   \begin{table}
  #
  # NOT:
  #   \begin{table}[H]
  #
  # and no other forced placement option.
  # --------------------------------------------------------------------------
  
  lines <- c(
    "\\begin{table}",
    "  \\centering",
    paste0(
      "  ",
      font_size
    )
  )
  
  
  if (!is.null(caption)) {
    
    lines <- c(
      lines,
      paste0(
        "  \\caption{",
        .audit_escape_latex(caption),
        "}"
      )
    )
  }
  
  
  if (!is.null(label)) {
    
    lines <- c(
      lines,
      paste0(
        "  \\label{",
        label,
        "}"
      )
    )
  }
  
  
  lines <- c(
    lines,
    
    paste0(
      "  \\begin{tabular}{",
      tabular_spec,
      "}"
    ),
    
    "    \\hline",
    
    paste0(
      "    ",
      header_line
    ),
    
    "    \\hline",
    
    paste0(
      "    ",
      body_lines
    ),
    
    "    \\hline",
    
    "  \\end{tabular}",
    
    "\\end{table}"
  )
  
  
  writeLines(
    lines,
    file,
    useBytes = TRUE
  )
  
  
  invisible(file)
}

# ------------------------------------------------------------------------------
# Save standardized outputs
# ------------------------------------------------------------------------------

save_mis_audit <- function(
    audit,
    output_dir,
    prefix = "audit",
    write_tex = TRUE
) {
  
  if (
    !inherits(
      audit,
      "mis_audit"
    )
  ) {
    stop(
      "`audit` must be an object returned by run_mis_audit().",
      call. = FALSE
    )
  }
  
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # --------------------------------------------------------------------------
  # Prepare output objects
  # --------------------------------------------------------------------------
  
  baseline_table <- audit$baseline
  
  path_table <- audit_path_for_csv(
    audit
  )
  
  summary_table <- audit$summary
  
  ids_table <- audit_ids_long(
    audit
  )
  
  
  # --------------------------------------------------------------------------
  # Full R object
  # --------------------------------------------------------------------------
  
  rds_file <- file.path(
    output_dir,
    paste0(
      prefix,
      ".rds"
    )
  )
  
  
  saveRDS(
    audit,
    rds_file
  )
  
  
  # --------------------------------------------------------------------------
  # Required CSV outputs
  # --------------------------------------------------------------------------
  
  baseline_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_baseline.csv"
    )
  )
  
  
  path_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_path.csv"
    )
  )
  
  
  summary_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_summary.csv"
    )
  )
  
  
  ids_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_influential_ids.csv"
    )
  )
  
  
  utils::write.csv(
    baseline_table,
    baseline_file,
    row.names = FALSE,
    na = ""
  )
  
  
  utils::write.csv(
    path_table,
    path_file,
    row.names = FALSE,
    na = ""
  )
  
  
  utils::write.csv(
    summary_table,
    summary_file,
    row.names = FALSE,
    na = ""
  )
  
  
  utils::write.csv(
    ids_table,
    ids_file,
    row.names = FALSE,
    na = ""
  )
  
  
  # --------------------------------------------------------------------------
  # Paper-facing LaTeX outputs
  #
  # No [H].
  # No [htbp].
  # Plain \begin{table} allows normal LaTeX float placement.
  # --------------------------------------------------------------------------
  
  baseline_tex_file <- NULL
  summary_tex_file <- NULL
  
  
  if (isTRUE(write_tex)) {
    
    baseline_tex_file <- file.path(
      output_dir,
      paste0(
        prefix,
        "_baseline.tex"
      )
    )
    
    
    summary_tex_file <- file.path(
      output_dir,
      paste0(
        prefix,
        "_summary.tex"
      )
    )
    
    
    audit_write_tex_table(
      data = baseline_table,
      file = baseline_tex_file,
      caption = NULL,
      label = NULL,
      digits = 4
    )
    
    
    audit_write_tex_table(
      data = summary_table,
      file = summary_tex_file,
      caption = NULL,
      label = NULL,
      digits = 4
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Return file locations
  # --------------------------------------------------------------------------
  
  invisible(
    list(
      rds = rds_file,
      
      baseline = baseline_file,
      path = path_file,
      summary = summary_file,
      influential_ids = ids_file,
      
      baseline_tex = baseline_tex_file,
      summary_tex = summary_tex_file
    )
  )
}