# ==============================================================================
# audit/function/audit_output.R
#
# Purpose:
#   Save standardized MIS audit outputs.
#
# Public functions:
#   audit_ids_long()
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
    "mis_ids",
    "greedy_positions",
    "greedy_ids"
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
    
    methods <- c(
      "MIS",
      "Greedy"
    )
    
    for (method in methods) {
      
      if (method == "MIS") {
        
        positions <-
          path$mis_positions[[i]]
        
        ids <-
          path$mis_ids[[i]]
        
      } else {
        
        positions <-
          path$greedy_positions[[i]]
        
        ids <-
          path$greedy_ids[[i]]
      }
      
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
        
        direction =
          path$direction[i],
        
        method =
          method,
        
        position =
          as.integer(
            positions
          ),
        
        observation_id =
          as.character(
            ids
          ),
        
        stringsAsFactors = FALSE
      )
      
      z <- z + 1L
    }
  }
  
  if (length(rows) == 0L) {
    
    return(
      data.frame(
        study_id = character(),
        estimand_id = character(),
        target = character(),
        k = integer(),
        direction = character(),
        method = character(),
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
# Save standardized outputs
# ------------------------------------------------------------------------------

save_mis_audit <- function(
    audit,
    output_dir,
    prefix = "audit"
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
  
  
  # Full R object
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
  
  
  # Validation / baseline
  baseline_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_baseline.csv"
    )
  )
  
  utils::write.csv(
    audit$baseline,
    baseline_file,
    row.names = FALSE,
    na = ""
  )
  
  
  # Full k path
  path_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_path.csv"
    )
  )
  
  utils::write.csv(
    audit_path_for_csv(
      audit
    ),
    path_file,
    row.names = FALSE,
    na = ""
  )
  
  
  # One-row study summary
  summary_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_summary.csv"
    )
  )
  
  utils::write.csv(
    audit$summary,
    summary_file,
    row.names = FALSE,
    na = ""
  )
  
  
  # Long-form observation IDs
  ids_file <- file.path(
    output_dir,
    paste0(
      prefix,
      "_influential_ids.csv"
    )
  )
  
  utils::write.csv(
    audit_ids_long(
      audit
    ),
    ids_file,
    row.names = FALSE,
    na = ""
  )
  
  
  invisible(
    list(
      rds = rds_file,
      baseline = baseline_file,
      path = path_file,
      summary = summary_file,
      influential_ids = ids_file
    )
  )
}