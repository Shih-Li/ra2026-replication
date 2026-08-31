# ==============================================================================
# audit/function/audit_load_mis.R
#
# Purpose:
#   Load the MIS search engine from the separate mis_project_m repository
#   without assuming that the two repositories are adjacent on disk.
#
# Required files in mis_project_m:
#   R/dinkelbach_topk.R
#   R/fast_sens_topk.R
#
# Recommended local configuration:
#   Set MIS_PROJECT_M_ROOT in ~/.Renviron, for example:
#
#     MIS_PROJECT_M_ROOT=D:/Research/mis_project_m
#
# Then each audit script only needs:
#
#   load_mis_engine()
# ==============================================================================


load_mis_engine <- function(
    mis_root = Sys.getenv("MIS_PROJECT_M_ROOT"),
    force = FALSE,
    verbose = TRUE
) {
  
  required_functions <- c(
    "dinkelbach_topk_lm",
    "fast_sens_topk"
  )
  
  already_loaded <- vapply(
    required_functions,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )
  
  if (all(already_loaded) && !isTRUE(force)) {
    
    if (isTRUE(verbose)) {
      message("MIS engine already loaded.")
    }
    
    return(
      invisible(
        list(
          root = NA_character_,
          loaded = required_functions,
          reused = TRUE
        )
      )
    )
  }
  
  if (
    !is.character(mis_root) ||
    length(mis_root) != 1L ||
    is.na(mis_root) ||
    !nzchar(mis_root)
  ) {
    stop(
      paste0(
        "MIS project location is not configured.\n",
        "Set environment variable MIS_PROJECT_M_ROOT to the local ",
        "mis_project_m directory, or call load_mis_engine(mis_root = ...)."
      ),
      call. = FALSE
    )
  }
  
  mis_root <- path.expand(mis_root)
  
  if (!dir.exists(mis_root)) {
    stop(
      "MIS_PROJECT_M_ROOT does not exist: ",
      mis_root,
      call. = FALSE
    )
  }
  
  mis_root <- normalizePath(
    mis_root,
    winslash = "/",
    mustWork = TRUE
  )
  
  required_files <- file.path(
    mis_root,
    "R",
    c(
      "dinkelbach_topk.R",
      "fast_sens_topk.R"
    )
  )
  
  missing_files <- required_files[
    !file.exists(required_files)
  ]
  
  if (length(missing_files) > 0L) {
    stop(
      paste0(
        "Missing required MIS file(s):\n",
        paste(
          paste0("  - ", missing_files),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }
  
  # Source into the global analysis session so all dependencies defined in
  # the two MIS files are visible to the audit scripts.
  for (f in required_files) {
    
    if (isTRUE(verbose)) {
      message("Sourcing MIS engine: ", f)
    }
    
    sys.source(
      f,
      envir = .GlobalEnv
    )
  }
  
  loaded <- vapply(
    required_functions,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )
  
  if (!all(loaded)) {
    stop(
      "MIS files were sourced but required function(s) are still missing: ",
      paste(
        required_functions[!loaded],
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (isTRUE(verbose)) {
    message(
      "MIS engine loaded from: ",
      mis_root
    )
  }
  
  invisible(
    list(
      root = mis_root,
      loaded = required_functions,
      reused = FALSE
    )
  )
}