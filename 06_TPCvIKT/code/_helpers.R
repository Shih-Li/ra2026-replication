# Shared helpers for the Paper 6 R replication.

source(file.path(if (dir.exists(file.path(getwd(), "code"))) getwd() else dirname(getwd()), "code", "_config.R"), local = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

resolve_dta <- function(path) {
  candidates <- unique(c(path, paste0(path, ".dta"), sub("\\.dta$", "", path, ignore.case = TRUE)))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Required Stata file not found: ", path, call. = FALSE)
  hit[[1]]
}

read_dta_safe <- function(path) {
  x <- haven::read_dta(resolve_dta(path))
  # Stata value labels are metadata. Drop the haven_labelled wrapper at import
  # so ordinary numeric/string operations behave like Stata variables.
  x[] <- lapply(x, function(v) {
    if (inherits(v, "haven_labelled")) haven::zap_labels(v) else v
  })
  normalize_ids(x)
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) stop("Required CSV not found: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

normalize_ids <- function(df) {
  id_names <- intersect(
    c("id_1", "id_loc", "idloc", "cve_viv", "cveviv2", "cve_res", "pid", "pal_id"),
    names(df)
  )
  for (nm in id_names) {
    if (inherits(df[[nm]], "haven_labelled")) df[[nm]] <- haven::zap_labels(df[[nm]])
    if (!is.character(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    df[[nm]][is.na(df[[nm]])] <- ""
  }
  if ("etapa" %in% names(df)) {
    df$etapa <- suppressWarnings(as.numeric(haven::zap_labels(df$etapa)))
  }
  df
}

zap_all_labels <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
  })
  df
}

as_num <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))

row_total <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  z <- as.data.frame(lapply(df[cols], as_num))
  all_na <- rowSums(!is.na(z)) == 0
  out <- rowSums(z, na.rm = TRUE)
  out[all_na] <- NA_real_
  out
}

row_missing <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(0L, nrow(df)))
  rowSums(is.na(df[cols]))
}

row_max_stata <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  z <- as.data.frame(lapply(df[cols], as_num))
  all_na <- rowSums(!is.na(z)) == 0
  out <- apply(z, 1, max, na.rm = TRUE)
  out[all_na] <- NA_real_
  out
}

harmonize_keys <- function(x, y, by) {
  for (k in by) {
    if (!k %in% names(x) || !k %in% names(y)) stop("Join key missing: ", k, call. = FALSE)
    xn <- is.numeric(x[[k]]) || is.integer(x[[k]])
    yn <- is.numeric(y[[k]]) || is.integer(y[[k]])
    if (!(xn && yn)) {
      x[[k]] <- as.character(x[[k]])
      y[[k]] <- as.character(y[[k]])
      x[[k]][is.na(x[[k]])] <- ""
      y[[k]][is.na(y[[k]])] <- ""
    }
  }
  list(x = x, y = y)
}

stata_merge <- function(master, using, by, update = FALSE, replace = FALSE) {
  h <- harmonize_keys(master, using, by)
  master <- h$x; using <- h$y
  master$.__master__ <- TRUE
  using$.__using__ <- TRUE
  out <- dplyr::full_join(master, using, by = by, suffix = c(".m", ".u"), relationship = "many-to-many")
  
  overlaps <- intersect(setdiff(names(master), c(by, ".__master__")), setdiff(names(using), c(by, ".__using__")))
  coalesce_stata <- function(primary, secondary) {
    # Emulate Stata merge, update/replace semantics while handling the fact
    # that the same raw variable can occasionally arrive in R with different
    # storage types across survey modules (for example character vs double).
    # Stata string missing is exactly ""; whitespace is not missing.
    
    drop_label <- function(v) {
      if (inherits(v, "haven_labelled")) haven::zap_labels(v) else v
    }
    primary <- drop_label(primary)
    secondary <- drop_label(secondary)
    
    is_stata_missing_vec <- function(v) {
      if (is.character(v)) {
        n_bytes <- nchar(v, type = "bytes", allowNA = TRUE, keepNA = TRUE)
        is.na(v) | (!is.na(n_bytes) & n_bytes == 0L)
      } else {
        is.na(v)
      }
    }
    
    numeric_like <- function(v) {
      if (is.numeric(v) || is.integer(v) || is.logical(v)) return(TRUE)
      if (!is.character(v)) return(FALSE)
      miss <- is_stata_missing_vec(v)
      vals <- v[!miss]
      if (!length(vals)) return(TRUE)
      z <- suppressWarnings(as.numeric(vals))
      all(!is.na(z))
    }
    
    p_num <- numeric_like(primary)
    s_num <- numeric_like(secondary)
    
    if (p_num && s_num) {
      # If both sides are numerically representable, use a plain double.
      # Empty strings become NA before the Stata-style fill.
      p <- if (is.character(primary)) suppressWarnings(as.numeric(primary)) else as.numeric(primary)
      s <- if (is.character(secondary)) suppressWarnings(as.numeric(secondary)) else as.numeric(secondary)
      idx <- which(is.na(p) & !is.na(s))
      if (length(idx)) p[idx] <- s[idx]
      return(p)
    }
    
    # Otherwise preserve the information as strings. This also handles
    # genuinely textual variables and legacy-encoded byte strings safely.
    p <- as.character(primary)
    s <- as.character(secondary)
    p_miss <- is_stata_missing_vec(p)
    s_miss <- is_stata_missing_vec(s)
    idx <- which(p_miss & !s_miss)
    if (length(idx)) p[idx] <- s[idx]
    p[is.na(p)] <- ""
    p
  }
  
  for (nm in overlaps) {
    m <- paste0(nm, ".m"); u <- paste0(nm, ".u")
    if (replace) out[[nm]] <- coalesce_stata(out[[u]], out[[m]])
    else if (update) out[[nm]] <- coalesce_stata(out[[m]], out[[u]])
    else out[[nm]] <- out[[m]]
    out[[m]] <- NULL; out[[u]] <- NULL
  }
  out$._merge <- dplyr::case_when(
    !is.na(out$.__master__) & !is.na(out$.__using__) ~ 3L,
    !is.na(out$.__master__) ~ 1L,
    TRUE ~ 2L
  )
  out$.__master__ <- NULL; out$.__using__ <- NULL
  normalize_ids(out)
}

fill_within <- function(df, group, cols, order = NULL, direction = c("downup", "updown")) {
  direction <- match.arg(direction)
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(df)
  if (!is.null(order)) df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(c(group, order))))
  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group)))
  if (direction == "downup") {
    df <- tidyr::fill(df, dplyr::all_of(cols), .direction = "downup")
  } else {
    df <- tidyr::fill(df, dplyr::all_of(cols), .direction = "updown")
  }
  dplyr::ungroup(df)
}

rename_existing <- function(df, mapping) {
  mapping <- mapping[names(mapping) %in% names(df)]
  if (!length(mapping)) return(df)
  # mapping is old -> new
  for (old in names(mapping)) names(df)[names(df) == old] <- unname(mapping[[old]])
  df
}

drop_existing <- function(df, cols) dplyr::select(df, -dplyr::any_of(cols))

recode_missing <- function(x, values) {
  x <- as_num(x)
  x[x %in% values] <- NA_real_
  x
}

recode_binary_2_0 <- function(x, missing = c(9)) {
  x <- as_num(x)
  x[x %in% missing] <- NA_real_
  x[x == 2] <- 0
  x
}

mode_numeric <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

stata_pctile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p / 100, na.rm = TRUE, type = 2))
}

winsor_upper_by <- function(df, value, groups, p = 0.95) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(
      .cap = stats::quantile(.data[[value]], probs = p, na.rm = TRUE, names = FALSE, type = 2),
      "{value}" := dplyr::if_else(!is.na(.data[[value]]) & .data[[value]] > .cap, .cap, .data[[value]])
    ) |>
    dplyr::select(-.cap) |>
    dplyr::ungroup()
}

save_intermediate <- function(x, name) saveRDS(x, file.path(INTERMEDIATE_DIR, paste0(name, ".rds")))
save_processed <- function(x, name) saveRDS(x, file.path(PROCESSED_DIR, paste0(name, ".rds")))
load_intermediate <- function(name) readRDS(file.path(INTERMEDIATE_DIR, paste0(name, ".rds")))
load_processed <- function(name) readRDS(file.path(PROCESSED_DIR, paste0(name, ".rds")))

assert_raw_files <- function(paths) {
  missing <- paths[!vapply(paths, function(p) any(file.exists(c(p, paste0(p, ".dta")))), logical(1))]
  if (length(missing)) stop("Missing required source files:\n", paste(" -", missing, collapse = "\n"), call. = FALSE)
  invisible(TRUE)
}

cluster_formula <- function(cluster = "id_loc") stats::as.formula(paste0("~", cluster))

# Fit the Stata-style clustered regressions used throughout the replication.
# Important translation detail: Stata arithmetic such as x/0 produces missing
# (.) and reg automatically excludes observations missing any estimation
# variable. R instead produces Inf/-Inf, which can survive into fixest and make
# a clustered VCOV contain non-finite entries. Build the estimation sample
# explicitly so R follows Stata's missing-value behavior.
fit_clustered <- function(data, outcome, rhs, subset = rep(TRUE, nrow(data)), cluster = "id_loc", fe = NULL, no_intercept = FALSE) {
  if (!outcome %in% names(data)) stop("Outcome not found: ", outcome, call. = FALSE)
  if (!cluster %in% names(data)) stop("Cluster variable not found: ", cluster, call. = FALSE)
  
  d <- data[which(subset %in% TRUE), , drop = FALSE]
  rhs_txt <- if (length(rhs)) paste(rhs, collapse = " + ") else "1"
  if (no_intercept) rhs_txt <- paste0("0 + ", rhs_txt)
  fml <- if (is.null(fe)) {
    stats::as.formula(paste(outcome, "~", rhs_txt))
  } else {
    stats::as.formula(paste(outcome, "~", rhs_txt, "|", fe))
  }
  
  # Variables actually required by the formula, plus the cluster identifier.
  model_vars <- unique(c(all.vars(fml), cluster))
  missing_vars <- setdiff(model_vars, names(d))
  if (length(missing_vars)) {
    stop(
      "Regression variables not found for ", outcome, ": ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  
  keep <- rep(TRUE, nrow(d))
  for (nm in model_vars) {
    v <- d[[nm]]
    if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
    
    if (is.numeric(v) || is.integer(v)) {
      # Stata would treat results such as division by zero as missing rather
      # than +/-Inf, so non-finite numeric values cannot enter e(sample).
      keep <- keep & !is.na(v) & is.finite(as.numeric(v))
    } else if (is.logical(v)) {
      keep <- keep & !is.na(v)
    } else if (is.character(v)) {
      # normalize_ids() represents missing Stata IDs as "". Do not let a blank
      # ID become an artificial cluster or factor level.
      n_bytes <- nchar(v, type = "bytes", allowNA = TRUE, keepNA = TRUE)
      keep <- keep & !is.na(v) & !is.na(n_bytes) & n_bytes > 0L
    } else if (is.factor(v)) {
      keep <- keep & !is.na(v)
    } else {
      keep <- keep & !is.na(v)
    }
  }
  d <- d[which(keep %in% TRUE), , drop = FALSE]
  
  if (!nrow(d)) {
    stop("No usable observations remain for clustered regression: ", outcome, call. = FALSE)
  }
  
  cl <- d[[cluster]]
  if (inherits(cl, "haven_labelled")) cl <- haven::zap_labels(cl)
  n_clusters <- length(unique(cl[!is.na(cl)]))
  if (n_clusters < 2L) {
    stop(
      "Clustered regression for ", outcome,
      " has only ", n_clusters, " non-missing cluster(s) after Stata-style sample filtering.",
      call. = FALSE
    )
  }
  
  tryCatch(
    fixest::feols(
      fml,
      data = d,
      cluster = cluster_formula(cluster),
      warn = FALSE,
      notes = FALSE
    ),
    error = function(e) {
      stop(
        "Clustered regression failed for ", outcome,
        " (N = ", nrow(d), ", clusters = ", n_clusters, "). ",
        "Original fixest error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

wald_equal <- function(model, lhs, rhs) {
  if (!all(c(lhs, rhs) %in% names(stats::coef(model)))) return(NA_real_)
  unname(lincomb(model, stats::setNames(c(1, -1), c(lhs, rhs)))[["p"]])
}

lincomb <- function(model, weights) {
  b <- stats::coef(model)
  V <- stats::vcov(model)
  w <- rep(0, length(b)); names(w) <- names(b)
  for (nm in names(weights)) if (nm %in% names(w)) w[[nm]] <- weights[[nm]]
  est <- sum(w * b)
  se <- sqrt(as.numeric(t(w) %*% V %*% w))
  stat <- est / se
  df <- tryCatch(fixest::degrees_freedom(model, type = "t"), error = function(e) Inf)
  p <- if (is.finite(df)) 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE) else 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  c(estimate = est, se = se, p = p)
}

model_rows <- function(model, keep = NULL, extras = list(), model_name = NULL) {
  x <- broom::tidy(model, conf.int = FALSE)
  if (!is.null(keep)) x <- x[x$term %in% keep, , drop = FALSE]
  x$model <- model_name %||% "model"
  if (length(extras)) {
    for (nm in names(extras)) x[[nm]] <- extras[[nm]]
  }
  x
}

write_table_xlsx <- function(df, filename, sheet = "results", append = FALSE) {
  path <- file.path(OUTPUT_DIR, filename)
  if (append && file.exists(path)) {
    wb <- openxlsx::loadWorkbook(path)
    if (sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
  } else wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, df)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

append_table_sheet <- function(df, filename, sheet) write_table_xlsx(df, filename, sheet = sheet, append = TRUE)

# CDC 2000 z-score helper. This mirrors zanthro(..., US) with age-appropriate
# CDC LMS references in childsds. BMI is only defined from age 2 onward.
cdc_sds <- function(value, age_years, male, item) {
  sex <- ifelse(male == 1, "male", ifelse(male == 0, "female", NA_character_))
  out <- rep(NA_real_, length(value))
  if (item == "bmi") {
    idx <- !is.na(value) & !is.na(age_years) & age_years >= 2 & age_years < 20 & !is.na(sex)
    if (any(idx)) out[idx] <- childsds::sds(value[idx], age_years[idx], sex[idx], item = "bmi", ref = childsds::cdc.ref, male = "male", female = "female", type = "SDS")
  } else if (item == "weight") {
    idx0 <- !is.na(value) & !is.na(age_years) & age_years < 2 & !is.na(sex)
    idx2 <- !is.na(value) & !is.na(age_years) & age_years >= 2 & age_years < 20 & !is.na(sex)
    if (any(idx0)) out[idx0] <- childsds::sds(value[idx0], age_years[idx0], sex[idx0], item = "weight", ref = childsds::cdc.ref, male = "male", female = "female", type = "SDS")
    if (any(idx2)) out[idx2] <- childsds::sds(value[idx2], age_years[idx2], sex[idx2], item = "weight2_20", ref = childsds::cdc.ref, male = "male", female = "female", type = "SDS")
  } else if (item == "height") {
    idx0 <- !is.na(value) & !is.na(age_years) & age_years < 2 & !is.na(sex)
    idx2 <- !is.na(value) & !is.na(age_years) & age_years >= 2 & age_years < 20 & !is.na(sex)
    if (any(idx0)) out[idx0] <- childsds::sds(value[idx0], age_years[idx0], sex[idx0], item = "height0_3", ref = childsds::cdc.ref, male = "male", female = "female", type = "SDS")
    if (any(idx2)) out[idx2] <- childsds::sds(value[idx2], age_years[idx2], sex[idx2], item = "height2_20", ref = childsds::cdc.ref, male = "male", female = "female", type = "SDS")
  } else stop("Unknown CDC item: ", item)
  out
}

cdc_bmi_percentile <- function(bmi, age_years, male) {
  sex <- ifelse(male == 1, "male", ifelse(male == 0, "female", NA_character_))
  out <- rep(NA_real_, length(bmi))
  idx <- !is.na(bmi) & !is.na(age_years) & age_years >= 2 & age_years < 20 & !is.na(sex)
  if (any(idx)) out[idx] <- childsds::sds(bmi[idx], age_years[idx], sex[idx], item = "bmi", ref = childsds::cdc.ref, male = "male", female = "female", type = "perc")
  out
}

# Basic/conditional Lee (2009) bounds. tight_vars are discrete strata used to
# tighten the bounds, matching Stata leebounds ..., tight(...).
lee_bounds_once <- function(data, outcome, treatment, tight_vars = character()) {
  d <- data
  d$.__sel <- !is.na(d[[outcome]])
  d$.__t <- as.integer(d[[treatment]] == 1)
  if (!length(tight_vars)) d$.__stratum <- "all" else {
    d$.__stratum <- interaction(d[tight_vars], drop = TRUE, lex.order = TRUE)
  }
  strata <- split(d, d$.__stratum, drop = TRUE)
  pieces <- lapply(strata, function(s) {
    n1 <- sum(s$.__t == 1); n0 <- sum(s$.__t == 0)
    if (!n1 || !n0) return(NULL)
    p1 <- mean(s$.__sel[s$.__t == 1]); p0 <- mean(s$.__sel[s$.__t == 0])
    y1 <- s[[outcome]][s$.__t == 1 & s$.__sel]
    y0 <- s[[outcome]][s$.__t == 0 & s$.__sel]
    if (!length(y1) || !length(y0) || p1 == 0 || p0 == 0) return(NULL)
    if (p1 >= p0) {
      keep_share <- p0 / p1
      qlo <- stats::quantile(y1, probs = 1 - keep_share, na.rm = TRUE, names = FALSE, type = 2)
      qhi <- stats::quantile(y1, probs = keep_share, na.rm = TRUE, names = FALSE, type = 2)
      mu1_lo <- mean(y1[y1 <= qhi], na.rm = TRUE)
      mu1_hi <- mean(y1[y1 >= qlo], na.rm = TRUE)
      lo <- mu1_lo - mean(y0, na.rm = TRUE)
      hi <- mu1_hi - mean(y0, na.rm = TRUE)
    } else {
      keep_share <- p1 / p0
      qlo <- stats::quantile(y0, probs = 1 - keep_share, na.rm = TRUE, names = FALSE, type = 2)
      qhi <- stats::quantile(y0, probs = keep_share, na.rm = TRUE, names = FALSE, type = 2)
      mu0_lo <- mean(y0[y0 <= qhi], na.rm = TRUE)
      mu0_hi <- mean(y0[y0 >= qlo], na.rm = TRUE)
      lo <- mean(y1, na.rm = TRUE) - mu0_hi
      hi <- mean(y1, na.rm = TRUE) - mu0_lo
    }
    weight <- nrow(s) * min(p1, p0)
    c(lower = lo, upper = hi, weight = weight)
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(c(lower = NA_real_, upper = NA_real_))
  m <- do.call(rbind, pieces)
  c(lower = stats::weighted.mean(m[, "lower"], m[, "weight"]), upper = stats::weighted.mean(m[, "upper"], m[, "weight"]))
}

lee_bounds_cluster_boot <- function(data, outcome, treatment, cluster = "id_loc", tight_vars = character(), reps = 500, seed = 2012) {
  point <- lee_bounds_once(data, outcome, treatment, tight_vars)
  set.seed(seed)
  ids <- unique(data[[cluster]])
  boots <- replicate(reps, {
    draw <- sample(ids, length(ids), replace = TRUE)
    chunks <- lapply(seq_along(draw), function(i) {
      z <- data[data[[cluster]] == draw[[i]], , drop = FALSE]
      z[[cluster]] <- paste0(z[[cluster]], "__", i)
      z
    })
    b <- dplyr::bind_rows(chunks)
    lee_bounds_once(b, outcome, treatment, tight_vars)
  })
  se <- apply(boots, 1, stats::sd, na.rm = TRUE)
  data.frame(bound = c("lower", "upper"), estimate = unname(point), std.error = unname(se), reps = reps)
}
