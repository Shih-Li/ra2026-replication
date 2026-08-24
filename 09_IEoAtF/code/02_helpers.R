# 02_helpers.R
# Shared helpers for the Cai-Szeidl R translation.

if (!exists("paths")) source("code/01_setup.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

read_stata <- function(path) {
  x <- haven::read_dta(path)
  x[] <- lapply(x, haven::zap_labels)
  as.data.frame(x)
}

require_cols <- function(data, cols, context = "data") {
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop("Missing column(s) in ", context, ": ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

existing_vars <- function(data, vars) intersect(vars, names(data))

pattern_vars <- function(data, pattern) grep(pattern, names(data), value = TRUE)

stata_estimation_n <- function(data, vars) {
  vars <- unique(vars[!is.na(vars) & nzchar(vars)])
  vars <- intersect(vars, names(data))
  if (!length(vars)) return(nrow(data))
  sum(stats::complete.cases(data[, vars, drop = FALSE]))
}


quote_formula_name <- function(x) {
  # Stata commonly creates names such as _Ibasecount_2. They are valid
  # data-frame column names but not bare R formula symbols. Quote any
  # non-syntactic name with backticks before constructing a formula.
  x <- as.character(x)
  syntactic <- make.names(x) == x & !grepl("^[.][0-9]", x)
  out <- x
  out[!syntactic] <- paste0("`", gsub("`", "\\`", x[!syntactic], fixed = TRUE), "`")
  out
}

rhs_string <- function(terms) {
  terms <- unique(terms[!is.na(terms) & nzchar(terms)])
  if (!length(terms)) "1" else paste(quote_formula_name(terms), collapse = " + ")
}

make_fe_formula <- function(outcome, terms, fe = NULL) {
  base <- paste(quote_formula_name(outcome), "~", rhs_string(terms))
  if (!is.null(fe) && nzchar(fe)) base <- paste(base, "|", quote_formula_name(fe))
  stats::as.formula(base)
}

fit_ols <- function(data, outcome, terms, subset = NULL, cluster = NULL,
                    fe = NULL, weights = NULL) {
  d <- data
  if (!is.null(subset)) d <- d[which(subset %in% TRUE), , drop = FALSE]
  f <- make_fe_formula(outcome, terms, fe = fe)
  args <- list(fml = f, data = d, notes = FALSE, warn = FALSE)
  if (!is.null(weights)) {
    args$weights <- stats::as.formula(paste0("~", quote_formula_name(weights)))
  }
  fit <- do.call(fixest::feols, args)
  
  # fixest removes singleton fixed-effect groups by default. Stata xtreg, fe
  # still reports those complete observations in e(N). Keep the fixest fit
  # (which matches the coefficients/SEs well) but store a Stata-style report N.
  report_vars <- c(outcome, terms, fe, weights, cluster)
  cc_vars <- intersect(unique(report_vars), names(d))
  cc <- if (length(cc_vars)) stats::complete.cases(d[, cc_vars, drop = FALSE]) else rep(TRUE, nrow(d))
  attr(fit, "ra_report_n") <- sum(cc)
  
  if (!is.null(cluster)) {
    attr(fit, "ra_vcov") <- stats::as.formula(paste0("~", quote_formula_name(cluster)))
    attr(fit, "ra_cluster_name") <- cluster
    attr(fit, "ra_nclusters") <- length(unique(d[[cluster]][cc & !is.na(d[[cluster]])]))
  } else {
    attr(fit, "ra_vcov") <- "iid"
  }
  fit
}

fit_iv <- function(data, outcome, exogenous, endogenous, instruments,
                   subset = NULL, fe = "firmid", cluster = NULL) {
  d <- data
  if (!is.null(subset)) d <- d[which(subset %in% TRUE), , drop = FALSE]
  f <- paste(
    quote_formula_name(outcome), "~", rhs_string(exogenous),
    "|", quote_formula_name(fe),
    "|", rhs_string(endogenous), "~", rhs_string(instruments)
  )
  fit <- fixest::feols(stats::as.formula(f), data = d, notes = FALSE, warn = FALSE)
  # fixest names second-stage IV coefficients fit_<endogenous>. Keep the
  # endogenous-variable list so tidy_model() can report the original Stata
  # coefficient names (loanuse, comploanuse, etc.).
  attr(fit, "ra_iv_endogenous") <- endogenous
  
  report_vars <- c(outcome, exogenous, endogenous, instruments, fe, cluster)
  cc_vars <- intersect(unique(report_vars), names(d))
  cc <- if (length(cc_vars)) stats::complete.cases(d[, cc_vars, drop = FALSE]) else rep(TRUE, nrow(d))
  attr(fit, "ra_report_n") <- sum(cc)
  
  attr(fit, "ra_vcov") <- if (is.null(cluster)) {
    "iid"
  } else {
    attr(fit, "ra_cluster_name") <- cluster
    attr(fit, "ra_nclusters") <- length(unique(d[[cluster]][cc & !is.na(d[[cluster]])]))
    stats::as.formula(paste0("~", quote_formula_name(cluster)))
  }
  fit
}

model_vcov <- function(model) {
  v <- attr(model, "ra_vcov") %||% "iid"
  stats::vcov(model, vcov = v)
}

model_coeftable <- function(model) {
  v <- attr(model, "ra_vcov") %||% "iid"
  as.data.frame(summary(model, vcov = v)$coeftable, stringsAsFactors = FALSE)
}

tidy_model <- function(model, terms = NULL, model_id = NA_character_,
                       outcome = NA_character_, extras = list()) {
  ct <- model_coeftable(model)
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  names(ct)[1:4] <- c("estimate", "std.error", "statistic", "p.value")
  
  # fixest prefixes fitted endogenous regressors in IV second stages with
  # "fit_". Translate only the endogenous variables recorded by fit_iv()
  # back to their source names so Table 9 matches Stata's xtivreg output.
  iv_endo <- attr(model, "ra_iv_endogenous")
  if (!is.null(iv_endo) && length(iv_endo)) {
    for (v in iv_endo) {
      ct$term[ct$term == paste0("fit_", v)] <- v
    }
  }
  
  if (!is.null(terms)) ct <- ct[ct$term %in% terms, , drop = FALSE]
  
  # rep(..., nrow(ct)) is deliberate: assigning a scalar to a zero-row data
  # frame throws an error and obscures the underlying missing-term problem.
  ct$model <- rep(model_id, nrow(ct))
  ct$outcome <- rep(outcome, nrow(ct))
  ct$n <- rep(attr(model, "ra_report_n") %||% stats::nobs(model), nrow(ct))
  for (nm in names(extras)) ct[[nm]] <- rep(extras[[nm]], nrow(ct))
  ct[, c("model", "outcome", "term", "estimate", "std.error", "statistic", "p.value", "n",
         setdiff(names(extras), c("model", "outcome", "term", "estimate", "std.error",
                                  "statistic", "p.value", "n"))), drop = FALSE]
}

coef_stat <- function(model, term) {
  x <- tidy_model(model, terms = term)
  if (!nrow(x)) return(c(estimate = NA_real_, se = NA_real_, p = NA_real_))
  c(estimate = x$estimate[1], se = x$std.error[1], p = x$p.value[1])
}

joint_wald_p <- function(model, terms) {
  b <- stats::coef(model)
  keep <- intersect(terms, names(b))
  if (!length(keep)) return(NA_real_)
  V <- model_vcov(model)[keep, keep, drop = FALSE]
  bb <- b[keep]
  inv <- try(solve(V), silent = TRUE)
  if (inherits(inv, "try-error")) return(NA_real_)
  q <- length(keep)
  stat <- as.numeric(t(bb) %*% inv %*% bb) / q
  v <- attr(model, "ra_vcov")
  if (inherits(v, "formula")) {
    # Stata's test after vce(cluster clustvar) reports an F test with
    # denominator df equal to the number of estimation clusters minus one.
    g <- attr(model, "ra_nclusters")
    if (is.finite(g) && g > 1) {
      p <- stats::pf(stat, df1 = q, df2 = g - 1, lower.tail = FALSE)
    } else {
      p <- stats::pchisq(stat * q, df = q, lower.tail = FALSE)
    }
  } else {
    p <- stats::pf(stat, df1 = q, df2 = max(stats::df.residual(model), 1), lower.tail = FALSE)
  }
  p
}

joint_wald_stat <- function(model, terms) {
  b <- stats::coef(model)
  keep <- intersect(terms, names(b))
  if (!length(keep)) return(NA_real_)
  V <- model_vcov(model)[keep, keep, drop = FALSE]
  inv <- try(solve(V), silent = TRUE)
  if (inherits(inv, "try-error")) return(NA_real_)
  bb <- b[keep]
  as.numeric(t(bb) %*% inv %*% bb) / length(keep)
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

stata_mdev <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  m <- mean(x)
  mean(abs(x - m))
}

write_table <- function(df, stem, caption = NULL) {
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  base <- paste0(paths$result_prefix, stem)
  utils::write.csv(df, file.path(paths$tables, paste0(base, ".csv")),
                   row.names = FALSE, na = "")
  tex <- knitr::kable(df, format = "latex", booktabs = TRUE, caption = caption, digits = 4)
  writeLines(tex, file.path(paths$tables, paste0(base, ".tex")), useBytes = TRUE)
  invisible(df)
}

save_figure <- function(plot, stem, width, height) {
  ggplot2::ggsave(
    filename = file.path(paths$figures, paste0(paths$result_prefix, stem, ".pdf")),
    plot = plot, width = width, height = height, units = "in"
  )
  invisible(plot)
}

control_mean <- function(data, outcome, mask) {
  safe_mean(data[[outcome]][which(mask %in% TRUE)])
}

model_rows <- function(model, keep_terms, model_id, outcome, control = NA_real_,
                       fixed_effects = NA_character_, notes = NA_character_) {
  x <- tidy_model(model, keep_terms, model_id, outcome)
  if (!nrow(x) && length(keep_terms)) {
    available <- tidy_model(model)$term
    stop(
      "No requested coefficient(s) found for ", model_id,
      ". Requested: ", paste(keep_terms, collapse = ", "),
      ". Available: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  x$control_mean <- rep(control, nrow(x))
  x$fixed_effects <- rep(fixed_effects, nrow(x))
  x$notes <- rep(notes, nrow(x))
  x
}

# Cluster bootstrap sample. Duplicated sampled clusters receive new cluster ids;
# panel ids are also relabelled so duplicated towns do not share fixed effects.
cluster_resample <- function(data, cluster, panel = NULL) {
  ids <- unique(data[[cluster]][!is.na(data[[cluster]])])
  draws <- sample(ids, length(ids), replace = TRUE)
  pieces <- vector("list", length(draws))
  for (i in seq_along(draws)) {
    z <- data[data[[cluster]] == draws[i], , drop = FALSE]
    z$.boot_cluster <- i
    if (!is.null(panel)) z$.boot_panel <- interaction(i, z[[panel]], drop = TRUE)
    pieces[[i]] <- z
  }
  dplyr::bind_rows(pieces)
}

# rwolf2 in the authors' code specifies cluster(survey_town) but not
# idcluster(). Thus resampling duplicates towns while the model syntax keeps
# the original survey_town and firmid identifiers.
rwolf2_cluster_resample <- function(data, cluster) {
  ids <- unique(data[[cluster]][!is.na(data[[cluster]])])
  draws <- sample(ids, length(ids), replace = TRUE)
  dplyr::bind_rows(lapply(draws, function(id) {
    data[data[[cluster]] == id, , drop = FALSE]
  }))
}

fit_spec <- function(data, spec, boot = FALSE) {
  cl <- if (boot) ".boot_cluster" else spec$cluster
  fe <- spec$fe
  if (boot && !is.null(fe) && identical(fe, spec$panel %||% "")) fe <- ".boot_panel"
  fit_ols(
    data = data,
    outcome = spec$outcome,
    terms = spec$rhs,
    subset = spec$subset_fun(data),
    cluster = cl,
    fe = fe,
    weights = spec$weights %||% NULL
  )
}

# Romano-Wolf stepdown analogue for the exact family of coefficients requested.
# It uses a cluster bootstrap, studentized centered bootstrap statistics, and
# the max-t stepdown rule. This mirrors the purpose of Stata rwolf2 while
# remaining transparent in R.
romano_wolf_cluster <- function(data, specs, reps = getOption("ra2026.rwolf_reps", 1000L),
                                seed = 123L, cluster = "survey_town",
                                panel = "firmid") {
  original_models <- lapply(specs, function(s) fit_spec(data, s, boot = FALSE))
  hyp <- do.call(rbind, lapply(seq_along(specs), function(i) {
    do.call(rbind, lapply(specs[[i]]$test_terms, function(term) {
      st <- coef_stat(original_models[[i]], term)
      data.frame(
        spec_index = i, model = specs[[i]]$id, outcome = specs[[i]]$outcome,
        term = term, estimate = st["estimate"], se = st["se"],
        obs_t = abs(st["estimate"] / st["se"]), stringsAsFactors = FALSE
      )
    }))
  }))
  hyp$hypothesis <- seq_len(nrow(hyp))
  B <- matrix(NA_real_, nrow = reps, ncol = nrow(hyp))
  
  set.seed(seed)
  for (b in seq_len(reps)) {
    bd <- rwolf2_cluster_resample(data, cluster = cluster)
    for (i in seq_along(specs)) {
      fitb <- try(fit_spec(bd, specs[[i]], boot = FALSE), silent = TRUE)
      if (inherits(fitb, "try-error")) next
      rows <- which(hyp$spec_index == i)
      for (j in rows) {
        st <- coef_stat(fitb, hyp$term[j])
        if (is.finite(st["se"]) && st["se"] > 0) {
          B[b, j] <- abs((st["estimate"] - hyp$estimate[j]) / st["se"])
        }
      }
    }
  }
  
  ok <- is.finite(hyp$obs_t)
  adj <- rep(NA_real_, nrow(hyp))
  if (any(ok)) {
    idx <- which(ok)
    ord <- idx[order(hyp$obs_t[idx], decreasing = TRUE)]
    raw_step <- rep(NA_real_, length(ord))
    for (k in seq_along(ord)) {
      remaining <- ord[k:length(ord)]
      mx <- apply(B[, remaining, drop = FALSE], 1, function(z) {
        z <- z[is.finite(z)]
        if (!length(z)) NA_real_ else max(z)
      })
      valid <- is.finite(mx)
      raw_step[k] <- if (any(valid)) (1 + sum(mx[valid] >= hyp$obs_t[ord[k]])) / (1 + sum(valid)) else NA_real_
    }
    adj_sorted <- cummax(replace(raw_step, is.na(raw_step), 0))
    adj[ord] <- adj_sorted
  }
  hyp$rw_p.value <- adj
  hyp$obs_t <- NULL
  hyp
}

prepare_loanmain <- function() {
  d <- read_stata(paths$loanmain)
  require_cols(d, c("firmid", "survey_town", "round", "type", "survey_town_type"), "loanmain.dta")
  
  d <- d |>
    dplyr::group_by(.data$survey_town, .data$round) |>
    dplyr::mutate(
      marketsize = dplyr::n(),
      townsize = dplyr::n(),
      sumtreated = sum(.data$type, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  if ("labor" %in% names(d)) {
    base_avg <- d |>
      dplyr::filter(.data$round == 1) |>
      dplyr::group_by(.data$survey_town) |>
      dplyr::summarise(avgnumlabor = mean(.data$labor, na.rm = TRUE), .groups = "drop")
    d <- dplyr::left_join(d, base_avg, by = "survey_town")
  }
  
  if (all(c("industry", "part5revenue") %in% names(d))) {
    d <- d |>
      dplyr::group_by(.data$survey_town, .data$industry, .data$round) |>
      dplyr::mutate(
        indsize = dplyr::n(),
        indsalessum = sum(.data$part5revenue, na.rm = TRUE),
        marketshare = .data$part5revenue / (2 * .data$indsalessum),
        herf = 2 * sum(.data$marketshare^2, na.rm = TRUE),
        invherf = 1 / .data$herf
      ) |>
      dplyr::ungroup()
  }
  
  d$treatratio_peer <- with(d, (sumtreated - type) / (townsize - 1))
  d$inter_untpeer <- with(d, treatratio_peer * (1 - type))
  
  if ("lnpart5revenue" %in% names(d)) {
    d <- d |>
      dplyr::group_by(.data$firmid) |>
      dplyr::arrange(.data$round, .by_group = TRUE) |>
      dplyr::mutate(
        .baseline_sales = {
          z <- .data$lnpart5revenue[.data$round == 1]
          if (length(z)) z[1] else NA_real_
        },
        growthsales = dplyr::if_else(.data$round > 1, .data$lnpart5revenue - .data$.baseline_sales, NA_real_),
        avggrowthsales = {
          z <- .data$growthsales[.data$round > 1]
          m <- if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
          ifelse(.data$round > 1, m, NA_real_)
        },
        baselnsales = .data$.baseline_sales
      ) |>
      dplyr::ungroup()
    d$.baseline_sales <- NULL
  }
  
  for (pair in list(c("total_profit", "baseprofit"), c("lnlabor", "baselnlabor"))) {
    if (pair[1] %in% names(d)) {
      var <- pair[1]; out <- pair[2]
      tmp <- d |>
        dplyr::group_by(.data$firmid) |>
        dplyr::arrange(.data$round, .by_group = TRUE) |>
        dplyr::mutate(.base_tmp = {
          z <- .data[[var]][.data$round == 1]
          if (length(z)) z[1] else NA_real_
        }) |>
        dplyr::ungroup()
      d[[out]] <- tmp$.base_tmp
    }
  }
  
  if (all(c("post", "treatratio_comp") %in% names(d))) {
    d$interpost <- d$post * d$type
    d$inter4post <- d$post * d$treatratio_comp
    d$interpost_comp1 <- d$post * d$treatratio_comp * d$type
    d$interpost_comp2 <- d$post * d$treatratio_comp * (1 - d$type)
  }
  
  if ("category" %in% names(d)) {
    # Stata:
    #   gen categoryi = 0
    #   replace categoryi = 1 if category == i
    # Therefore missing category values remain zero, not NA.
    for (i in 1:8) {
      z <- integer(nrow(d))
      z[!is.na(d$category) & d$category == i] <- 1L
      d[[paste0("category", i)]] <- z
    }
  }
  
  if ("survey_town_type" %in% names(d) && "post" %in% names(d)) {
    d$market50 <- as.integer(d$survey_town_type == 1)
    d$market80 <- as.integer(d$survey_town_type == 2)
    d$market50T <- d$market50 * d$type
    d$market80T <- d$market80 * d$type
    d$postmarket50 <- d$post * d$market50
    d$postmarket80 <- d$post * d$market80
    d$postmarket50T <- d$post * d$market50T
    d$postmarket80T <- d$post * d$market80T
  }
  
  if ("treatratio_comp" %in% names(d) && "post" %in% names(d)) {
    d$bin1 <- as.integer(!is.na(d$treatratio_comp) & d$treatratio_comp == 0)
    d$bin2 <- as.integer(!is.na(d$treatratio_comp) & d$treatratio_comp > 0 & d$treatratio_comp < 0.51)
    d$bin3 <- as.integer(!is.na(d$treatratio_comp) & d$treatratio_comp > 0.5 & d$treatratio_comp < 0.77)
    d$bin4 <- as.integer(!is.na(d$treatratio_comp) & d$treatratio_comp > 0.77)
    for (i in 1:4) {
      b <- d[[paste0("bin", i)]] == 1
      d[[paste0("mtreatcomp", i)]] <- ifelse(b, safe_mean(d$treatratio_comp[b]), NA_real_)
      d[[paste0("interpost_bin", i)]] <- d$post * d[[paste0("bin", i)]]
    }
  }
  
  for (v in existing_vars(d, c("part5revenue", "total_profit", "labor"))) {
    d[[paste0("asinh", v)]] <- asinh(d[[v]])
    shut <- paste0("shut_", v)
    if (shut %in% names(d)) d[[paste0("asinhshut_", v)]] <- asinh(d[[shut]])
  }
  
  d$midline <- as.integer(d$round == 2)
  d$endline <- as.integer(d$round == 3)
  if ("treatratio_comp" %in% names(d)) {
    d$interpost_round11 <- d$type * d$endline
    d$interpost_round12 <- d$treatratio_comp * d$endline
    d$interpost_round21 <- d$type * d$midline
    d$interpost_round22 <- d$treatratio_comp * d$midline
  }
  
  # Consumer z-scores in the Stata code use egen mdev(), i.e. mean absolute deviation.
  for (v in existing_vars(d, c("coneval_squality", "coneval_env", "coneval_money", "coneval_all",
                               "finknowledge", "difficultyloan"))) {
    ref <- d$type == 0 & d$round == 3
    m <- safe_mean(d[[v]][ref])
    s <- stata_mdev(d[[v]][ref])
    d[[paste0("z", v)]] <- ifelse(d$round == 3, (d[[v]] - m) / s, NA_real_)
  }
  
  local_dummies <- existing_vars(d, c("dummy_lcomp", "dummy_lnoncomp", "dummy_nonlcomp", "dummy_nonlnoncomp"))
  if (length(local_dummies) == 4 && "post" %in% names(d)) {
    d$dummy_allfour <- as.integer(rowSums(d[, local_dummies, drop = FALSE] == 1, na.rm = TRUE) > 0)
    d$pldummy1 <- d$post * d$dummy_lcomp
    d$pldummy2 <- d$post * d$dummy_lnoncomp
    d$pldummy3 <- d$post * d$dummy_nonlcomp
    d$pldummy4 <- d$post * d$dummy_nonlnoncomp
  }
  
  if ("lcompnum" %in% names(d) && "post" %in% names(d)) {
    for (i in 0:4) {
      d[[paste0("dlcompnum", i)]] <- as.integer(d$lcompnum == i)
      d[[paste0("interdlcompnum", i)]] <- d$post * d[[paste0("dlcompnum", i)]]
    }
  }
  
  if (all(c("treatratio_lnoncomp", "survey_town", "industry", "indsize", "post") %in% names(d))) {
    d$highnoncomp <- as.integer(!is.na(d$treatratio_lnoncomp) & d$treatratio_lnoncomp != 0)
    d <- d |>
      dplyr::group_by(.data$survey_town, .data$industry) |>
      dplyr::mutate(highnoncompsum = sum(.data$highnoncomp, na.rm = TRUE)) |>
      dplyr::ungroup()
    d$highnoncompshare <- with(d, (highnoncompsum / 3 - highnoncomp) / (indsize - 1))
    d$highnoncompshare[is.na(d$highnoncompshare)] <- 0
  }
  
  local_map <- c(
    localinter1 = "treatratio_lcomp",
    localinter2 = "treatratio_lnoncomp",
    localinter5 = "treatratio_nonlcomp",
    localinter6 = "treatratio_nonlnoncomp",
    localinter10 = "highnoncompshare",
    localinter13 = "indsize"
  )
  if ("post" %in% names(d)) {
    for (nm in names(local_map)) {
      v <- local_map[[nm]]
      if (v %in% names(d)) d[[nm]] <- d$post * d[[v]]
    }
  }
  
  if (all(c("newloan", "type", "survey_town_type", "round") %in% names(d))) {
    d$loanuse <- 0
    d$loanuse[d$type == 1 & d$newloan == 1 & d$round > 1] <- 1
    d$loanuse[d$type == 0 & d$newloan == 1 & d$survey_town_type != 0 & d$round > 2] <- 1
    if (all(c("survey_town", "industry", "indsize") %in% names(d))) {
      d <- d |>
        dplyr::group_by(.data$survey_town, .data$industry, .data$round) |>
        dplyr::mutate(loanusesum = sum(.data$loanuse, na.rm = TRUE)) |>
        dplyr::ungroup()
      d$comploanuse <- with(d, (loanusesum - loanuse) / (indsize - 1))
      d$comploanuse[is.na(d$comploanuse)] <- 0
    }
  }
  
  d
}

prepare_market <- function() read_stata(paths$market)

baseline_controls <- function(data) {
  existing_vars(data, c("firmage", "retail", "labor", "total_profit", "part5revenue",
                        "gender", "age", "education_college", "polconnection",
                        "bankloan", "num_clients", "num_supplier"))
}

endline_controls <- function(data) {
  c(
    existing_vars(data, "baselabor"),
    pattern_vars(data, "^_Ibasecount"),
    pattern_vars(data, "^category[1-8]$")
  )
}