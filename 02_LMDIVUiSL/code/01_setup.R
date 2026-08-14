# ============================================================
# 01_setup.R
# Common setup and helpers for replication target 02
# Meriggi et al., "Last-mile delivery increases vaccine uptake in Sierra Leone"
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(420)

required_packages <- c(
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "fixest", "fwildclusterboot", "dqrng", "knitr",
  "writexl", "nnet"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install required packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))\n",
    "Note: fwildclusterboot may be installed from the maintainer's R-universe if unavailable on CRAN."
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(ggplot2)
  library(fixest)
})

ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
DATA_DIR <- file.path(ROOT, "data", "source", "cleaned")
OUT_DIR <- file.path(ROOT, "output")
OUT_MAIN_FIG <- file.path(OUT_DIR, "figures", "main")
OUT_APP_FIG <- file.path(OUT_DIR, "figures", "appendix")
OUT_MAIN_TABLE <- file.path(OUT_DIR, "tables", "main")
OUT_APP_TABLE <- file.path(OUT_DIR, "tables", "appendix")
OUT_DIAG <- file.path(OUT_DIR, "diagnostics")

invisible(lapply(
  c(DATA_DIR, OUT_MAIN_FIG, OUT_APP_FIG, OUT_MAIN_TABLE, OUT_APP_TABLE, OUT_DIAG),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

# Execution switches. Set FALSE while developing if desired.
RUN_IN_TEXT <- TRUE
RUN_FIGURES <- TRUE
RUN_TABLES <- TRUE
RUN_SUPPLEMENT <- TRUE
RUN_WILD_BOOTSTRAP <- TRUE
WILD_REPS <- 999L
WILD_SEED <- 420L

# Original Stata global:
# vaccinated_baseline_18 perc_emp_ag radio_ownership breast own_land
COMMUNITY_COVARIATES <- c(
  "vaccinated_baseline_18", "perc_emp_ag", "radio_ownership", "breast", "own_land"
)

CENSUS_2015_VARS <- c(
  "perc_immun", "perc_literacy", "avg_age", "health_fivemiles_above",
  "perc_christian", "perc_muslim", "perc_born", "perc_emp_ag", "perc_acc_int",
  "formal_structure", "own_land", "prim_school_fivemiles",
  "source_water_fivemiles", "radio_ownership", "cell_ownership", "formal_roof",
  "all_assets"
)

VILLAGE_CENSUS_VARS <- c(
  "villsize", "vaccinated_baseline_18", "anyschooling", "BSL_owns_land",
  "BSL_reduced_portions", "age", "hh_gender", "breast", "preg"
)

KNOWLEDGE_OUTCOMES <- c(
  "END_covid_believe", "END_covid_know", "END_effect_stragree", "END_safe_stragree"
)

TRUST_OUTCOMES <- c(
  "END_trust_chc", "END_trust_mohs", "END_trust_media",
  "END_trust_socmedia", "END_trust_famfriend"
)

STATA_SSC <- fixest::ssc(
  K.adj = TRUE,
  K.fixef = "full",
  K.exact = TRUE,
  G.adj = TRUE,
  G.df = "min",
  t.df = "min"
)

`%||%` <- function(x, y) if (is.null(x)) y else x

read_clean <- function(filename) {
  path <- file.path(DATA_DIR, filename)
  if (!file.exists(path)) stop("Missing source data: ", path)
  haven::read_dta(path)
}

require_vars <- function(data, vars, context = "operation") {
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    stop(context, " requires missing variable(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

safe_mean <- function(x) if (sum(!is.na(x)) == 0L) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)
safe_se <- function(x) {
  n <- sum(!is.na(x))
  if (n < 2L) return(NA_real_)
  stats::sd(x, na.rm = TRUE) / sqrt(n)
}
safe_sum <- function(x) if (sum(!is.na(x)) == 0L) NA_real_ else sum(x, na.rm = TRUE)
safe_n_unique <- function(x) dplyr::n_distinct(x[!is.na(x)])

complete_mask <- function(data, vars) {
  require_vars(data, vars, "complete-case sample")
  stats::complete.cases(data[, vars, drop = FALSE])
}

var_label <- function(data, var) {
  lab <- attr(data[[var]], "label", exact = TRUE)
  if (is.null(lab) || is.na(lab) || !nzchar(trimws(lab))) var else trimws(lab)
}

as_numeric_plain <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) return(as.numeric(x))
  x
}

rhs_formula <- function(outcome, rhs = character(), fe = NULL) {
  rhs <- rhs[nzchar(rhs)]
  rhs_txt <- if (length(rhs)) paste(rhs, collapse = " + ") else "1"
  txt <- paste(outcome, "~", rhs_txt)
  if (!is.null(fe) && nzchar(fe)) txt <- paste0(txt, " | ", fe)
  stats::as.formula(txt)
}

fit_fe_model <- function(
    data,
    outcome,
    rhs,
    fe = "grpID",
    cluster = NULL,
    robust = FALSE
) {
  fml <- rhs_formula(outcome, rhs, fe)
  needed <- unique(c(all.vars(fml), cluster))
  require_vars(data, needed, paste0("Model for ", outcome))

  d <- data[, needed, drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!nrow(d)) stop("No complete observations for model: ", outcome)

  vc <- if (!is.null(cluster)) {
    stats::as.formula(paste0("~", cluster))
  } else if (robust) {
    "hetero"
  } else {
    "iid"
  }

  model <- fixest::feols(
    fml,
    data = d,
    vcov = vc,
    ssc = STATA_SSC,
    fixef.rm = "none",
    data.save = TRUE,
    notes = FALSE,
    warn = TRUE
  )
  attr(model, "rep_data") <- d
  attr(model, "rep_cluster") <- cluster
  attr(model, "rep_outcome") <- outcome
  attr(model, "rep_rhs") <- rhs
  attr(model, "rep_fe") <- fe
  attr(model, "rep_robust") <- robust
  model
}

coef_table <- function(model) {
  out <- as.data.frame(fixest::coeftable(model)) |>
    tibble::rownames_to_column("term")
  names(out)[2:5] <- c("estimate", "std_error", "statistic", "p_value")
  out
}

coef_stat <- function(model, term, field = c("estimate", "std_error", "p_value")) {
  field <- match.arg(field)
  tab <- coef_table(model)
  i <- match(term, tab$term)
  if (is.na(i)) return(NA_real_)
  tab[[field]][i]
}

model_r2 <- function(model) {
  val <- suppressWarnings(fixest::r2(model, "r2"))
  as.numeric(val[1])
}

model_data <- function(model) attr(model, "rep_data", exact = TRUE)

cluster_count <- function(model, cluster = attr(model, "rep_cluster", exact = TRUE)) {
  if (is.null(cluster)) return(stats::nobs(model))
  d <- model_data(model)
  safe_n_unique(d[[cluster]])
}

linear_combo <- function(model, weights) {
  b <- stats::coef(model)
  if (!all(names(weights) %in% names(b))) {
    return(c(estimate = NA_real_, std_error = NA_real_, p_value = NA_real_))
  }
  V <- stats::vcov(model)
  L <- setNames(rep(0, length(b)), names(b))
  L[names(weights)] <- weights
  est <- sum(L * b)
  se2 <- as.numeric(t(L) %*% V %*% L)
  if (!is.finite(se2) || se2 <= 0) {
    return(c(estimate = est, std_error = NA_real_, p_value = NA_real_))
  }
  se <- sqrt(se2)
  df <- suppressWarnings(fixest::degrees_freedom(model, type = "t"))
  p <- 2 * stats::pt(abs(est / se), df = df, lower.tail = FALSE)
  c(estimate = est, std_error = se, p_value = p)
}

wild_boot_p <- function(model, param, clustid = NULL, B = WILD_REPS, seed = WILD_SEED) {
  if (!RUN_WILD_BOOTSTRAP) return(NA_real_)

  # fwildclusterboot currently has two constraints that matter here:
  # 1) helper-fitted fixest models can fail on re-evaluation when their
  #    original data symbol is no longer visible (e.g. DATA_SAVED);
  # 2) when clustid is NULL, boottest() forces engine = "R-lean", and that
  #    engine does not accept fixest fixed effects.
  #
  # We therefore use two bootstrap-only refits:
  # - clustered Stata boottest calls: fixest + engine = "R" + projected FE;
  # - unclustered Stata boottest calls: ordinary lm with FE entered as factor
  #   dummies, then the supported heteroskedastic R-lean bootstrap.
  # The original fitted model is still used for coefficients/SEs/table output.

  boot_data <- model_data(model)
  outcome <- attr(model, "rep_outcome", exact = TRUE)
  rhs <- attr(model, "rep_rhs", exact = TRUE)
  fe <- attr(model, "rep_fe", exact = TRUE)

  if (is.null(boot_data) || is.null(outcome) || is.null(rhs)) {
    stop("Wild bootstrap model is missing stored replication metadata.")
  }

  if (!is.null(fe) && nzchar(fe) && fe %in% names(boot_data)) {
    boot_data[[fe]] <- as.factor(boot_data[[fe]])
  }

  data_name <- ".LMDIVUiSL_FWILD_DATA"
  temp_names <- data_name
  existed <- exists(data_name, envir = .GlobalEnv, inherits = FALSE)
  old_value <- if (existed) get(data_name, envir = .GlobalEnv, inherits = FALSE) else NULL

  assign(data_name, boot_data, envir = .GlobalEnv)
  on.exit({
    if (existed) {
      assign(data_name, old_value, envir = .GlobalEnv)
    } else if (exists(data_name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = data_name, envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  dqrng::dqset.seed(seed)

  if (is.null(clustid)) {
    # Stata Table 3 column (3): areg ..., absorb(grpID), followed by boottest
    # with no cluster option. fwildclusterboot implements the no-cluster
    # heteroskedastic wild bootstrap only through R-lean, which cannot consume
    # a fixest absorbed-FE object. OLS with explicit FE dummies is algebraically
    # equivalent to the absorbed-FE regression, so use lm() for this bootstrap.
    rhs_lm <- rhs
    if (!is.null(fe) && nzchar(fe)) rhs_lm <- c(rhs_lm, paste0("factor(", fe, ")"))
    lm_fml <- stats::as.formula(
      paste(outcome, "~", if (length(rhs_lm)) paste(rhs_lm, collapse = " + ") else "1")
    )

    assign(".LMDIVUiSL_FWILD_LM_FML", lm_fml, envir = .GlobalEnv)
    on.exit({
      if (exists(".LMDIVUiSL_FWILD_LM_FML", envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = ".LMDIVUiSL_FWILD_LM_FML", envir = .GlobalEnv)
      }
    }, add = TRUE)

    boot_model <- eval(
      quote(
        stats::lm(
          .LMDIVUiSL_FWILD_LM_FML,
          data = .LMDIVUiSL_FWILD_DATA,
          model = TRUE,
          x = TRUE,
          y = TRUE
        )
      ),
      envir = .GlobalEnv
    )

    bt <- fwildclusterboot::boottest(
      boot_model,
      param = param,
      B = as.integer(B),
      type = "rademacher",
      impose_null = TRUE,
      p_val_type = "two-tailed",
      conf_int = FALSE,
      engine = "R-lean"
    )
  } else {
    # Clustered specifications: use the native R engine, which supports
    # projecting out one fixed effect via fe = ... . Keep all symbols used by
    # the stored model call in the global environment while boottest executes.
    assign(".LMDIVUiSL_FWILD_FML", rhs_formula(outcome, rhs, fe), envir = .GlobalEnv)
    assign(".LMDIVUiSL_FWILD_VCOV", stats::as.formula(paste0("~", clustid)), envir = .GlobalEnv)
    on.exit({
      for (nm in c(".LMDIVUiSL_FWILD_FML", ".LMDIVUiSL_FWILD_VCOV")) {
        if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) rm(list = nm, envir = .GlobalEnv)
      }
    }, add = TRUE)

    boot_model <- eval(
      quote(
        fixest::feols(
          .LMDIVUiSL_FWILD_FML,
          data = .LMDIVUiSL_FWILD_DATA,
          vcov = .LMDIVUiSL_FWILD_VCOV,
          ssc = STATA_SSC,
          fixef.rm = "none",
          data.save = FALSE,
          notes = FALSE,
          warn = TRUE
        )
      ),
      envir = .GlobalEnv
    )

    bt <- fwildclusterboot::boottest(
      boot_model,
      param = param,
      B = as.integer(B),
      clustid = clustid,
      fe = fe,
      type = "rademacher",
      impose_null = TRUE,
      bootstrap_type = "fnw11",
      p_val_type = "two-tailed",
      conf_int = FALSE,
      engine = "R"
    )
  }

  as.numeric(bt$p_val)
}

merge_community_covariates <- function(individual, community, covariates = COMMUNITY_COVARIATES) {
  require_vars(individual, "community_code", "individual data merge")
  require_vars(community, c("community_code", covariates), "community data merge")

  using <- community |>
    dplyr::select(community_code, dplyr::all_of(covariates)) |>
    dplyr::distinct(community_code, .keep_all = TRUE)

  master_names <- names(individual)
  out <- dplyr::left_join(individual, using, by = "community_code", suffix = c("", ".community"))

  for (v in covariates) {
    uv <- paste0(v, ".community")
    if (uv %in% names(out)) {
      if (v %in% master_names) {
        # Stata merge ..., update replace: nonmissing values from using override master.
        out[[v]] <- dplyr::if_else(!is.na(out[[uv]]), out[[uv]], out[[v]])
      } else {
        out[[v]] <- out[[uv]]
      }
      out[[uv]] <- NULL
    }
  }
  out
}

# Two-stage sharpened FDR q-values, translated literally from Tables 6 and 7.
sharpened_qvalues <- function(p_values, step = 0.001) {
  p_values <- as.numeric(p_values)
  ans <- rep(NA_real_, length(p_values))
  ok <- which(is.finite(p_values))
  if (!length(ok)) return(ans)

  p <- p_values[ok]
  M <- length(p)
  ord <- order(p)
  p_sorted <- p[ord]
  rank <- seq_len(M)
  q_sorted <- rep(NA_real_, M)

  q_grid <- seq(1, step, by = -step)
  for (q in q_grid) {
    q_adj <- q / (1 + q)

    reject1 <- p_sorted <= q_adj * rank / M
    total1 <- if (any(reject1)) max(rank[reject1]) else 0L

    if (total1 >= M) {
      q2 <- Inf
    } else {
      q2 <- q_adj * (M / (M - total1))
    }

    reject2 <- p_sorted <= q2 * rank / M
    total2 <- if (any(reject2)) max(rank[reject2]) else 0L
    if (total2 > 0L) q_sorted[rank <= total2] <- q
  }

  restored <- rep(NA_real_, M)
  restored[ord] <- q_sorted
  ans[ok] <- restored
  ans
}

wald_block_p <- function(beta, V) {
  if (!length(beta)) return(NA_real_)
  keep <- is.finite(beta)
  beta <- beta[keep]
  V <- V[keep, keep, drop = FALSE]
  if (!length(beta)) return(NA_real_)
  stat <- as.numeric(t(beta) %*% qr.solve(V, beta))
  stats::pchisq(stat, df = length(beta), lower.tail = FALSE)
}

multinom_joint_p <- function(data, outcome, predictors, ref, target_class) {
  require_vars(data, c(outcome, predictors), "multinomial balance test")
  d <- data[, c(outcome, predictors), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[[outcome]] <- stats::relevel(factor(d[[outcome]]), ref = as.character(ref))

  fml <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  fit <- nnet::multinom(fml, data = d, trace = FALSE, Hess = TRUE)
  V <- stats::vcov(fit)
  b <- stats::coef(fit)

  if (is.null(dim(b))) {
    # Binary multinomial case.
    beta <- b[predictors]
    Vsub <- V[predictors, predictors, drop = FALSE]
    return(wald_block_p(beta, Vsub))
  }

  cls <- as.character(target_class)
  if (!cls %in% rownames(b)) stop("Target multinomial class not estimated: ", cls)

  beta <- b[cls, predictors]
  vn <- colnames(V)
  candidates <- paste0(cls, ":", predictors)
  if (!all(candidates %in% vn)) {
    candidates <- paste(cls, predictors)
  }
  if (!all(candidates %in% vn)) {
    # Fall back to matching suffixes; protects against nnet naming differences.
    candidates <- vapply(predictors, function(v) {
      hit <- vn[grepl(paste0("(^|:|\\.)", cls, "(:|\\.)?.*", v, "$"), vn)]
      if (!length(hit)) NA_character_ else hit[1]
    }, character(1))
  }
  if (anyNA(candidates)) stop("Could not match multinomial vcov coefficient names")

  Vsub <- V[candidates, candidates, drop = FALSE]
  wald_block_p(beta, Vsub)
}

glm_joint_p <- function(data, outcome, predictors) {
  require_vars(data, c(outcome, predictors), "joint binary test")
  d <- data[, c(outcome, predictors), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  fml <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  fit <- stats::glm(fml, data = d, family = stats::binomial())
  b <- stats::coef(fit)[predictors]
  V <- stats::vcov(fit)[predictors, predictors, drop = FALSE]
  wald_block_p(b, V)
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (!is.finite(x)) return("")
  formatC(x, digits = digits, format = "f")
}

reg_table <- function(models, terms, model_names = names(models), term_labels = NULL, extras = NULL) {
  if (is.null(model_names) || any(!nzchar(model_names))) model_names <- paste0("Model ", seq_along(models))
  if (is.null(term_labels)) term_labels <- setNames(terms, terms)

  rows <- list()
  for (term in terms) {
    label <- if (!is.null(names(term_labels)) && term %in% names(term_labels)) unname(term_labels[term]) else term
    est <- vapply(models, coef_stat, numeric(1), term = term, field = "estimate")
    se <- vapply(models, coef_stat, numeric(1), term = term, field = "std_error")
    p <- vapply(models, coef_stat, numeric(1), term = term, field = "p_value")
    rows[[length(rows) + 1L]] <- c(
      Row = label,
      setNames(vapply(seq_along(est), function(i) paste0(fmt_num(est[i]), stars(p[i])), character(1)), model_names)
    )
    rows[[length(rows) + 1L]] <- c(
      Row = "",
      setNames(vapply(se, function(x) if (is.finite(x)) paste0("(", fmt_num(x), ")") else "", character(1)), model_names)
    )
  }

  if (!is.null(extras)) {
    extra_names <- unique(unlist(lapply(extras, names), use.names = FALSE))
    for (nm in extra_names) {
      vals <- vapply(extras, function(x) {
        value <- x[[nm]]
        if (is.null(value) || length(value) == 0L || is.na(value)) "" else as.character(value)
      }, character(1))
      rows[[length(rows) + 1L]] <- c(Row = nm, setNames(vals, model_names))
    }
  }

  as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
}

write_table <- function(tab, stem, digits = 3, excel = FALSE, escape = TRUE) {
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tab, paste0(stem, ".csv"), row.names = FALSE, na = "")
  tex <- knitr::kable(tab, format = "latex", booktabs = TRUE, digits = digits, escape = escape)
  writeLines(tex, paste0(stem, ".tex"), useBytes = TRUE)
  if (excel) writexl::write_xlsx(tab, paste0(stem, ".xlsx"))
  invisible(tab)
}

save_plot <- function(plot, stem, width = 8, height = 5, dpi = 300) {
  dir.create(dirname(stem), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(paste0(stem, ".png"), plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  grDevices::svg(paste0(stem, ".svg"), width = width, height = height, bg = "white")
  print(plot)
  grDevices::dev.off()
  invisible(plot)
}

mean_sd_text <- function(x) paste0(fmt_num(safe_mean(x)), " (", fmt_num(safe_sd(x)), ")")
