# 04_covariate_choice.R
# Translation of 1.0.CovariateChoice.do.
#
# Important Stata details reproduced here:
#   1. Remove no-data / no-variation candidates.
#   2. Remove exact multicollinearity.
#   3. Sort by youthid before constructing folds.
#   4. Construct 10 folds within sector using seed 1234567.
#   5. Expand the dataset by integer attr_wgt BEFORE cross-validation.
#   6. Keep lagged outcome, treatments, and blocks unpenalized.

if (!exists("paths")) {
  source("code/01_setup.R")
}

if (!exists("primary")) {
  source("code/02_variable_lists.R")
}

if (!exists("fit_model")) {
  source("code/03_helpers.R")
}

# ------------------------------------------------------------------
# Candidate-variable list
# ------------------------------------------------------------------

if (!file.exists(paths$kitchensink)) {
  stop(
    "Required file not found: ",
    paths$kitchensink
  )
}

ks <- utils::read.csv(
  paths$kitchensink,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM",
  fill = TRUE
)

names(ks) <- tolower(
  trimws(
    names(ks)
  )
)

if (!"variable_midline" %in% names(ks)) {
  stop(
    "kitchensink.csv must contain variable_midline."
  )
}

candidates <- trimws(
  as.character(
    ks$variable_midline
  )
)

candidates <- unique(
  candidates[
    !is.na(candidates) &
      candidates != ""
  ]
)

# ------------------------------------------------------------------
# Follow-up sample
# ------------------------------------------------------------------

d <- read_panel() |>
  dplyr::filter(
    round > 0
  )

if (nrow(d) != 1770) {
  stop(
    "The original Stata covariate-selection script requires exactly ",
    "1,770 follow-up observations. Found ",
    nrow(d),
    "."
  )
}

if (anyDuplicated(d$youthid)) {
  stop(
    "Follow-up sample must contain one row per youthid."
  )
}

# ------------------------------------------------------------------
# Remove unavailable / empty / constant variables
# ------------------------------------------------------------------

candidates <- existing_vars(
  d,
  candidates
)

numeric_candidate <- vapply(
  d[
    ,
    candidates,
    drop = FALSE
  ],
  function(x) {
    is.numeric(x) ||
      is.integer(x) ||
      is.logical(x)
  },
  logical(1)
)

if (any(!numeric_candidate)) {
  stop(
    "Non-numeric kitchensink variables found: ",
    paste(
      candidates[!numeric_candidate],
      collapse = ", "
    )
  )
}

candidates <- candidates[
  vapply(
    candidates,
    function(v) {
      
      x <- d[[v]]
      
      z <- stats::na.omit(x)
      
      length(z) > 0 &&
        stats::sd(
          as.numeric(z)
        ) > 0
    },
    logical(1)
  )
]

if (!length(candidates)) {
  stop(
    "No usable kitchensink variables remain."
  )
}

# ------------------------------------------------------------------
# Approximate Stata _rmcoll using weighted QR decomposition
# ------------------------------------------------------------------

rmcoll_vars <- unique(
  c(
    candidates,
    "attr_wgt"
  )
)

rmcoll_sample <- stats::complete.cases(
  d[
    ,
    rmcoll_vars,
    drop = FALSE
  ]
)

if (!any(rmcoll_sample)) {
  stop(
    "No complete observations available for the multicollinearity check."
  )
}

X_rm <- as.matrix(
  d[
    rmcoll_sample,
    candidates,
    drop = FALSE
  ]
)

storage.mode(X_rm) <- "double"

w_rm <- sqrt(
  as.numeric(
    d$attr_wgt[
      rmcoll_sample
    ]
  )
)

X_rm <- sweep(
  X_rm,
  1,
  w_rm,
  "*"
)

X_rm <- cbind(
  `(Intercept)` = w_rm,
  X_rm
)

qr_rm <- qr(
  X_rm,
  tol = 1e-10,
  LAPACK = FALSE
)

independent <- qr_rm$pivot[
  seq_len(
    qr_rm$rank
  )
]

keep_candidate_index <- independent[
  independent > 1
] - 1L

keep_candidate_index <- sort(
  unique(
    keep_candidate_index
  )
)

removed_collinear <- setdiff(
  candidates,
  candidates[
    keep_candidate_index
  ]
)

if (length(removed_collinear)) {
  message(
    "Removed ",
    length(removed_collinear),
    " multicollinear kitchensink variable(s)."
  )
}

candidates <- candidates[
  keep_candidate_index
]

# ------------------------------------------------------------------
# Construct folds exactly in youthid-sorted order
# ------------------------------------------------------------------

folds <- d |>
  dplyr::select(
    youthid,
    sector
  ) |>
  dplyr::arrange(
    youthid
  )

set.seed(1234567)

folds$.u <- stats::runif(
  nrow(folds)
)

folds <- folds |>
  dplyr::group_by(
    sector
  ) |>
  dplyr::mutate(
    .rank = rank(
      .u,
      ties.method = "first"
    ),
    .n = dplyr::n(),
    .r = .rank / .n,
    fold = pmin(
      10L,
      ceiling(
        10 * .r
      )
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    youthid,
    fold
  )

d <- d |>
  dplyr::left_join(
    folds,
    by = "youthid"
  )

if (anyNA(d$fold)) {
  stop(
    "Fold assignment failed."
  )
}

# ------------------------------------------------------------------
# Reproduce Stata:
#
#   assert attr_wgt == 1 | attr_wgt == 2
#   expand attr_wgt
# ------------------------------------------------------------------

aw <- as.numeric(
  d$attr_wgt
)

if (
  any(
    is.na(aw) |
    !(aw %in% c(1, 2))
  )
) {
  stop(
    "attr_wgt must equal 1 or 2, matching the original Stata script."
  )
}

expand_index <- rep(
  seq_len(
    nrow(d)
  ),
  times = as.integer(aw)
)

dx <- d[
  expand_index,
  ,
  drop = FALSE
]

# ------------------------------------------------------------------
# LASSO
# ------------------------------------------------------------------

selected <- character()

blocks <- block_vars(
  dx
)

unpenalized_base <- existing_vars(
  dx,
  c(
    "treat_HD",
    "treat_GD",
    "treat_combined",
    blocks
  )
)

for (y in primary) {
  
  if (!y %in% names(dx)) {
    next
  }
  
  lag_y <- paste0(
    "L",
    y
  )
  
  unpenalized <- existing_vars(
    dx,
    c(
      lag_y,
      unpenalized_base
    )
  )
  
  vars <- unique(
    c(
      candidates,
      unpenalized
    )
  )
  
  needed <- unique(
    c(
      y,
      vars,
      "fold"
    )
  )
  
  cc <- stats::complete.cases(
    dx[
      ,
      needed,
      drop = FALSE
    ]
  )
  
  dd <- dx[
    cc,
    ,
    drop = FALSE
  ]
  
  if (!nrow(dd)) {
    next
  }
  
  X <- as.matrix(
    dd[
      ,
      vars,
      drop = FALSE
    ]
  )
  
  storage.mode(X) <- "double"
  
  penalty <- ifelse(
    colnames(X) %in% unpenalized,
    0,
    1
  )
  
  cv <- glmnet::cv.glmnet(
    x = X,
    y = as.numeric(dd[[y]]),
    family = "gaussian",
    alpha = 1,
    foldid = dd$fold,
    type.measure = "mse",
    penalty.factor = penalty,
    standardize = TRUE,
    intercept = TRUE,
    maxit = 200000
  )
  
  fit <- glmnet::glmnet(
    x = X,
    y = as.numeric(dd[[y]]),
    family = "gaussian",
    alpha = 1,
    lambda = cv$lambda.min,
    penalty.factor = penalty,
    standardize = TRUE,
    intercept = TRUE,
    maxit = 200000
  )
  
  cf <- as.matrix(
    stats::coef(fit)
  )[, 1]
  
  pick <- names(cf)[
    cf != 0
  ]
  
  pick <- setdiff(
    pick,
    "(Intercept)"
  )
  
  pick <- intersect(
    pick,
    candidates
  )
  
  selected <- union(
    selected,
    pick
  )
  
  message(
    y,
    ": selected ",
    length(pick),
    " candidate controls"
  )
}

# ------------------------------------------------------------------
# Save R-generated controls
# ------------------------------------------------------------------

readr::write_csv(
  tibble::tibble(
    controls = selected
  ),
  paths$controls
)

message(
  "Wrote ",
  length(selected),
  " controls to ",
  paths$controls
)