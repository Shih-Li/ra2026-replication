# Faithful R implementation of _gweightave.ado supplied with the replication package.
# Executable Stata behavior followed here:
#   1. correlation matrix from complete cases in the full sample;
#   2. symmetric generalized inverse; row sums are weights;
#   3. each component is standardized using mean and SD in normby == TRUE;
#   4. observation-specific denominator excludes weights for missing components;
#   5. constructed index is standardized again in normby == TRUE.

symmetric_ginv <- function(M, tol = sqrt(.Machine$double.eps)) {
  M <- as.matrix(M)
  M[!is.finite(M)] <- 0
  e <- eigen((M + t(M)) / 2, symmetric = TRUE)
  keep <- abs(e$values) > tol * max(1, max(abs(e$values)))
  if (!any(keep)) return(matrix(0, nrow(M), ncol(M)))
  e$vectors[, keep, drop = FALSE] %*%
    diag(1 / e$values[keep], nrow = sum(keep)) %*%
    t(e$vectors[, keep, drop = FALSE])
}

weightave <- function(data, vars, normby = rep(TRUE, nrow(data)), return_weights = FALSE) {
  stopifnot(all(vars %in% names(data)))
  X <- as.data.frame(data[, vars, drop = FALSE])
  normby <- !is.na(normby) & as.logical(normby)

  cc <- stats::complete.cases(X)
  if (sum(cc) < 2L) stop("Not enough complete observations to compute the correlation matrix.")
  Sigma <- stats::cor(X[cc, , drop = FALSE])
  Sigma[!is.finite(Sigma)] <- 0
  weights <- rowSums(symmetric_ginv(Sigma))
  names(weights) <- vars

  Z <- matrix(NA_real_, nrow(X), ncol(X), dimnames = list(NULL, vars))
  for (j in seq_along(vars)) {
    x <- X[[j]]
    mu <- mean(x[normby], na.rm = TRUE)
    s <- stats::sd(x[normby], na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
      Z[, j] <- NA_real_
    } else {
      Z[, j] <- (x - mu) / s
    }
  }

  observed <- !is.na(Z)
  numerator <- rowSums(sweep(replace(Z, is.na(Z), 0), 2, weights, `*`))
  denominator <- rowSums(sweep(observed * 1, 2, weights, `*`))
  index <- numerator / denominator
  index[rowSums(!is.na(X)) == 0L | !is.finite(index)] <- NA_real_

  mu_i <- mean(index[normby], na.rm = TRUE)
  sd_i <- stats::sd(index[normby], na.rm = TRUE)
  if (is.finite(sd_i) && sd_i != 0) index <- (index - mu_i) / sd_i

  if (return_weights) return(list(index = index, weights = weights, correlation = Sigma))
  index
}
