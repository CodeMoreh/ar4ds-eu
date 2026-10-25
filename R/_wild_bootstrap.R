##############################################################################
# Title:        _wild_bootstrap.R – the significance rule of the curve
# Purpose:      The restricted wild cluster bootstrap by country with which
#               every point of the curve is classed: the null hypothesis that
#               the coefficient is zero is imposed, the residuals of the model
#               fitted under that null are flipped in sign country by country,
#               and the clustered t of each replicate is set against the
#               clustered t of the data. The p-value is the share of
#               replicates whose t is at least as large in absolute value
#               (symmetric, two-sided).
#               The sign draws are made once, as a matrix with one row per
#               member state and one column per replicate, from a fixed seed
#               under a stated generator, and the same matrix serves every
#               specification. The rows are keyed by country code, so a fit
#               on fewer countries takes the rows of the countries that it
#               holds, and the order of the data rows does not matter. Anyone
#               who copies this file and fits the same model on the same cells
#               therefore obtains the stored p-value exactly, without
#               reseeding anything. The draws leave the random-number stream
#               of the session as they found it.
#               The replicates are not refitted one by one. Every quantity of
#               a replicate is linear in its signs, so the numerator of each
#               replicate and the country sums of its scores are products of
#               two small matrices with the sign matrix (the route that the
#               boottest software of Roodman and colleagues takes); the result
#               equals refitting each replicate.
#               Regression weights enter by the square-root transform of the
#               outcome and the design.
#               Constants: seed 20261025 (Mersenne-Twister, Inversion,
#               Rejection), 9,999 replicates, Rademacher signs (-1 and +1 with
#               equal probability) and the six-point weights of Webb.
# Author:       Chris Moreh
# Last updated: 2026-10-25
# Sourced by:   R/_curve_tools.R, and through it every stage from
#               R/09_spec_curve.R onwards
##############################################################################

wcr_seed      <- 20261025
wcr_draws     <- 9999
# the 27 member states of 2004-2013, in alphabetical order of their codes
wcr_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB",
                   "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
                   "SE", "SI", "SK")

# The sign matrix: rows are countries, columns are replicates, filled column
# by column, so that replicate b holds the 27 signs drawn in turn.
wcr_signs <- function(type = c("rademacher", "webb"), B = wcr_draws, seed = wcr_seed,
                      countries = wcr_countries) {
  type <- match.arg(type)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    RNGkind(old_kind[1], old_kind[2], old_kind[3])
    if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
    else rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  values <- switch(type,
    rademacher = c(-1, 1),
    webb       = c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)))
  matrix(sample(values, length(countries) * B, replace = TRUE),
         nrow = length(countries), dimnames = list(countries, NULL))
}

# y: the outcome; X: the full design matrix, with a column named `term`;
# cluster: the country code of each row; signs: a matrix from wcr_signs();
# w: regression weights or NULL. Returns the bootstrap p-value, the clustered
# t of the data (without small-sample factor, which cancels in the
# comparison), and the numbers of replicates and clusters.
wcr_test <- function(y, X, cluster, signs, term = "x", w = NULL) {
  if (!is.null(w)) {
    y <- y * sqrt(w)
    X <- X * sqrt(w)
  }
  j  <- match(term, colnames(X))
  cl <- sort(unique(as.character(cluster)))
  stopifnot("a cluster has no row in the sign matrix" = all(cl %in% rownames(signs)),
            "the term is not a column of the design" = !is.na(j))
  g <- match(as.character(cluster), cl)
  qr_u <- qr(X)
  qr_r <- qr(X[, -j, drop = FALSE])
  xt  <- qr.resid(qr_r, X[, j])            # the term, net of the other regressors
  e_r <- qr.resid(qr_r, y)                 # residuals under the null
  u   <- qr.resid(qr_u, y)                 # residuals of the full model
  t0  <- sum(xt * y) / sqrt(sum(rowsum(xt * u, g)^2))
  # replicate b has outcome e_r * v_b[g]: its numerator is a' v_b, and its
  # full-model residuals are U v_b, where column h of U is the full-model
  # residual of e_r masked to country h
  a <- rowsum(xt * e_r, g)
  E <- matrix(0, length(y), length(cl))
  E[cbind(seq_along(y), g)] <- e_r  # r-style: keep – filling a matrix
  U <- qr.resid(qr_u, E)
  C <- rowsum(xt * U, g)
  V <- signs[cl, , drop = FALSE]
  tb <- drop(crossprod(a, V)) / sqrt(colSums((C %*% V)^2))
  c(p = mean(abs(tb) >= abs(t0)), t = t0, B = ncol(V), G = length(cl))
}
