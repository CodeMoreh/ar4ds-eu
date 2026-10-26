################################################################################
# Title:        _tools.R
# Purpose:      The estimator of the housing-audit case: one function that fits
#               any specification of the menu and returns its estimate in
#               percentage points, its test and the Multi100 partial correlation.
# Author:       Claude for CM
# Last updated: 2026-10-25
# Sourced by:   R/housing/02_spec_curve.R, R/housing/03_reference_marks.R,
#               R/housing/04_menu_deviations.R, R/housing/05_displays.R, R/housing/check_parity.R
################################################################################

# The fits use stats only (and dplyr for the rows), on purpose: the same code
# runs in the downloadable workspace and in webR on the browser lab, so all
# three give the same numbers. R/housing/check_parity.R checks it against estimatr
# (HC2) and marginaleffects (average marginal effects).

# The options of the curve, in menu order. Anything else is refused by
# check_spec() with the reason recorded in cases/housing/data/menu_options.csv.
SPEC_AXES <- list(
  comparison = c("white", "nonhispanic"),
  population = c("replies", "all"),
  uncoded    = c("drop", "not_named"),
  model      = c("lpm", "logit"),
  adjust     = c("none", "email", "place", "place_ethnic")
)

# Covariates of each adjustment set; all are recorded before the emails went out.
ADJUST_TERMS <- list(
  none         = character(0),
  email        = "hard_email",
  place        = c("hard_email", "pop_third", "poverty_third"),
  place_ethnic = c("hard_email", "pop_third", "poverty_third", "black_third", "hispanic_third")
)

# The analysed rows and the outcome of one specification. Bounced emails never
# reached an official and are outside the experiment under every option.
spec_frame <- function(audit, comparison = "white", population = "replies", uncoded = "drop") {
  audit |>
    dplyr::filter(!is.na(reply),
                  .env$comparison == "nonhispanic" | group %in% c("white", "hispanic"),
                  .env$population == "all" | reply == 1) |>
    dplyr::mutate(
      hisp = as.integer(group == "hispanic"),
      y = dplyr::case_when(reply == 0 ~ 0,
                           is.na(named) & .env$uncoded == "not_named" ~ 0,
                           .default = named)
    ) |>
    dplyr::filter(!is.na(y))
}

# HC2 standard errors (Neyman-style for a randomised comparison), written out so
# that no package is needed in the browser.
vcov_hc2 <- function(fit) {
  x <- model.matrix(fit)
  e <- residuals(fit)
  h <- hat(qr(x), intercept = FALSE)
  bread <- solve(crossprod(x))
  meat <- crossprod(x * (e / sqrt(1 - h)))
  bread %*% meat %*% bread
}

# Average marginal effect of hisp in a logit, with a delta-method SE taken from
# a numerical gradient: the mean of p(hisp = 1) - p(hisp = 0) over the analysed rows.
ame_logit <- function(fit) {
  x1 <- model.matrix(fit)
  x0 <- x1
  x1[, "hisp"] <- 1  # r-style: keep – a model matrix, not a data frame
  x0[, "hisp"] <- 0  # r-style: keep – a model matrix, not a data frame
  ame <- \(beta) mean(plogis(x1 %*% beta) - plogis(x0 %*% beta))
  beta <- coef(fit)
  grad <- purrr::map_dbl(seq_along(beta), \(k) {
    step <- 1e-6 * max(1, abs(beta[k]))
    up <- beta
    dn <- beta
    up[k] <- up[k] + step
    dn[k] <- dn[k] - step
    (ame(up) - ame(dn)) / (2 * step)
  })
  list(est = ame(beta), se = sqrt(drop(t(grad) %*% vcov(fit) %*% grad)))
}

# The conversion rules of Multi100 (standard_correlation_coefficient.R): t with
# its df; z with the sample size standing in for the missing df; a chi-square
# read as the square of a z, so it carries no sign.
r_multi100 <- function(stat, stat_type, df, n) {
  dplyr::case_when(
    stat_type == "t" ~ stat / sqrt(stat^2 + df),
    stat_type == "z" ~ tanh(abs(stat) * sqrt(1 / (n - 3))) * sign(stat),
    stat_type == "chi2" ~ tanh(sqrt(abs(stat)) * sqrt(1 / (n - 3)))
  )
}

check_spec <- function(comparison, population, uncoded, model, adjust) {
  chosen <- list(comparison = comparison, population = population, uncoded = uncoded,
                 model = model, adjust = adjust)
  bad <- names(chosen)[!purrr::map2_lgl(chosen, SPEC_AXES[names(chosen)], \(v, ok) v %in% ok)]
  if (length(bad) > 0) {
    stop("Not on the curve: ", paste0(bad, " = '", unlist(chosen[bad]), "'", collapse = ", "),
         ". The options are listed in menu_options.csv.", call. = FALSE)
  }
  invisible(TRUE)
}

# One specification: estimate in percentage points (negative = Hispanic names
# greeted by name less often, the direction of the claim), its SE, the test of
# the model that the specification fits, and the partial correlation by the Multi100 rule.
fit_spec <- function(audit, comparison = "white", population = "replies", uncoded = "drop",
                     model = "lpm", adjust = "none") {
  check_spec(comparison, population, uncoded, model, adjust)
  terms <- ADJUST_TERMS[[adjust]]
  frame <- spec_frame(audit, comparison, population, uncoded) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(terms), \(x) !is.na(x)))
  rhs <- paste(c("hisp", terms), collapse = " + ")
  form <- stats::as.formula(paste("y ~", rhs))

  if (model == "lpm") {
    fit <- stats::lm(form, data = frame)
    coef_b <- unname(coef(fit)["hisp"])
    coef_se <- sqrt(vcov_hc2(fit)["hisp", "hisp"])
    stat <- coef_b / coef_se
    df <- fit$df.residual
    p <- 2 * stats::pt(-abs(stat), df)
    est <- coef_b
    est_se <- coef_se
    stat_type <- "t"
  } else {
    fit <- stats::glm(form, family = stats::binomial(), data = frame)
    coef_b <- unname(coef(fit)["hisp"])
    coef_se <- sqrt(vcov(fit)["hisp", "hisp"])
    stat <- coef_b / coef_se
    df <- NA_real_
    p <- 2 * stats::pnorm(-abs(stat))
    ame <- ame_logit(fit)
    est <- ame$est
    est_se <- ame$se
    stat_type <- "z"
  }

  n <- nrow(frame)
  data.frame(
    comparison = comparison, population = population, uncoded = uncoded,
    model = model, adjust = adjust,
    b_pp = 100 * est, se_pp = 100 * est_se,
    coef = coef_b, coef_se = coef_se,
    stat_type = stat_type, stat = stat, df = df, p = p,
    r = r_multi100(stat, stat_type, df, n),
    n = n, n_hisp = sum(frame$hisp == 1), n_comp = sum(frame$hisp == 0),
    rate_hisp = 100 * mean(frame$y[frame$hisp == 1]),
    rate_comp = 100 * mean(frame$y[frame$hisp == 0])
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(comparison, population, uncoded, model, adjust) {
  sprintf("comparison=%s population=%s uncoded=%s model=%s adjust=%s",
          comparison, population, uncoded, model, adjust)
}
