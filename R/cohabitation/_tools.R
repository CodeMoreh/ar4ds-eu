################################################################################
# Title:        _tools.R
# Purpose:      The estimator of the cohabitation case: the menu of the curve,
#               the person-months of any specification, and one function that
#               fits it (multinomial, logit or complementary log-log
#               discrete-time model, or the linear probability check) and
#               returns the coefficient of the later cohort on the marriage
#               transition, its design-based test, the conventional test and the
#               Multi100 partial correlation.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   R/cohabitation/02_person_months.R, R/cohabitation/03_spec_curve.R,
#               R/cohabitation/04_reference_marks.R, R/cohabitation/05_menu_deviations.R,
#               R/cohabitation/06_displays.R, R/cohabitation/check_parity.R
################################################################################

# The fits use base R (stats) and dplyr only, on purpose: the same code runs in
# the downloadable workspace and in webR on the browser lab. The multinomial
# model is fitted by Newton-Raphson below rather than by nnet, and the
# design-based errors are written out rather than taken from the survey
# package; R/cohabitation/check_parity.R checks both against nnet and survey.

# The menu of the curve, in menu order: every combination is one specification
# (2 x 2 x 3 x 2 x 2 x 3 = 144). Anything else is refused by check_spec() unless
# it is one of the off-curve options listed in OFF_CURVE.
SPEC_AXES <- list(
  cohorts = c("paper", "five_years"),
  horizon = c("five_years", "three_years"),
  adjust  = c("none", "age", "composition"),
  weights = c("weighted", "unweighted"),
  time    = c("linear", "yearly"),
  model   = c("mlogit", "logit", "cloglog")
)

# Options that fit_spec() can also fit off the curve, for the displays and the
# reference marks. For `comparison`, `education`, `short` and `errors` the first
# option is the one that the curve holds fixed.
OFF_CURVE <- list(
  adjust     = c("race_period", "educ_period", "total_duration"),
  time       = c("none"),
  model      = c("lpm"),
  comparison = c("1995_2006", "2002_2006", "1995_2011", "2002_2011", "2006_2011"),
  education  = c("paper", "score"),
  short      = c("drop", "keep"),
  errors     = c("design", "woman", "model")
)

# The surveys compared: the earlier and the later cohort. The paper compares
# NSFG 1995 with NSFG 2006-2010; the others pair the cycles that SCORE and the
# Multi100 analysts used, and `2011` pools NSFG 2011-2013 and 2013-2015, as
# the SCORE replication did.
COMPARISONS <- list(
  "1995_2006" = list(earlier = "1995", later = "2006-10"),
  "2002_2006" = list(earlier = "2002", later = "2006-10"),
  "1995_2011" = list(earlier = "1995", later = c("2011-13", "2013-15")),
  "2002_2011" = list(earlier = "2002", later = c("2011-13", "2013-15")),
  "2006_2011" = list(earlier = "2006-10", later = c("2011-13", "2013-15"))
)

# Covariates of each adjustment set besides the later cohort (`later`) and the
# duration terms. `race_period` is Model 2 of Table 2 of the paper and
# `educ_period` its Model 1: with the interactions, the coefficient of `later`
# is the change for non-Hispanic white women or for college graduates only.
# `total_duration` adds the length of the whole union, a quantity that is known
# only once the union has ended, as the file that SCORE built does.
ADJUST_TERMS <- list(
  none           = character(0),
  age            = c("age", "age2"),
  composition    = c("age", "age2", "black", "hispanic", "other", "less_hs", "hs", "some_college"),
  race_period    = c("age", "age2", "black", "hispanic", "other", "less_hs", "hs", "some_college",
                     "later_black", "later_hispanic", "later_other"),
  educ_period    = c("age", "age2", "black", "hispanic", "other", "less_hs", "hs", "some_college",
                     "later_less_hs", "later_hs", "later_some_college"),
  total_duration = c("age", "age2", "black", "hispanic", "other", "less_hs", "hs", "some_college",
                     "union_length")
)

# The specification closest to the model of the paper (Table 2), without its
# interaction of race-ethnicity and period
REFERENCE <- list(cohorts = "paper", horizon = "five_years", adjust = "composition",
                  weights = "weighted", time = "linear", model = "mlogit")

check_spec <- function(cohorts, horizon, adjust, weights, time, model,
                       comparison = "1995_2006", education = "paper", short = "drop", errors = "design") {
  chosen <- list(cohorts = cohorts, horizon = horizon, adjust = adjust, weights = weights, time = time,
                 model = model, comparison = comparison, education = education, short = short,
                 errors = errors)
  widened <- intersect(names(SPEC_AXES), names(OFF_CURVE))
  allowed <- c(purrr::imap(SPEC_AXES, \(options, axis) c(options, OFF_CURVE[[axis]])),
               OFF_CURVE[setdiff(names(OFF_CURVE), widened)])
  bad <- names(chosen)[!purrr::map2_lgl(chosen, allowed[names(chosen)], \(v, ok) length(v) == 1 && v %in% ok)]
  if (length(bad) > 0) {
    stop("Not on the menu: ", paste0(bad, " = '", unlist(chosen[bad]), "'", collapse = ", "),
         ". The options are listed in menu_options.csv.", call. = FALSE)
  }
  invisible(TRUE)
}

# The unions of a specification: first premarital cohabitations of NSFG 1995
# (begun 1990-1995) against those of NSFG 2006-2010, begun no more than five
# years before the interview; `paper` keeps from NSFG 2006-2010 only the
# unions begun in 2005 or later, as the paper does. Unions that lasted less
# than a month are left out unless `short = "keep"`. `later` marks the later
# cohort.
spec_unions <- function(unions, cohorts = "paper", short = "drop", comparison = "1995_2006") {
  pair <- COMPARISONS[[comparison]]
  unions |>
    dplyr::filter(cycle %in% c(pair$earlier, pair$later), since_start <= 60, !is.na(duration),
                  .env$short == "keep" | duration >= 1,
                  .env$cohorts == "five_years" | cycle != "2006-10" | from_2005) |>
    dplyr::mutate(later = as.integer(cycle %in% pair$later))
}

# One row per union and month at risk, from the month in which the couple
# moved in (month 1) to the month of the marriage, the separation or the
# interview; the outcome is recorded in the last month. Under a three-year
# horizon months after the 36th are dropped and a union still intact at 36
# months counts as intact.
person_months <- function(unions, horizon = "five_years") {
  last <- if (horizon == "three_years") 36 else Inf
  unions |>
    dplyr::mutate(n_months = pmin(duration + 1, last),
                  event = dplyr::if_else(duration + 1 <= last, outcome, "intact")) |>
    tidyr::uncount(n_months, .id = "month") |>
    dplyr::mutate(y = dplyr::if_else(month == duration + 1, event, "intact")) |>
    dplyr::select(-event)
}

# The analysed person-months of one specification with every column that a
# model of the menu can use
spec_frame <- function(unions, cohorts = "paper", horizon = "five_years", weights = "weighted",
                       comparison = "1995_2006", education = "paper", short = "drop") {
  spec_unions(unions, cohorts, short, comparison) |>
    dplyr::mutate(
      educ = if (.env$education == "score") educ_score else educ,
      age = age_start, age2 = age_start^2,
      black = as.integer(race == "black"), hispanic = as.integer(race == "hispanic"),
      other = as.integer(race == "other"),
      less_hs = as.integer(educ == "less_hs"), hs = as.integer(educ == "hs"),
      some_college = as.integer(educ == "some_college"),
      later_black = later * black, later_hispanic = later * hispanic, later_other = later * other,
      later_less_hs = later * less_hs, later_hs = later * hs, later_some_college = later * some_college,
      union_length = duration,
      w = if (.env$weights == "weighted") weight else 1
    ) |>
    person_months(horizon) |>
    dplyr::mutate(w = w / mean(w),
                  year_of_union = pmin(ceiling(month / 12), 5))
}

# The design matrix: an intercept, the later cohort, the adjustment terms and
# the duration terms (months since the start, or one baseline per year of the
# union with the first year as reference)
spec_matrix <- function(frame, adjust = "composition", time = "linear") {
  x <- cbind(`(Intercept)` = 1, later = frame$later, as.matrix(frame[, ADJUST_TERMS[[adjust]]]))
  if (time == "linear") x <- cbind(x, month = frame$month)
  if (time == "yearly") {
    years <- sort(unique(frame$year_of_union))[-1]
    yr <- outer(frame$year_of_union, years, `==`) * 1
    colnames(yr) <- paste0("year", years)
    x <- cbind(x, yr)
  }
  x
}

# Multinomial logit by Newton-Raphson, with intact as the reference category:
# one column of coefficients for marriage and one for separation. Returns the
# coefficients, the information matrix and the score contribution of every row.
fit_mlogit <- function(x, y, w, tol = 1e-10, max_iter = 50) {
  yy <- cbind(marriage = as.numeric(y == "marriage"), separation = as.numeric(y == "separation"))
  k <- ncol(x)
  # Start from the shares of the three outcomes, with the intercept in column 1
  beta <- matrix(0, k, 2)
  beta[1, ] <- log(colSums(w * yy) / sum(w * (1 - rowSums(yy))))  # r-style: keep – a coefficient matrix
  probs <- function(beta) {
    e <- exp(x %*% beta)
    e / (1 + rowSums(e))
  }
  info_of <- function(p) {
    block <- \(a, b) crossprod(x, x * (w * (p[, a] * (a == b) - p[, a] * p[, b])))
    rbind(cbind(block(1, 1), block(1, 2)), cbind(block(2, 1), block(2, 2)))  # r-style: keep – matrix blocks
  }
  loglik <- function(beta) {
    eta <- x %*% beta
    sum(w * (rowSums(yy * eta) - log(1 + rowSums(exp(eta)))))
  }
  ll <- loglik(beta)
  for (iter in seq_len(max_iter)) {  # r-style: keep – Newton-Raphson, each step needs the last
    p <- probs(beta)
    grad <- c(crossprod(x, w * (yy[, 1] - p[, 1])), crossprod(x, w * (yy[, 2] - p[, 2])))
    step <- matrix(solve(info_of(p), grad), k, 2)
    # Halve the step until the log-likelihood does not fall
    halvings <- purrr::detect_index(0:20, \(h) loglik(beta + step / 2^h) >= ll)
    beta_new <- beta + step / 2^(max(halvings, 1) - 1)
    ll_new <- loglik(beta_new)
    beta <- beta_new
    if (abs(ll_new - ll) < tol * (abs(ll) + tol)) break
    ll <- ll_new
  }
  p <- probs(beta)
  dimnames(beta) <- list(colnames(x), colnames(yy))
  scores <- cbind(x * (w * (yy[, 1] - p[, 1])), x * (w * (yy[, 2] - p[, 2])))
  list(coef = beta, info = info_of(p), scores = scores, p_marriage = p[, 1], iter = iter)
}

# A binary model of marriage in the month (a separation month counts as a month
# without marriage, after which the union leaves the risk set): logit or
# complementary log-log by glm, or the linear probability model by lm. Returns
# what fit_mlogit() returns, for the marriage column only.
fit_binary <- function(x, y, w, model) {
  ym <- as.numeric(y == "marriage")
  if (model == "lpm") {
    fit <- stats::lm.wfit(x, ym, w)
    e <- ym - drop(x %*% fit$coefficients)
    info <- crossprod(x, x * w)
    return(list(coef = cbind(marriage = fit$coefficients), info = info, scores = x * (w * e),
                residual_var = sum(w * e^2) / (length(ym) - ncol(x))))
  }
  fam <- stats::quasibinomial(link = if (model == "logit") "logit" else "cloglog")
  fit <- stats::glm.fit(x, ym, weights = w, family = fam)
  eta <- drop(x %*% fit$coefficients)
  mu <- fam$linkinv(eta)
  d <- fam$mu.eta(eta)
  info <- crossprod(x, x * (w * d^2 / fam$variance(mu)))
  list(coef = cbind(marriage = fit$coefficients), info = info,
       scores = x * (w * d * (ym - mu) / fam$variance(mu)), p_marriage = mu)
}

# Between-cluster variance of summed score contributions, clusters within
# strata: the Taylor linearisation of the survey package with its default
# with-replacement design. `design` lists every cluster of the surveys with
# its stratum; clusters that hold no analysed row enter with zero totals, as
# they do for a subset of a survey design. Clustering by woman passes the women
# as clusters of a single stratum.
meat_clustered <- function(scores, cluster, design) {
  totals <- rowsum(scores, cluster, reorder = FALSE)
  empty <- setdiff(design$psu, rownames(totals))
  totals <- rbind(totals, matrix(0, length(empty), ncol(scores), dimnames = list(empty, NULL)))  # r-style: keep – matrices
  str_of <- design$stratum[match(rownames(totals), design$psu)]
  n_h <- as.vector(table(str_of)[str_of])
  means <- rowsum(totals, str_of, reorder = FALSE)[str_of, , drop = FALSE] / n_h
  crossprod((totals - means) * sqrt(n_h / (n_h - 1)))
}

# Any model of the menu with every coefficient: the coefficients (one column per
# equation: marriage, and separation for the multinomial model), their
# variance under the chosen errors and under the model, and the degrees of
# freedom and counts of the fit. Design-based errors take the strata and
# clusters of the two surveys, with t on clusters minus strata minus
# coefficients plus one df, as svyglm counts them.
fit_model <- function(unions, cohorts = "paper", horizon = "five_years", adjust = "composition",
                      weights = "weighted", time = "linear", model = "mlogit", comparison = "1995_2006",
                      education = "paper", short = "drop", errors = "design") {
  check_spec(cohorts, horizon, adjust, weights, time, model, comparison, education, short, errors)
  frame <- spec_frame(unions, cohorts, horizon, weights, comparison, education, short)
  x <- spec_matrix(frame, adjust, time)
  # Columns are divided by their largest value for the fit (age squared runs to
  # about 1,800), which keeps the information matrix well conditioned; the
  # results are scaled back below.
  col_scale <- apply(abs(x), 2, max)  # r-style: keep – a model matrix, not a data frame
  xs <- sweep(x, 2, col_scale, `/`)
  fit <- if (model == "mlogit") fit_mlogit(xs, frame$y, frame$w) else fit_binary(xs, frame$y, frame$w, model)
  bread <- solve(fit$info)

  design <- unions |>
    dplyr::filter(cycle %in% frame$cycle) |>
    dplyr::distinct(stratum, psu)
  women <- unique(paste(frame$cycle, frame$caseid))
  meat <- switch(errors,
                 design = meat_clustered(fit$scores, frame$psu, design),
                 woman = meat_clustered(fit$scores, paste(frame$cycle, frame$caseid),
                                        data.frame(stratum = "all", psu = women)),
                 model = NULL)
  v <- if (is.null(meat)) bread else bread %*% meat %*% bread
  bread_model <- if (model == "lpm") bread * fit$residual_var else bread
  scale_all <- rep(col_scale, ncol(fit$coef))
  terms <- paste(rep(colnames(fit$coef), each = ncol(x)), colnames(x), sep = ":")
  unscale <- \(m) structure(m / outer(scale_all, scale_all), dimnames = list(terms, terms))

  list(
    coef = fit$coef / col_scale, vcov = unscale(v), vcov_model = unscale(bread_model),
    df = switch(errors,
                design = length(unique(frame$psu)) - length(unique(frame$stratum)) - length(fit$coef) + 1,
                woman = length(women) - 1,
                model = Inf),
    n = nrow(frame), n_unions = length(women), n_marriages = sum(frame$y == "marriage"),
    n_psu = length(unique(frame$psu)), n_strata = length(unique(frame$stratum))
  )
}

# One specification: the coefficient of the later cohort in the marriage
# equation (log odds of marrying in a month for logit, log of the relative risk
# of marrying rather than staying together for the multinomial model, log of
# the monthly hazard for the complementary log-log; negative = cohabitors of
# the later cohort marry less, the direction of the claim), its error and test
# under the chosen errors (design-based on the curve), the conventional test,
# and the partial correlation by the Multi100 rule from the conventional z and
# the number of person-months.
fit_spec <- function(unions, cohorts = "paper", horizon = "five_years", adjust = "composition",
                     weights = "weighted", time = "linear", model = "mlogit", comparison = "1995_2006",
                     education = "paper", short = "drop", errors = "design") {
  m <- fit_model(unions, cohorts, horizon, adjust, weights, time, model, comparison, education, short, errors)
  b <- m$coef["later", "marriage"]
  se <- sqrt(m$vcov["marriage:later", "marriage:later"])
  stat <- b / se
  se_model <- sqrt(m$vcov_model["marriage:later", "marriage:later"])
  z_model <- b / se_model
  data.frame(
    cohorts = cohorts, horizon = horizon, adjust = adjust, weights = weights, time = time, model = model,
    comparison = comparison, education = education, short = short, errors = errors,
    b = b, se = se, stat_type = if (is.finite(m$df)) "t" else "z", stat = stat, df = m$df,
    p = 2 * stats::pt(-abs(stat), m$df),
    se_model = se_model, z_model = z_model, p_model = 2 * stats::pnorm(-abs(z_model)),
    r = r_multi100(z_model, "z", NA, m$n),
    ratio = exp(b),
    n = m$n, n_unions = m$n_unions, n_marriages = m$n_marriages, n_psu = m$n_psu, n_strata = m$n_strata
  )
}

# The conversion rules of Multi100 (standard_correlation_coefficient.R): t with
# its df; z with the sample size standing in for the missing df; F with one
# numerator df read as the square of a t; a chi-square read as the square of a
# z, so it carries no sign.
r_multi100 <- function(stat, stat_type, df, n) {
  stat_type <- rep_len(stat_type, length(stat))
  dplyr::case_when(
    stat_type == "t" ~ stat / sqrt(stat^2 + df),
    stat_type == "z" ~ tanh(abs(stat) * sqrt(1 / (n - 3))) * sign(stat),
    stat_type == "F" ~ sqrt(abs(stat) / (abs(stat) + df)),
    stat_type == "chi2" ~ tanh(sqrt(abs(stat)) * sqrt(1 / (n - 3)))
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(cohorts, horizon, adjust, weights, time, model) {
  sprintf("cohorts=%s horizon=%s adjust=%s weights=%s time=%s model=%s",
          cohorts, horizon, adjust, weights, time, model)
}
