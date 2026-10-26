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

# The same code runs in the downloadable workspace and in webR on the browser
# lab, so all three give the same numbers. The statistics come from packages:
# survival splits each union into its months at risk; survey fits the binary
# models with errors that follow the design of the surveys; nnet fits the
# multinomial model and svyVGAM gives it the same design-based errors; insight
# reads the coefficients and their variances from each fit; effectsize holds
# two of the conversion rules of Multi100. R/cohabitation/check_parity.R checks
# the fits against routes that share none of this code. The rows are chosen
# with dplyr, because datawizard evaluates its conditions in a fresh
# environment and so cannot see the arguments of a function.

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
# interview; the outcome is recorded in the last month. A union is at risk for
# `duration + 1` months and ends in an event or, if it is intact, is censored
# in the last of them. Under a three-year horizon a union still intact at 36
# months is censored there. survival::survSplit() (Therneau and Grambsch 2000)
# cuts each union at every month and keeps its event in the last row only.
person_months <- function(unions, horizon = "five_years") {
  last <- if (horizon == "three_years") 36 else Inf
  # The status is a factor whose first level is the censoring, the form that
  # survival takes for competing events; survSplit() renames that level
  # "censor", so the outcome is read back from the position of the level.
  at_risk <- unions |>
    dplyr::mutate(month = pmin(duration + 1, last),
                  status = factor(dplyr::if_else(duration + 1 <= last, outcome, "intact"),
                                  levels = c("intact", "marriage", "separation")))
  survival::survSplit(at_risk, cut = seq_len(max(at_risk$month) - 1), end = "month", event = "status",
                      start = "month_start") |>
    dplyr::mutate(y = c("intact", "marriage", "separation")[as.integer(status)]) |>
    dplyr::select(-"month_start", -"status")
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

# Any model of the menu with every coefficient: the coefficients (one column per
# equation: marriage, and separation for the multinomial model), their
# variance under the chosen errors and under the model, the degrees of
# freedom and counts of the fit, and the fit itself.
#
# Design-based errors take the strata and clusters of the two surveys, by the
# linearisation of survey (Binder 1983; Lumley 2004, Journal of Statistical
# Software): survey::svyglm() for the binary models and svyVGAM::svy_vglm()
# for the multinomial logit of VGAM (Yee and Wild 1996, JRSS B), with t on
# clusters minus strata minus coefficients plus one df, the residual df of
# survey::svyglm(). The analysed months are a domain of the surveys: the design
# is declared on every cluster of the surveys compared and then subset, so that
# a cluster without analysed months still counts in its stratum. Errors
# clustered by woman take each woman as a cluster of a single stratum, with
# t on the number of women minus one df. The conventional errors treat every
# person-month as independent: the inverse information of the same fit
# (stats::glm(), or the naive variance of svyVGAM::svy_vglm()).
fit_model <- function(unions, cohorts = "paper", horizon = "five_years", adjust = "composition",
                      weights = "weighted", time = "linear", model = "mlogit", comparison = "1995_2006",
                      education = "paper", short = "drop", errors = "design") {
  check_spec(cohorts, horizon, adjust, weights, time, model, comparison, education, short, errors)
  frame <- spec_frame(unions, cohorts, horizon, weights, comparison, education, short) |>
    dplyr::mutate(marry = as.numeric(y == "marriage"),
                  y = factor(y, levels = c("intact", "marriage", "separation")),
                  woman = paste(cycle, caseid), keep = TRUE)
  rhs <- c("later", ADJUST_TERMS[[adjust]],
           switch(time, linear = "month", yearly = "factor(year_of_union)", none = NULL))

  design <- if (errors == "woman") {
    survey::svydesign(ids = ~woman, weights = ~w, data = frame)
  } else {
    clusters <- unions |>
      dplyr::filter(cycle %in% frame$cycle) |>
      dplyr::distinct(stratum, psu) |>
      dplyr::mutate(w = 1, keep = FALSE)
    survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w, nest = TRUE,
                      data = dplyr::bind_rows(frame, clusters)) |>
      subset(keep)  # r-style: keep – the subset method of a survey design keeps the strata of the full design
  }

  if (model == "mlogit") {
    # The multinomial logit by maximum likelihood with nnet::multinom()
    # (Venables and Ripley 2002), the software of the SCORE replication; then
    # svyVGAM::svy_vglm() started at that estimate, which iterates until no
    # coefficient moves by more than 1e-10 and gives the design-based errors.
    # The start matters where the months since the start and the length of the
    # whole union (`total_duration`) together all but fix the month of the
    # event: there the iterations of VGAM, left to themselves, stop far from
    # the maximum.
    form <- stats::reformulate(rhs, response = "y")
    start <- nnet::multinom(form, data = frame, weights = w, trace = FALSE)
    fit <- svyVGAM::svy_vglm(form, design = design, family = VGAM::multinomial(refLevel = "intact"),
                             coefstart = c(stats::coef(start)), criterion = "coefficients", epsilon = 1e-10)
    v_model <- fit$naive.var
    # VGAM names the coefficients of the marriage equation `:1` and those of the
    # separation equation `:2`
    equation <- c("marriage", "separation")[as.integer(sub(".*:", "", names(stats::coef(fit))))]
    term <- sub(":[12]$", "", names(stats::coef(fit)))
  } else {
    # glm() computes the variance from the weights of its last iteration, one
    # step before the estimate; stopping when the deviance changes by less
    # than 1e-13 of itself, rather than 1e-8, makes that step negligible
    fam <- switch(model, logit = stats::quasibinomial(link = "logit"),
                  cloglog = stats::quasibinomial(link = "cloglog"), lpm = stats::gaussian())
    fit <- survey::svyglm(stats::reformulate(rhs, response = "marry"), design = design, family = fam,
                          control = stats::glm.control(epsilon = 1e-13))
    conventional <- stats::glm(stats::reformulate(rhs, response = "marry"), data = frame, weights = w,
                               family = fam, control = stats::glm.control(epsilon = 1e-13))
    # the binomial variance of the logit and the complementary log-log, which
    # fixes the dispersion at one; the residual variance of the linear model
    v_model <- stats::vcov(conventional, dispersion = if (model == "lpm") NULL else 1)
    equation <- rep("marriage", length(stats::coef(fit)))
    term <- names(stats::coef(fit))
  }

  # Coefficients and variances in the order of the equations, every term of
  # the marriage equation first
  ord <- order(match(equation, c("marriage", "separation")))
  labels <- paste(equation, term, sep = ":")[ord]
  coefs <- insight::get_parameters(fit)$Estimate[ord]
  v <- if (errors == "model") v_model else insight::get_varcov(fit)
  list(
    coef = matrix(coefs, ncol = length(unique(equation)),
                  dimnames = list(unique(term[ord]), unique(equation[ord]))),
    vcov = structure(v[ord, ord], dimnames = list(labels, labels)),
    vcov_model = structure(v_model[ord, ord], dimnames = list(labels, labels)),
    df = switch(errors,
                design = survey::degf(design) + 1 - length(coefs),
                woman = survey::degf(design),
                model = Inf),
    n = nrow(frame), n_unions = dplyr::n_distinct(frame$woman), n_marriages = sum(frame$marry),
    n_psu = dplyr::n_distinct(frame$psu), n_strata = dplyr::n_distinct(frame$stratum),
    fit = fit
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
  data.frame(  # r-style: keep – a data frame prints every digit that the tasks quote
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

# The conversion rules of Multi100 (standard_correlation_coefficient.R in
# github.com/marton-balazs-kovacs/multi100): t with its df, which is the rule of
# effectsize::t_to_r(); z with the sample size standing in for the missing df;
# F with one numerator df read as the square of a t, the rule of
# effectsize::F_to_r(); a chi-square read as the square of a z, so it carries
# no sign.
r_multi100 <- function(stat, stat_type, df, n) {
  stat_type <- rep_len(stat_type, length(stat))
  dplyr::case_when(
    stat_type == "t" ~ effectsize::t_to_r(stat, df, ci = NULL)$r,
    stat_type == "z" ~ tanh(abs(stat) * sqrt(1 / (n - 3))) * sign(stat),
    stat_type == "F" ~ effectsize::F_to_r(abs(stat), 1, df, ci = NULL)$r,
    stat_type == "chi2" ~ tanh(sqrt(abs(stat)) * sqrt(1 / (n - 3)))
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(cohorts, horizon, adjust, weights, time, model) {
  sprintf("cohorts=%s horizon=%s adjust=%s weights=%s time=%s model=%s",
          cohorts, horizon, adjust, weights, time, model)
}
