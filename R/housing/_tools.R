################################################################################
# Title:        _tools.R
# Purpose:      The estimator of the housing-audit case: one function that fits
#               any specification of the menu and returns its estimate in
#               percentage points, its 95% interval, the test of its model and
#               the Multi100 partial correlation.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   R/housing/02_spec_curve.R, R/housing/03_reference_marks.R,
#               R/housing/04_menu_deviations.R, R/housing/05_displays.R,
#               R/housing/06_replication.R, R/housing/check_parity.R,
#               cases/housing/index.qmd, cases/housing/menu.qmd
################################################################################

# The same code runs in the downloadable workspace and in webR on the browser
# lab, so all three give the same numbers. The standard errors and the average
# marginal effect come from packages: parameters with sandwich for the HC2
# errors of the linear model, marginaleffects for the average marginal effect
# of the logit. R/housing/check_parity.R checks both against estimatr and
# margins. The rows are chosen with dplyr, because datawizard evaluates its
# conditions in a fresh environment and so cannot see the arguments of a
# function.

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

# The conversion rules of Multi100 (standard_correlation_coefficient.R in
# github.com/marton-balazs-kovacs/multi100): t with its df, which is the rule of
# effectsize::t_to_r(); z with the sample size standing in for the missing df;
# a chi-square read as the square of a z, so it carries no sign.
r_multi100 <- function(stat, stat_type, df, n) {
  dplyr::case_when(
    stat_type == "t" ~ effectsize::t_to_r(stat, df, ci = NULL)$r,
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
# greeted by name less often, the direction of the claim), its SE and 95%
# interval, the test of the model that the specification fits, and the partial
# correlation by the Multi100 rule.
fit_spec <- function(audit, comparison = "white", population = "replies", uncoded = "drop",
                     model = "lpm", adjust = "none") {
  check_spec(comparison, population, uncoded, model, adjust)
  terms <- ADJUST_TERMS[[adjust]]
  # only the model columns, and only the emails with every covariate;
  # marginaleffects refuses a data column called `group`
  frame <- spec_frame(audit, comparison, population, uncoded) |>
    dplyr::select("y", "hisp", dplyr::all_of(terms)) |>
    stats::na.omit()
  form <- stats::reformulate(c("hisp", terms), response = "y")

  if (model == "lpm") {
    fit <- stats::lm(form, data = frame)
    # HC2 errors (MacKinnon and White 1985) from sandwich::vcovHC() (Zeileis
    # 2004, Journal of Statistical Software), with the t test on the residual df
    coefs <- parameters::model_parameters(fit, vcov = "HC2")
    row <- datawizard::data_filter(coefs, Parameter == "hisp")
    test <- list(coef = row$Coefficient, coef_se = row$SE, stat_type = "t", stat = row$t,
                 df = row$df_error, p = row$p)
    est <- list(b = row$Coefficient, se = row$SE, low = row$CI_low, high = row$CI_high)
  } else {
    fit <- stats::glm(form, family = stats::binomial(), data = frame)
    coefs <- parameters::model_parameters(fit, ci_method = "wald")
    row <- datawizard::data_filter(coefs, Parameter == "hisp")
    test <- list(coef = row$Coefficient, coef_se = row$SE, stat_type = "z", stat = row$z,
                 df = NA_real_, p = row$p)
    # The average marginal effect: the mean over the analysed emails of the
    # predicted probability with a Hispanic name minus that with a comparison
    # name, with delta-method errors (marginaleffects; Arel-Bundock, Greifer and
    # Heiss 2024, Journal of Statistical Software)
    ame <- marginaleffects::avg_comparisons(fit, variables = list(hisp = 0:1))
    est <- list(b = ame$estimate, se = ame$std.error, low = ame$conf.low, high = ame$conf.high)
  }

  tibble::tibble(
    comparison = comparison, population = population, uncoded = uncoded,
    model = model, adjust = adjust,
    b_pp = 100 * est$b, se_pp = 100 * est$se,
    coef = test$coef, coef_se = test$coef_se,
    stat_type = test$stat_type, stat = test$stat, df = test$df, p = test$p,
    r = r_multi100(test$stat, test$stat_type, test$df, nrow(frame)),
    n = nrow(frame), n_hisp = sum(frame$hisp == 1), n_comp = sum(frame$hisp == 0),
    rate_hisp = 100 * mean(frame$y[frame$hisp == 1]),
    rate_comp = 100 * mean(frame$y[frame$hisp == 0]),
    ci_low = 100 * est$low, ci_high = 100 * est$high
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(comparison, population, uncoded, model, adjust) {
  sprintf("comparison=%s population=%s uncoded=%s model=%s adjust=%s",
          comparison, population, uncoded, model, adjust)
}
