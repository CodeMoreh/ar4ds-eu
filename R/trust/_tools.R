################################################################################
# Title:        _tools.R
# Purpose:      The estimator of the immigration-and-trust case: the menu of the
#               curve, and one function that fits any specification of it (and
#               the off-curve options that a linear model can fit) and returns
#               the coefficient of concern about immigration, its tests and the
#               Multi100 partial correlation.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   R/trust/03_spec_curve.R, R/trust/04_reference_marks.R,
#               R/trust/05_menu_deviations.R, R/trust/06_displays.R,
#               R/trust/check_parity.R, cases/trust/menu.qmd
################################################################################

# The same code runs in the downloadable workspace and in webR on the browser
# lab, so all three give the same numbers. The statistics come from packages:
# fixest fits the regression with the fixed effects absorbed and computes the
# errors clustered by country, parameters reads the coefficient, its error, t,
# df, p and 95% interval from each fit, datawizard averages the items of a
# scale, and effectsize converts t to the partial correlation of Multi100.
# R/trust/check_parity.R checks the fits against estimatr. The rows are chosen
# with dplyr, because datawizard evaluates its conditions in a fresh
# environment and so cannot see the arguments of a function.

# The menu of the curve, in menu order: every combination is one specification
# (4 x 4 x 3 x 2 x 2 = 192). Anything else is refused by check_spec() unless it
# is one of the off-curve options listed in OFF_CURVE.
SPEC_AXES <- list(
  outcome  = c("parliament", "politicians", "legal", "index"),
  controls = c("none", "demog", "outlook", "paper"),
  weights  = c("none", "design", "population"),
  items    = c("complete", "available"),
  fe       = c("country_round", "country_and_round")
)

# Settings that the curve holds fixed, with the alternatives that fit_spec()
# can also fit for the displays beside the curve. Where a list starts with the
# setting of the curve, that first option is it; `controls`, `weights` and `fe`
# list only the alternatives to the options of the curve.
OFF_CURVE <- list(
  controls = c("paper_no_unhappy"),
  weights = c("post", "post_population"),
  sample  = c("paper", "all_respondents"),
  concern = c("index", "economy", "culture", "place"),
  extra   = c("none", "gov_dissat"),
  cluster = c("country", "country_round"),
  fe      = c("pooled")
)

# Outcomes: distrust (0-10, the trust items reversed) in each institution, or
# the mean of the three.
OUTCOME_ITEMS <- list(
  parliament  = "distrust_parl",
  politicians = "distrust_pol",
  legal       = "distrust_legal",
  index       = c("distrust_parl", "distrust_pol", "distrust_legal")
)

CONCERN_ITEMS <- list(
  index   = c("imm_econ", "imm_culture", "imm_place"),
  economy = "imm_econ",
  culture = "imm_culture",
  place   = "imm_place"
)

# Adjustment sets, each nested in the next, and the set of the paper without
# unhappiness, which the Multi100 instruction of Task 2 required. `soc_distrust`
# is the mean of the three items of interpersonal distrust, built by the same
# rule as the scales.
CONTROL_TERMS <- list(
  none    = character(0),
  demog   = c("age", "female", "education", "hh_income_z"),
  outlook = c("age", "female", "education", "hh_income_z",
              "unhappy", "life_dissat", "meet_rarely", "soc_distrust", "left_right"),
  paper   = c("age", "female", "education", "hh_income_z",
              "unhappy", "life_dissat", "meet_rarely", "soc_distrust", "left_right",
              "econ_dissat", "income_difficulty", "health_dissat", "educ_dissat",
              "winner", "far_right_vote"),
  paper_no_unhappy = c("age", "female", "education", "hh_income_z",
                       "life_dissat", "meet_rarely", "soc_distrust", "left_right",
                       "econ_dissat", "income_difficulty", "health_dissat", "educ_dissat",
                       "winner", "far_right_vote")
)

# The specification closest to the model of the paper (Table 3, parliament)
REFERENCE <- list(outcome = "parliament", controls = "paper", weights = "none",
                  items = "complete", fe = "country_round")

check_spec <- function(outcome, controls, weights, items, fe,
                       sample = "paper", concern = "index", extra = "none", cluster = "country") {
  chosen <- list(outcome = outcome, controls = controls, weights = weights, items = items, fe = fe,
                 sample = sample, concern = concern, extra = extra, cluster = cluster)
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

# The mean of a set of 0-10 items. "complete" needs every item answered (the
# focal rule of the jp7tr reproduction); "available" averages whatever items
# were answered (its additional analysis, and the Multi100 file). A single item
# is its own scale, since datawizard::row_means() needs two columns or more.
scale_mean <- function(data, items, rule) {
  if (length(items) == 1) return(data[[items]])
  datawizard::row_means(data, select = items, min_valid = if (rule == "complete") length(items) else 1)
}

# The analysed rows of one specification: the sample, the outcome, the measure
# of concern, the controls and the weight, without the rows that miss any of them.
spec_frame <- function(ess, outcome = "parliament", controls = "paper", weights = "none",
                       items = "complete", sample = "paper", concern = "index", extra = "none") {
  terms <- c(CONTROL_TERMS[[controls]], if (extra != "none") extra)
  ess |>
    dplyr::filter(.env$sample == "all_respondents" | (citizen %in% 1 & minority %in% 0)) |>
    dplyr::mutate(
      y = scale_mean(dplyr::pick(dplyr::everything()), OUTCOME_ITEMS[[outcome]], items),
      x = scale_mean(dplyr::pick(dplyr::everything()), CONCERN_ITEMS[[concern]], items),
      soc_distrust = scale_mean(dplyr::pick(dplyr::everything()),
                                c("distrust_people", "unfair_people", "unhelpful_people"), items),
      w = switch(.env$weights, none = 1, design = dweight, population = dweight * pweight,
                 post = pspwght, post_population = pspwght * pweight),
      cr = paste0(cntry, essround)
    ) |>
    dplyr::select(cntry, essround, cr, y, x, w, dplyr::all_of(terms)) |>
    dplyr::filter(dplyr::if_all(dplyr::everything(), \(v) !is.na(v)))
}

# One specification: the coefficient of concern about immigration (points of
# distrust on the 0-10 scale per point of concern; positive = more concern goes
# with more distrust, the direction of the claim), its errors clustered by
# country (CR1, t on G - 1 df, the rule of the curve), the conventional test,
# the 95% interval under each, and the partial correlation by the Multi100 rule.
fit_spec <- function(ess, outcome = "parliament", controls = "paper", weights = "none",
                     items = "complete", fe = "country_round", sample = "paper",
                     concern = "index", extra = "none", cluster = "country") {
  check_spec(outcome, controls, weights, items, fe, sample, concern, extra, cluster)
  frame <- spec_frame(ess, outcome, controls, weights, items, sample, concern, extra)

  # An intercept for every country-round, or for every country with a dummy for
  # every round but the first, absorbed by fixest (Bergé, Butts and McDermott
  # 2026, arXiv:2601.21749) rather than estimated
  rhs <- c("x", CONTROL_TERMS[[controls]], if (extra != "none") extra,
           if (fe == "country_and_round") "factor(essround)")
  absorbed <- switch(fe, country_round = "| cr", country_and_round = "| cntry", pooled = "")
  form <- stats::as.formula(paste("y ~", paste(rhs, collapse = " + "), absorbed))
  fit <- fixest::feols(form, data = frame, weights = ~w, vcov = "iid")

  # Errors clustered by country (or country-round): CR1 with the default
  # small-sample correction of fixest, G / (G - 1) times (n - 1) / (n - K), where
  # K leaves out the intercepts nested in the clusters (Cameron and Miller 2015,
  # Journal of Human Resources), and t on G - 1 df
  clustered <- summary(fit, cluster = if (cluster == "country") ~cntry else ~cr) |>
    parameters::model_parameters() |>
    datawizard::data_filter(Parameter == "x")
  # Conventional (model-based) errors, with the absorbed intercepts in the df
  conventional <- parameters::model_parameters(fit) |>
    datawizard::data_filter(Parameter == "x")

  n <- stats::nobs(fit)
  data.frame(  # r-style: keep – a data frame prints every digit that the tasks quote
    outcome = outcome, controls = controls, weights = weights, items = items, fe = fe,
    sample = sample, concern = concern, extra = extra, cluster = cluster,
    b = clustered$Coefficient, se = clustered$SE, stat_type = "t", stat = clustered$t,
    df = clustered$df_error, p = clustered$p,
    se_conv = conventional$SE, t_conv = conventional$t, df_conv = conventional$df_error,
    p_conv = conventional$p,
    r = r_multi100(conventional$t, "t", conventional$df_error, n),
    # the coefficient in standard deviations of the analysed rows, unweighted;
    # effectsize::standardize_parameters(method = "basic") gives the same for
    # an unweighted fit but stops at the round dummies of a fixest fit
    b_std = clustered$Coefficient * stats::sd(frame$x) / stats::sd(frame$y),
    n = n, n_clusters = dplyr::n_distinct(if (cluster == "country") frame$cntry else frame$cr),
    n_country_rounds = dplyr::n_distinct(frame$cr),
    ci_low = clustered$CI_low, ci_high = clustered$CI_high,
    ci_conv_low = conventional$CI_low, ci_conv_high = conventional$CI_high
  )
}

# The conversion rules of Multi100 (standard_correlation_coefficient.R in
# github.com/marton-balazs-kovacs/multi100): t with its df, which is the rule of
# effectsize::t_to_r(); z with the sample size standing in for the missing df;
# F with one numerator df read as the square of a t.
r_multi100 <- function(stat, stat_type, df, n) {
  stat_type <- rep_len(stat_type, length(stat))
  dplyr::case_when(
    stat_type == "t" ~ effectsize::t_to_r(stat, df, ci = NULL)$r,
    stat_type == "z" ~ tanh(abs(stat) * sqrt(1 / (n - 3))) * sign(stat),
    stat_type == "F" ~ sqrt(stat / (stat + df))
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(outcome, controls, weights, items, fe) {
  sprintf("outcome=%s controls=%s weights=%s items=%s fe=%s", outcome, controls, weights, items, fe)
}
