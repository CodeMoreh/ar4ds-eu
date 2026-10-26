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
#               R/trust/check_parity.R
################################################################################

# The fits use base R, dplyr and purrr only, on purpose: the same code runs in the
# downloadable workspace and in webR on the browser lab. Fixed effects are
# absorbed by subtracting group means, so a fit on 110,000 respondents needs no
# dummy matrix. R/trust/check_parity.R checks the results against fixest and
# sandwich.

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
# were answered (its additional analysis, and the Multi100 file).
scale_mean <- function(data, items, rule) {
  m <- as.matrix(data[, items, drop = FALSE])
  if (rule == "complete") return(rowMeans(m))
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA
  out
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

# Weighted group means subtracted from every column of a matrix
demean <- function(m, g, w) {
  means <- rowsum(w * m, g, reorder = FALSE) / as.vector(rowsum(w, g, reorder = FALSE))
  m - means[match(g, rownames(means)), , drop = FALSE]
}

# One specification: the coefficient of concern about immigration (points of
# distrust on the 0-10 scale per point of concern; positive = more concern goes
# with more distrust, the direction of the claim), its errors clustered by
# country (CR1, t on G - 1 df, the rule of the curve), the conventional test,
# and the partial correlation by the Multi100 rule.
fit_spec <- function(ess, outcome = "parliament", controls = "paper", weights = "none",
                     items = "complete", fe = "country_round", sample = "paper",
                     concern = "index", extra = "none", cluster = "country") {
  check_spec(outcome, controls, weights, items, fe, sample, concern, extra, cluster)
  frame <- spec_frame(ess, outcome, controls, weights, items, sample, concern, extra)
  terms <- c("x", CONTROL_TERMS[[controls]], if (extra != "none") extra)
  x <- as.matrix(frame[, terms])
  w <- frame$w

  # Round dummies for every round present but the first, so that a subset of
  # rounds still fits
  if (fe == "country_and_round") {
    shifts <- sort(unique(frame$essround))[-1]
    rounds <- outer(frame$essround, shifts, `==`) * 1
    colnames(rounds) <- paste0("round", shifts)
    x <- cbind(x, rounds)
  }
  absorbed <- switch(fe, country_round = frame$cr, country_and_round = frame$cntry,
                     pooled = rep("all", nrow(frame)))
  m <- demean(cbind(y = frame$y, x), absorbed, w)
  yt <- m[, 1]
  xt <- m[, -1, drop = FALSE]

  bread <- solve(crossprod(xt, w * xt))
  beta <- drop(bread %*% crossprod(xt, w * yt))
  e <- yt - drop(xt %*% beta)
  n <- nrow(xt)
  k <- ncol(xt)
  n_fe <- length(unique(absorbed))

  # Conventional (model-based) errors, with the absorbed groups in the df
  df_conv <- n - k - n_fe
  se_conv <- sqrt(sum(w * e^2) / df_conv * bread[1, 1])

  # Cluster-robust CR1 errors. Groups absorbed inside the clusters do not count
  # in K, apart from one that stands for the intercept (the default of fixest).
  groups <- if (cluster == "country") frame$cntry else frame$cr
  n_cl <- length(unique(groups))
  scores <- rowsum(xt * (w * e), groups)
  k_ssc <- k + 1
  adj <- n_cl / (n_cl - 1) * (n - 1) / (n - k_ssc)
  se <- sqrt(adj * (bread %*% crossprod(scores) %*% bread)[1, 1])

  b <- beta[[1]]
  t_cl <- b / se
  t_conv <- b / se_conv
  data.frame(
    outcome = outcome, controls = controls, weights = weights, items = items, fe = fe,
    sample = sample, concern = concern, extra = extra, cluster = cluster,
    b = b, se = se, stat_type = "t", stat = t_cl, df = n_cl - 1,
    p = 2 * stats::pt(-abs(t_cl), n_cl - 1),
    se_conv = se_conv, t_conv = t_conv, df_conv = df_conv,
    p_conv = 2 * stats::pt(-abs(t_conv), df_conv),
    r = r_multi100(t_conv, "t", df_conv, n),
    b_std = b * stats::sd(frame$x) / stats::sd(frame$y),
    n = n, n_clusters = n_cl, n_country_rounds = length(unique(frame$cr))
  )
}

# The conversion rules of Multi100 (standard_correlation_coefficient.R): t with
# its df; z with the sample size standing in for the missing df; F with one
# numerator df read as the square of a t.
r_multi100 <- function(stat, stat_type, df, n) {
  stat_type <- rep_len(stat_type, length(stat))
  dplyr::case_when(
    stat_type == "t" ~ stat / sqrt(stat^2 + df),
    stat_type == "z" ~ tanh(abs(stat) * sqrt(1 / (n - 3))) * sign(stat),
    stat_type == "F" ~ sqrt(stat / (stat + df))
  )
}

# The free-text label that carries a specification to the class chart.
spec_line <- function(outcome, controls, weights, items, fe) {
  sprintf("outcome=%s controls=%s weights=%s items=%s fe=%s", outcome, controls, weights, items, fe)
}
