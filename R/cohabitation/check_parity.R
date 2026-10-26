################################################################################
# Title:        check_parity.R
# Purpose:      Checks the estimator of R/cohabitation/_tools.R, which fits with
#               survey, nnet and svyVGAM, against routes that share none of its
#               fitting code: the multinomial coefficients and conventional
#               errors against nnet alone, the binary ones against VGAM and
#               estimatr; the design-based errors against jackknife replicate
#               weights of survey (a different variance estimator, so they agree
#               only closely); the degrees of freedom against a count of the
#               clusters and strata; the life table against the cumulative
#               incidence of cmprsk; the person-months against their count;
#               and the committed curve against a fresh fit.
# Reads:        cases/cohabitation/data/cohab_unions.csv,
#               cases/cohabitation/data/spec_curve.csv,
#               cases/cohabitation/data/display_lifetable.csv
# Writes:       nothing; stops on any disagreement (about 15 minutes, most of
#               it the jackknife of the multinomial model)
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/cohabitation/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" and
# the codes of the cases, strata and clusters as numbers
unions <- readr::read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                          col_types = readr::cols(caseid = readr::col_character(),
                                                  stratum = readr::col_character(),
                                                  psu = readr::col_character()))
curve <- readr::read_csv("cases/cohabitation/data/spec_curve.csv", show_col_types = FALSE,
                         col_types = readr::cols(stat_type = readr::col_character()))
lifetable <- readr::read_csv("cases/cohabitation/data/display_lifetable.csv", show_col_types = FALSE)

# The analysed person-months of a specification with the terms of its model.
# The columns are divided by their largest value, which changes no coefficient
# of `later` but keeps the numerical Hessian of nnet accurate (unscaled, age
# squared runs to about 1,800 beside 0/1 columns and costs it about 0.2% of the
# error of `later`). dplyr in the body: datawizard cannot see the arguments.
check_frame <- function(cohorts, horizon, adjust, weights, time) {
  frame <- spec_frame(unions, cohorts, horizon, weights) |>
    dplyr::mutate(y = factor(y, levels = c("intact", "marriage", "separation")),
                  marry = as.numeric(y == "marriage"),
                  dplyr::across(c("month", dplyr::all_of(ADJUST_TERMS[[adjust]])), \(v) v / max(abs(v))))
  rhs <- c("later", ADJUST_TERMS[[adjust]], switch(time, linear = "month", yearly = "factor(year_of_union)"))
  list(frame = frame, rhs = rhs)
}

# The multinomial logit by nnet alone (Venables and Ripley 2002), converged
# tightly: the coefficient of `later` and its conventional error
nnet_ref <- function(cohorts, horizon, adjust, weights, time) {
  cf <- check_frame(cohorts, horizon, adjust, weights, time)
  co <- nnet::multinom(stats::reformulate(cf$rhs, response = "y"), data = cf$frame, weights = w,
                       trace = FALSE, maxit = 2000, reltol = 1e-14, abstol = 1e-14) |>
    parameters::model_parameters(keep = "^later$") |>
    datawizard::data_filter(Response == "marriage")
  tibble::tibble(b_ref = co$Coefficient, se_model_ref = co$SE)
}

# The logit and complementary log-log by VGAM (Yee 2015), the linear
# probability model by estimatr (classical errors)
binary_ref <- function(cohorts, horizon, adjust, weights, time, model) {
  cf <- check_frame(cohorts, horizon, adjust, weights, time)
  form <- stats::reformulate(cf$rhs, response = "marry")
  if (model == "lpm") {
    fit <- estimatr::lm_robust(form, data = cf$frame, weights = w, se_type = "classical")
    return(tibble::tibble(b_ref = fit$coefficients[["later"]], se_model_ref = fit$std.error[["later"]]))
  }
  link <- if (model == "logit") "logitlink" else "clogloglink"
  fit <- VGAM::vglm(form, family = VGAM::binomialff(link = link), data = cf$frame, weights = w,
                    epsilon = 1e-12, criterion = "coefficients")
  tibble::tibble(b_ref = stats::coef(fit)[["later"]], se_model_ref = sqrt(stats::vcov(fit)["later", "later"]))
}

# Every fourth specification of the curve, and three fits off it
checked <- curve |>
  datawizard::data_filter(spec_id %% 4 == 1) |>
  datawizard::data_select(c("cohorts", "horizon", "adjust", "weights", "time", "model", "b", "se_model"))
off_curve <- tibble::tribble(
  ~cohorts, ~horizon,     ~adjust,       ~weights,   ~time,    ~model,
  "paper",  "five_years", "composition", "weighted", "linear", "lpm",
  "paper",  "five_years", "race_period", "weighted", "linear", "logit",
  "paper",  "five_years", "race_period", "weighted", "linear", "mlogit"
) |>
  dplyr::mutate(fit = purrr::pmap(dplyr::pick(dplyr::everything()), \(...) fit_spec(unions, ...)),
                b = purrr::map_dbl(fit, "b"), se_model = purrr::map_dbl(fit, "se_model")) |>
  datawizard::data_remove("fit")
point <- dplyr::bind_rows(checked, off_curve) |>
  dplyr::mutate(ref = purrr::pmap(dplyr::pick(cohorts:model), \(cohorts, horizon, adjust, weights, time, model)
    if (model == "mlogit") nnet_ref(cohorts, horizon, adjust, weights, time)
    else binary_ref(cohorts, horizon, adjust, weights, time, model))) |>
  tidyr::unnest(ref)

# The design-based errors of the reference specification, multinomial and
# logit, against jackknife replicate weights (JKn, one replicate per cluster
# left out; Lumley 2004): the survey design of the two surveys declared on all
# their clusters, the replicates built from it, and the analysed months kept
jackknife <- function(model) {
  cf <- check_frame("paper", "five_years", "composition", "weighted", "linear")
  clusters <- unions |>
    dplyr::filter(cycle %in% cf$frame$cycle) |>
    dplyr::distinct(stratum, psu) |>
    dplyr::mutate(w = 1, keep = FALSE)
  replicates <- survey::svydesign(ids = ~psu, strata = ~stratum, weights = ~w, nest = TRUE,
                                  data = dplyr::bind_rows(dplyr::mutate(cf$frame, keep = TRUE), clusters)) |>
    survey::as.svrepdesign(type = "JKn") |>
    subset(keep)  # r-style: keep – the subset method of a survey design
  if (model == "logit") {
    fit <- survey::svyglm(stats::reformulate(cf$rhs, response = "marry"), design = replicates,
                          family = stats::quasibinomial())
    return(survey::SE(fit)[["later"]])
  }
  # VGAM stops on rows of zero weight, which a replicate gives to the cluster
  # that it leaves out; those rows add nothing to the likelihood, so each
  # replicate is fitted without them
  form <- stats::reformulate(cf$rhs, response = "y")
  est <- survey::withReplicates(replicates, \(wts, data) {
    kept <- data |>
      dplyr::mutate(.rw = wts) |>
      dplyr::filter(.rw > 0)
    VGAM::vglm(form, family = VGAM::multinomial(refLevel = "intact"), data = kept,
               weights = .rw, epsilon = 1e-10, criterion = "coefficients") |>
      stats::coef() |>
      purrr::pluck("later:1")
  })
  survey::SE(est)[[1]]
}
ref_curve <- datawizard::data_filter(curve, is_reference)
ref_logit <- datawizard::data_filter(curve, cohorts == "paper", horizon == "five_years", adjust == "composition",
                                     weights == "weighted", time == "linear", model == "logit")
jk <- tibble::tibble(model = c("mlogit", "logit"), se = c(ref_curve$se, ref_logit$se),
                     se_jk = purrr::map_dbl(model, jackknife))

# The degrees of freedom of every twelfth specification: its clusters, less its
# strata, less its coefficients, plus one, counted from the analysed months
df_count <- curve |>
  datawizard::data_filter(spec_id %% 12 == 1) |>
  dplyr::mutate(df_count = purrr::pmap_dbl(dplyr::pick(cohorts:model), \(cohorts, horizon, adjust, weights, time, model) {
    frame <- spec_frame(unions, cohorts, horizon, weights)
    k <- 2 + length(ADJUST_TERMS[[adjust]]) + if (time == "linear") 1 else dplyr::n_distinct(frame$year_of_union) - 1
    dplyr::n_distinct(frame$psu) - dplyr::n_distinct(frame$stratum) - k * (if (model == "mlogit") 2 else 1) + 1
  }))

# The person-months of each horizon against their count from the unions
months_count <- purrr::map_lgl(SPEC_AXES$horizon, \(h) {
  u <- spec_unions(unions, "five_years")
  last <- if (h == "three_years") 36 else Inf
  pm <- person_months(u, h)
  nrow(pm) == sum(pmin(u$duration + 1, last)) &&
    sum(pm$y == "marriage") == sum(u$outcome == "marriage" & u$duration + 1 <= last)
})

# The life table without weights against the cumulative incidence of cmprsk
# (Gray 1988), which takes the same unions, times and outcomes
paper <- spec_unions(unions) |>
  datawizard::data_modify(months = duration + 1,
                          ended = factor(outcome, levels = c("intact", "marriage", "separation")),
                          cohort = dplyr::if_else(later == 1, "2005-2010", "1990-1995"))
aj <- survival::survfit(survival::Surv(months, ended) ~ cohort, data = paper) |>
  summary(times = c(12, 24, 36, 48))
gray <- cmprsk::cuminc(paper$months, as.integer(paper$ended) - 1, group = paper$cohort, cencode = 0) |>
  cmprsk::timepoints(times = c(12, 24, 36, 48))
lifetable_gap <- max(abs(c(t(gray$est[c("1990-1995 1", "2005-2010 1"), ])) - aj$pstate[, 2]),
                     abs(c(t(gray$est[c("1990-1995 2", "2005-2010 2"), ])) - aj$pstate[, 3]))
# the weights of the life table: the weighted unions at risk in month 1 are
# all the weighted unions of each cohort
weights_at_start <- paper |>
  datawizard::data_summary(w = sum(weight), by = "cohort") |>
  dplyr::left_join(datawizard::data_filter(lifetable, group_type == "all", month == 1), by = "cohort")

# The committed curve against a fresh fit of the estimator
fresh <- curve |>
  datawizard::data_filter(spec_id %% 6 == 1) |>
  dplyr::mutate(new = purrr::pmap(dplyr::pick(cohorts:model), \(...) fit_spec(unions, ...)[, c("b", "stat", "r")])) |>
  tidyr::unnest(new, names_sep = "_")

gaps <- tibble::tibble(
  b_vs_independent = max(abs(point$b / point$b_ref - 1)),
  se_model_vs_independent = max(abs(point$se_model / point$se_model_ref - 1)),
  se_vs_jackknife = max(abs(jk$se / jk$se_jk - 1)),
  df_vs_count = max(abs(df_count$df - df_count$df_count)),
  months_counted = all(months_count),
  lifetable_vs_cuminc = lifetable_gap,
  lifetable_weights = max(abs(weights_at_start$w / weights_at_start$w_at_risk - 1)),
  b_vs_fresh = max(abs(fresh$b - fresh$new_b)),
  stat_vs_fresh = max(abs(fresh$stat - fresh$new_stat))
)
print(gaps, width = Inf)
print(jk)

# Jackknife and linearisation are different estimators of the same variance,
# and with about 380 clusters they agree to within a few per cent
stopifnot(gaps$b_vs_independent < 1e-5, gaps$se_model_vs_independent < 1e-4,
          gaps$se_vs_jackknife < 0.03, gaps$df_vs_count == 0, gaps$months_counted,
          gaps$lifetable_vs_cuminc < 1e-9, gaps$lifetable_weights < 1e-9,
          gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on", nrow(point), "fits against nnet, VGAM and estimatr, the design-based errors of",  # r-style: keep – a checker, whose output is its report
    nrow(jk), "fits against jackknife replicates,", nrow(df_count), "df against a count, the person-months,",
    "the life table against cmprsk and", nrow(fresh), "rows of the curve against a fresh fit.\n")
