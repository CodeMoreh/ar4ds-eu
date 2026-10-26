##############################################################################
# Title:        08_mi_tier.R
# Purpose:      The multiple-imputation options of the no-opinion decision. A
#               respondent who mentions none of the 13 items has an undefined
#               share on every framing scale; the zero, drop and split rules
#               settle that by fiat, while imputation treats the scales as
#               missing and fills them from what is known about the
#               respondent. Two engines are run so that the course can compare
#               them on the same case. mice imputes the four base shares by
#               fully conditional specification with predictive mean matching,
#               which keeps imputed values inside the observed range of a
#               bounded scale. Amelia imputes them jointly under a
#               multivariate normal model fitted by EM with bootstrapping, and
#               its draws are clipped to the unit interval afterwards. The two
#               composite scales are derived from the completed parts. Both
#               engines condition on age, sex, education and the country and
#               year, which is the missing-at-random assumption in operation:
#               given who the respondent is and where and when the respondent
#               was asked, whether the respondent answered is taken to be
#               unrelated to what the answer would have been.
#               Each of the m completed respondent files is aggregated to
#               country-year means, the estimand cells of the unemployment arm
#               are fitted on every one at the standard measurement settings
#               (share of all 13 items, cells as interviewed, World Bank
#               series, raw predictor, linear family), and the m estimates are
#               pooled by Rubin's rules with the degrees of freedom of Barnard
#               and Rubin (1999, Biometrika), through mice::pool.scalar().
#               The fraction of missing information is kept, since it says how
#               much of the uncertainty about a coefficient is due to the
#               no-opinion respondents. The run takes about half an hour.
#               Seed: 20260906, set once before the first engine; mice and
#               Amelia draw from the session stream in that order.
# Reads:        _planning_data/person_level.rds (GESIS-derived, local only);
#               in cases/eu-frames/data/, EUframes_cy.csv, teney_panel_weights.csv
# Writes:       in cases/eu-frames/data/, spec_grid_mi.csv (pooled results,
#               both engines, both error conventions) and mi_diagnostics.csv
#               (imputation summaries by engine)
# Author:       Chris Moreh
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_paths.R")
set.seed(20260906)
M_IMP <- 10

person <- readRDS(person_file)
base   <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)
wts    <- datawizard::data_read(out_path("teney_panel_weights.csv"), verbose = FALSE)

outcomes   <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
scales     <- c(mcosmo = "cosmo", mutil = "util", mcomm = "comm", mlib = "lib",
                mpos = "pos", mneg = "neg")
estimators <- c("fe_twoway", "fe_country", "re", "pooled_cl")
samples    <- c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES")
reg_wts    <- c("none", "n_cell", "n_eff", "pop")

## ---- the imputation data ---------------------------------------------------------
## The four base shares are set to NA where the respondent mentioned nothing
## and imputed; the two composites are their exact sums and are derived from
## the completed parts. Imputing the composites directly fails: the
## collinearity screen of mice drops an exact sum from the model and leaves it
## unimputed, and either engine would otherwise draw composites that disagree
## with their parts. Age and sex have a handful of missing values and are
## imputed alongside; the 6,024 missing values of education become an
## explicit "unknown" band instead, since a multinomial imputation model on
## 416,698 rows, fifty times over, would cost more than it could return for a
## covariate that is there only as context.
parts <- c("cosmo", "util", "comm", "lib")
with_composites <- \(d) datawizard::data_modify(d, pos = pmin(cosmo + util, 1),
                                                 neg = pmin(comm + lib, 1))

imp_dat <- person |>
  dplyr::transmute(cell = factor(paste(cntry, year, sep = "_")),
                   cntry = factor(cntry), year = factor(year),
                   age, female = as.numeric(female),
                   edu4 = factor(dplyr::coalesce(edu4, "unknown")),
                   answered = n_all > 0,
                   dplyr::across(dplyr::all_of(parts), \(x) dplyr::if_else(n_all > 0, x, NA_real_)))
n_miss <- sum(!imp_dat$answered)

## ---- engine 1: mice, predictive mean matching ---------------------------------
## Country and year enter as factors rather than the 270-level cell factor,
## which pmm handles poorly as a predictor; the two main effects carry the
## context.

mice_dat <- datawizard::data_select(imp_dat, exclude = c("cell", "answered"))
meth <- mice::make.method(mice_dat)
meth[c(parts, "age", "female")] <- "pmm"  # r-style: keep – the method vector of mice
pred <- mice::make.predictorMatrix(mice_dat)
imp_mice <- mice::mice(mice_dat, m = M_IMP, maxit = 5, method = meth,
                       predictorMatrix = pred, printFlag = FALSE)
mice_sets <- purrr::map(seq_len(M_IMP), \(i) {
  mice::complete(imp_mice, i) |>
    datawizard::data_modify(cntry = as.character(cntry), year = as.integer(as.character(year))) |>
    with_composites()
})

## ---- engine 2: Amelia, EM with bootstrapping ------------------------------------
## Amelia wants numeric or declared-nominal columns; country and year go in as
## nominal so that the context is in the model. Draws outside [0, 1] are
## clipped, the standard treatment of a normal model on a bounded scale.

am_dat <- imp_dat |>
  datawizard::data_select(exclude = c("cell", "answered")) |>
  datawizard::data_modify(edu4 = as.character(edu4), cntry = as.character(cntry),
                          year = as.character(year))
am <- Amelia::amelia(as.data.frame(am_dat), m = M_IMP, noms = c("cntry", "year", "edu4"),
                     p2s = 0, parallel = "no")
amelia_sets <- purrr::map(am$imputations, \(d) {
  d |>
    dplyr::mutate(dplyr::across(dplyr::all_of(parts), \(x) pmin(pmax(x, 0), 1)),
                  female = as.numeric(female), year = as.integer(year)) |>
    with_composites()
})

## ---- from completed respondent files to cell panels ------------------------------

to_panel <- function(d) {
  d |>
    dplyr::summarise(dplyr::across(dplyr::all_of(unname(scales)), mean), n_cell = dplyr::n(),
                     .by = c(cntry, year)) |>
    # dplyr::rename(), because datawizard::data_rename() drops the names of a
    # vector passed by name and then asks for a replacement
    dplyr::rename(!!!stats::setNames(unname(scales), names(scales))) |>
    dplyr::inner_join(datawizard::data_select(base, select = c("cntry", "year", "bailout",
                                                               pred = "unemp", copred_g = "growth")),
                      by = c("cntry", "year")) |>
    dplyr::left_join(datawizard::data_select(wts, select = c("cntry", "year", "pop_15plus", "n_eff")),
                     by = c("cntry", "year"))
}

# dplyr::filter(), because the bailout condition names the argument d
prep_sample <- function(d, s) {
  switch(s,
    all        = d,
    y2004_2008 = dplyr::filter(d, year <= 2008),
    y2009_2013 = dplyr::filter(d, year >= 2009),
    no_bailout = dplyr::filter(d, !cntry %in% unique(d$cntry[d$bailout == 1])),
    no_GR_ES   = dplyr::filter(d, !cntry %in% c("GR", "ES")))
}

## ---- the estimand cells, fitted on one completed panel ---------------------------
## Only the unemployment arm is imputed: the no-opinion rule is a property of
## the outcome, and the growth arm shares the outcome panel, so its imputed
## variant would follow from the same completed files.

fit_cells <- function(panel) {
  grid <- tidyr::expand_grid(copredictor = c("none", "other"), sample = samples,
                             estimator = estimators, reg_weight = reg_wts) |>
    datawizard::data_filter(!(estimator == "re" & reg_weight != "none"))
  purrr::pmap(grid, \(copredictor, sample, estimator, reg_weight) {
    d <- prep_sample(panel, sample) |> datawizard::data_modify(x = pred, copred = copred_g)
    rhs <- if (copredictor == "none") "x" else "x + copred"
    w <- switch(reg_weight, none = NULL, n_cell = d$n_cell, n_eff = d$n_eff, pop = d$pop_15plus)
    out <- tryCatch({
      if (estimator == "re") {
        purrr::map(outcomes, \(y) {
          m <- plm::plm(stats::as.formula(paste(y, "~", rhs)), data = d,
                        index = c("cntry", "year"), model = "random")
          # the model-based and the Arellano clustered standard errors of plm
          se_c <- lmtest::coeftest(m)["x", "Std. Error"]
          se_k <- lmtest::coeftest(m, vcov. = plm::vcovHC(m, method = "arellano",
                                                          cluster = "group"))["x", "Std. Error"]
          tibble::tibble(outcome = y, se = c("conventional", "clustered"),
                         estimate = stats::coef(m)[["x"]], std_error = c(se_c, se_k),
                         df = nrow(d) - length(stats::coef(m)), n = nrow(d))
        }) |>
          purrr::list_rbind()
      } else {
        fe <- switch(estimator, fe_twoway = " | cntry + year", fe_country = " | cntry",
                     pooled_cl = "")
        fml <- stats::as.formula(paste0("c(", paste(outcomes, collapse = ", "), ") ~ ", rhs, fe))
        m <- if (is.null(w)) fixest::feols(fml, data = d) else fixest::feols(fml, data = d, weights = w)
        purrr::map2(seq_along(outcomes), outcomes, \(i, y) {
          mi <- m[[i]]
          ci <- fixest::coeftable(mi, vcov = "iid")
          ck <- fixest::coeftable(mi, vcov = ~cntry)
          tibble::tibble(outcome = y, se = c("conventional", "clustered"),
                         estimate = ci["x", "Estimate"],
                         std_error = c(ci["x", "Std. Error"], ck["x", "Std. Error"]),
                         df = fixest::degrees_freedom(mi, type = "resid"), n = stats::nobs(mi))
        }) |>
          purrr::list_rbind()
      }
    }, error = \(e) tibble::tibble(outcome = outcomes, se = "conventional",
                                   estimate = NA_real_, std_error = NA_real_,
                                   df = NA_real_, n = NA_integer_))
    dplyr::bind_cols(tibble::tibble(copredictor, sample, estimator, reg_weight), out)
  }) |>
    purrr::list_rbind()
}

## ---- Rubin's rules across the m fits -----------------------------------------------
## mice::pool.scalar() pools one coefficient by Rubin's rules (Rubin 1987), with
## the degrees of freedom of Barnard and Rubin (1999, Biometrika) for a
## complete-data df of n - k, here the mean residual df of the m fits; it
## returns the total variance t, the df and the fraction of missing
## information fmi.

run_engine <- function(sets, engine) {
  purrr::imap(sets, \(d, i) fit_cells(to_panel(d)) |> dplyr::mutate(imp = i)) |>
    purrr::list_rbind() |>
    dplyr::summarise(pool = list(mice::pool.scalar(estimate, std_error^2, n = mean(df) + 1, k = 1)),
                     n = dplyr::first(n),
                     .by = c(outcome, copredictor, sample, estimator, reg_weight, se)) |>
    dplyr::mutate(engine = engine, .before = 1) |>
    dplyr::mutate(estimate = purrr::map_dbl(pool, "qbar"),
                  std_error = sqrt(purrr::map_dbl(pool, "t")),
                  statistic = estimate / std_error,
                  df = purrr::map_dbl(pool, "df"),
                  fmi = purrr::map_dbl(pool, "fmi"),
                  m = purrr::map_int(pool, "m")) |>
    dplyr::select(engine, outcome, copredictor, sample, estimator, reg_weight, se,
                  estimate, std_error, statistic, df, n, fmi, m)
}

# r is the conversion rule of Multi100 (standard_correlation_coefficient.R in
# marton-balazs-kovacs/multi100 on GitHub), computed by effectsize::t_to_r()
pooled <- dplyr::bind_rows(run_engine(mice_sets, "mice_pmm"), run_engine(amelia_sets, "amelia_emb")) |>
  dplyr::mutate(predictor = "unemp", construction = "share", item_set = "all", noop = "mi",
                cell_weight = "unweighted", measure = "wb_nat", pred_form = "raw",
                family = "gaussian", positive_outcome = outcome %in% c("mcosmo", "mutil", "mpos"),
                p = 2 * stats::pt(-abs(statistic), df), sig = p < 0.05,
                supports_claim = dplyr::if_else(positive_outcome, estimate < 0, estimate > 0),
                r = effectsize::t_to_r(statistic, df)$r,
                r_ca = r * dplyr::if_else(positive_outcome, 1, -1))

datawizard::data_write(pooled, out_path("spec_grid_mi.csv"), verbose = FALSE)

## ---- diagnostics for the missing-answers page ---------------------------------------

diag <- dplyr::bind_rows(
  tibble::tibble(engine = "mice_pmm", quantity = "imputed respondents", value = n_miss),
  tibble::tibble(engine = "mice_pmm", quantity = "iterations", value = imp_mice$iteration),
  purrr::map(unname(scales), \(v) tibble::tibble(
    engine = c("mice_pmm", "amelia_emb"),
    quantity = paste0("mean imputed ", v),
    value = c(mean(unlist(purrr::map(mice_sets, \(d) d[[v]][!imp_dat$answered]))),
              mean(unlist(purrr::map(amelia_sets, \(d) d[[v]][!imp_dat$answered])))))) |>
    purrr::list_rbind(),
  purrr::map(unname(scales), \(v) tibble::tibble(
    engine = "observed", quantity = paste0("mean observed ", v),
    value = mean(person[[v]][person$n_all > 0]))) |>
    purrr::list_rbind(),
  # the four parts sum to exactly 1 for every respondent who answered; among
  # the imputed rows the sum says how far each engine strays from the simplex
  tibble::tibble(
    engine = c("mice_pmm", "amelia_emb"), quantity = "mean four-part sum, imputed rows",
    value = c(mean(unlist(purrr::map(mice_sets, \(d)
                datawizard::row_sums(d[!imp_dat$answered, ], select = parts)))),
              mean(unlist(purrr::map(amelia_sets, \(d)
                datawizard::row_sums(d[!imp_dat$answered, ], select = parts)))))),
  tibble::tibble(engine = "amelia_emb", quantity = "clipped draws share",
                 value = mean(unlist(purrr::map(am$imputations, \(d)
                   as.vector(as.matrix(d[!imp_dat$answered, parts]) < 0 |
                             as.matrix(d[!imp_dat$answered, parts]) > 1)))))
)
datawizard::data_write(diag, out_path("mi_diagnostics.csv"), verbose = FALSE)

pooled |>
  datawizard::data_filter(outcome == "mcosmo" & copredictor == "none" & estimator == "fe_twoway" &
                            sample == "all" & reg_weight == "none" & se == "conventional") |>
  datawizard::data_select(select = c("engine", "estimate", "statistic", "df", "fmi")) |>
  print()
