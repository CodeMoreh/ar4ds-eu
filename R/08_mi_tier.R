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
#               pooled by Rubin's rules with Barnard-Rubin degrees of freedom.
#               The fraction of missing information is kept, since it says how
#               much of the uncertainty about a coefficient is due to the
#               no-opinion respondents. The run takes about half an hour.
#               Seed: 20260906, set once before the first engine; mice and
#               Amelia draw from the session stream in that order.
# Reads:        _planning_data/person_level.rds (GESIS-derived, local only),
#               data/EUframes_cy.csv, data/teney_panel_weights.csv
# Writes:       data/spec_grid_mi.csv (pooled results, both engines, both
#               error conventions), data/mi_diagnostics.csv (imputation
#               summaries by engine)
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(fixest); library(plm)
  library(mice); library(Amelia)
})
source("R/_paths.R")
set.seed(20260906)
M_IMP <- 10

person <- readRDS(person_file)
base   <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)
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
with_composites <- \(d) mutate(d, pos = pmin(cosmo + util, 1), neg = pmin(comm + lib, 1))

imp_dat <- person |>
  transmute(cell = factor(paste(cntry, year, sep = "_")),
            cntry = factor(cntry), year = factor(year),
            age, female = as.numeric(female),
            edu4 = factor(coalesce(edu4, "unknown")),
            answered = n_all > 0,
            across(all_of(parts), \(x) if_else(n_all > 0, x, NA_real_)))
n_miss <- sum(!imp_dat$answered)

## ---- engine 1: mice, predictive mean matching ---------------------------------
## Country and year enter as factors rather than the 270-level cell factor,
## which pmm handles poorly as a predictor; the two main effects carry the
## context.

mice_dat <- imp_dat |> select(-cell, -answered)
meth <- make.method(mice_dat)
meth[c(parts, "age", "female")] <- "pmm"  # r-style: keep – the method vector of mice
pred <- make.predictorMatrix(mice_dat)
imp_mice <- mice(mice_dat, m = M_IMP, maxit = 5, method = meth,
                 predictorMatrix = pred, printFlag = FALSE)
mice_sets <- map(seq_len(M_IMP), \(i) {
  complete(imp_mice, i) |>
    mutate(cntry = as.character(cntry), year = as.integer(as.character(year))) |>
    with_composites()
})

## ---- engine 2: Amelia, EM with bootstrapping ------------------------------------
## Amelia wants numeric or declared-nominal columns; country and year go in as
## nominal so that the context is in the model. Draws outside [0, 1] are
## clipped, the standard treatment of a normal model on a bounded scale.

am_dat <- imp_dat |>
  select(-cell, -answered) |>
  mutate(edu4 = as.character(edu4), cntry = as.character(cntry), year = as.character(year))
am <- amelia(as.data.frame(am_dat), m = M_IMP, noms = c("cntry", "year", "edu4"),
             p2s = 0, parallel = "no")
amelia_sets <- map(am$imputations, \(d) {
  d |> mutate(across(all_of(parts), \(x) pmin(pmax(x, 0), 1)),
              female = as.numeric(female), year = as.integer(year)) |>
    with_composites()
})

## ---- from completed respondent files to cell panels ------------------------------

to_panel <- function(d) {
  d |>
    summarise(across(all_of(unname(scales)), mean), n_cell = n(), .by = c(cntry, year)) |>
    rename(!!!setNames(unname(scales), names(scales))) |>
    inner_join(base |> select(cntry, year, bailout, pred = unemp, copred_g = growth),
               by = c("cntry", "year")) |>
    left_join(wts |> select(cntry, year, pop_15plus, n_eff), by = c("cntry", "year"))
}

prep_sample <- function(d, s) {
  switch(s,
    all        = d,
    y2004_2008 = filter(d, year <= 2008),
    y2009_2013 = filter(d, year >= 2009),
    no_bailout = filter(d, !cntry %in% unique(d$cntry[d$bailout == 1])),
    no_GR_ES   = filter(d, !cntry %in% c("GR", "ES")))
}

## ---- the estimand cells, fitted on one completed panel ---------------------------
## Only the unemployment arm is imputed: the no-opinion rule is a property of
## the outcome, and the growth arm shares the outcome panel, so its imputed
## variant would follow from the same completed files.

fit_cells <- function(panel) {
  grid <- expand_grid(copredictor = c("none", "other"), sample = samples,
                      estimator = estimators, reg_weight = reg_wts) |>
    filter(!(estimator == "re" & reg_weight != "none"))
  pmap(grid, \(copredictor, sample, estimator, reg_weight) {
    d <- prep_sample(panel, sample) |> mutate(x = pred, copred = copred_g)
    rhs <- if (copredictor == "none") "x" else "x + copred"
    w <- switch(reg_weight, none = NULL, n_cell = d$n_cell, n_eff = d$n_eff, pop = d$pop_15plus)
    out <- tryCatch({
      if (estimator == "re") {
        map(outcomes, \(y) {
          m <- plm(as.formula(paste(y, "~", rhs)), data = d, index = c("cntry", "year"),
                   model = "random")
          b <- coef(m)[["x"]]
          se_c <- sqrt(diag(vcov(m)))[["x"]]
          se_k <- sqrt(diag(vcovHC(m, method = "arellano", cluster = "group")))[["x"]]
          tibble(outcome = y, se = c("conventional", "clustered"), estimate = b,
                 std_error = c(se_c, se_k), df = nrow(d) - length(coef(m)), n = nrow(d))
        }) |> list_rbind()
      } else {
        fe <- switch(estimator, fe_twoway = " | cntry + year", fe_country = " | cntry",
                     pooled_cl = "")
        fml <- as.formula(paste0("c(", paste(outcomes, collapse = ", "), ") ~ ", rhs, fe))
        m <- if (is.null(w)) feols(fml, data = d) else feols(fml, data = d, weights = w)
        map2(seq_along(outcomes), outcomes, \(i, y) {
          mi <- m[[i]]
          ci <- coeftable(mi, vcov = "iid"); ck <- coeftable(mi, vcov = ~cntry)
          tibble(outcome = y, se = c("conventional", "clustered"),
                 estimate = ci["x", "Estimate"],
                 std_error = c(ci["x", "Std. Error"], ck["x", "Std. Error"]),
                 df = degrees_freedom(mi, type = "resid"), n = nobs(mi))
        }) |> list_rbind()
      }
    }, error = \(e) tibble(outcome = outcomes, se = "conventional",
                           estimate = NA_real_, std_error = NA_real_,
                           df = NA_real_, n = NA_integer_))
    bind_cols(tibble(copredictor, sample, estimator, reg_weight), out)
  }) |> list_rbind()
}

## ---- Rubin's rules across the m fits -----------------------------------------------

pool_rubin <- function(fits) {
  fits |>
    summarise(m = n(),
              qbar = mean(estimate), ubar = mean(std_error^2), b = var(estimate),
              df_c = mean(df), n = first(n), .by = c(outcome, copredictor, sample,
                                                       estimator, reg_weight, se)) |>
    mutate(t_var = ubar + (1 + 1 / m) * b,
           std_error = sqrt(t_var),
           statistic = qbar / std_error,
           r_m = (1 + 1 / m) * b / ubar,
           fmi = (r_m + 2 / (df_c + 3)) / (r_m + 1),
           df_old = (m - 1) / r_m^2,
           df_obs = (df_c + 1) / (df_c + 3) * df_c * (1 - fmi),
           df = if_else(is.finite(df_old), df_old * df_obs / (df_old + df_obs), df_obs),
           estimate = qbar) |>
    select(outcome, copredictor, sample, estimator, reg_weight, se,
           estimate, std_error, statistic, df, n, fmi, m)
}

run_engine <- function(sets, engine) {
  fits <- imap(sets, \(d, i) fit_cells(to_panel(d)) |> mutate(imp = i)) |> list_rbind()
  pool_rubin(fits) |> mutate(engine = engine, .before = 1)
}

pooled <- bind_rows(run_engine(mice_sets, "mice_pmm"), run_engine(amelia_sets, "amelia_emb")) |>
  mutate(predictor = "unemp", construction = "share", item_set = "all", noop = "mi",
         cell_weight = "unweighted", measure = "wb_nat", pred_form = "raw", family = "gaussian",
         positive_outcome = outcome %in% c("mcosmo", "mutil", "mpos"),
         p = 2 * pt(-abs(statistic), df), sig = p < 0.05,
         supports_claim = if_else(positive_outcome, estimate < 0, estimate > 0),
         r = statistic / sqrt(statistic^2 + df),
         r_ca = r * if_else(positive_outcome, 1, -1))

datawizard::data_write(pooled, out_path("spec_grid_mi.csv"), verbose = FALSE)

## ---- diagnostics for the missing-answers page ---------------------------------------

diag <- bind_rows(
  tibble(engine = "mice_pmm", quantity = "imputed respondents", value = n_miss),
  tibble(engine = "mice_pmm", quantity = "iterations", value = imp_mice$iteration),
  map(unname(scales), \(v) tibble(
    engine = c("mice_pmm", "amelia_emb"),
    quantity = paste0("mean imputed ", v),
    value = c(mean(unlist(map(mice_sets, \(d) d[[v]][!imp_dat$answered]))),
              mean(unlist(map(amelia_sets, \(d) d[[v]][!imp_dat$answered])))))) |>
    list_rbind(),
  map(unname(scales), \(v) tibble(
    engine = "observed", quantity = paste0("mean observed ", v),
    value = mean(person[[v]][person$n_all > 0]))) |> list_rbind(),
  # the four parts sum to exactly 1 for every respondent who answered; among
  # the imputed rows the sum says how far each engine strays from the simplex
  tibble(engine = c("mice_pmm", "amelia_emb"), quantity = "mean four-part sum, imputed rows",
         value = c(mean(unlist(map(mice_sets, \(d) rowSums(d[!imp_dat$answered, parts])))),
                   mean(unlist(map(amelia_sets, \(d) rowSums(d[!imp_dat$answered, parts])))))),
  tibble(engine = "amelia_emb", quantity = "clipped draws share",
         value = mean(unlist(map(am$imputations, \(d)
           as.vector(as.matrix(d[!imp_dat$answered, parts]) < 0 |
                     as.matrix(d[!imp_dat$answered, parts]) > 1)))))
)
datawizard::data_write(diag, out_path("mi_diagnostics.csv"), verbose = FALSE)

pooled |>
  filter(outcome == "mcosmo", copredictor == "none", estimator == "fe_twoway",
         sample == "all", reg_weight == "none", se == "conventional") |>
  select(engine, estimate, statistic, df, fmi) |>
  print()
