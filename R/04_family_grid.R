##############################################################################
# Title:        04_family_grid.R
# Purpose:      Builds the family universe of 1,680 aggregated
#               specifications: the outcome-family axis doubles the canonical
#               840. The Gaussian half (840 rows) is imported from
#               spec_grid.csv; the beta half (840 rows) is fitted as the
#               logit-link twin of every cell, since the country-year outcomes
#               are proportions in (0, 1). Conventions follow 03_spec_grid.R:
#               the same six axes, the same exclusion of random effects with
#               weights, the statistic and df under the definition of each
#               estimator, r = stat / sqrt(stat^2 + df), interval and
#               significance via qt.
#               Estimators of the beta half: pooled_cl and the two
#               fixed-effects designs use betareg (fixed effects as country
#               and year dummies) – pooled_cl with cluster-robust (sandwich
#               vcovCL, by country) z statistics, the fixed-effects designs
#               with model-based z; re uses the beta family of glmmTMB with a
#               country random intercept (df = n minus the fixed coefficients,
#               the plm rule). n_cy weights enter betareg as case weights
#               normalised to sum to the sample size, so that they act as
#               relative precision weights rather than inflating the effective
#               N of the likelihood. The family axis was not part of the
#               Multi100 menu.
# Reads:        data/EUframes_cy.csv, data/spec_grid.csv
# Writes:       data/spec_grid_family.csv (840 Gaussian + 840 beta rows)
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr)
  library(betareg); library(glmmTMB); library(sandwich); library(lmtest)
})
source("R/_paths.R")

panel <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)
gauss <- datawizard::data_read(out_path("spec_grid.csv"), verbose = FALSE)

outcomes     <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
pred_forms   <- c("raw", "log")
copredictors <- c("none", "growth")
estimators   <- c("fe_twoway", "fe_country", "re", "pooled_cl")
samples      <- c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES")
weights_o    <- c("none", "n_cy")

grid <- expand_grid(outcome = outcomes, pred_form = pred_forms, copredictor = copredictors,
                    estimator = estimators, sample = samples, weights = weights_o) |>
  filter(!(estimator == "re" & weights == "n_cy"))

prep_sample <- function(d, s) {
  switch(s,
    all        = d,
    y2004_2008 = filter(d, year <= 2008),
    y2009_2013 = filter(d, year >= 2009),
    no_bailout = filter(d, !cntry %in% unique(d$cntry[d$bailout == 1])),
    no_GR_ES   = filter(d, !cntry %in% c("GR", "ES")))
}

prep_pred_form <- function(d, f) {
  d |> mutate(x = switch(f, raw = unemp, log = log(unemp)))
}

fit_one_beta <- function(row) {
  d <- panel |>
    prep_sample(row$sample) |>
    prep_pred_form(row$pred_form) |>
    mutate(y = .data[[row$outcome]],
           w_raw = if (row$weights == "n_cy") n_cy else 1,
           w = w_raw * n() / sum(w_raw))   # normalised case weights
  fml_rhs <- if (row$copredictor == "growth") "x + growth" else "x"

  out <- tryCatch({
    if (row$estimator == "re") {
      m <- glmmTMB(as.formula(paste("y ~", fml_rhs, "+ (1 | cntry)")),
                   family = beta_family(), data = d)
      s <- summary(m)$coefficients$cond
      est <- s["x", 1]; se <- s["x", 2]; stat <- s["x", 3]
      dfres <- nrow(d) - length(fixef(m)$cond)
    } else {
      fml <- switch(row$estimator,
        fe_twoway  = as.formula(paste("y ~", fml_rhs, "+ factor(cntry) + factor(year)")),
        fe_country = as.formula(paste("y ~", fml_rhs, "+ factor(cntry)")),
        pooled_cl  = as.formula(paste("y ~", fml_rhs)))
      m <- betareg(fml, data = d, weights = d$w)
      ct <- if (row$estimator == "pooled_cl") {
        coeftest(m, vcov = vcovCL(m, cluster = d$cntry))
      } else {
        coeftest(m)
      }
      est <- ct["x", "Estimate"]; se <- ct["x", "Std. Error"]; stat <- ct["x", "z value"]
      dfres <- df.residual(m)
    }
    r <- stat / sqrt(stat^2 + dfres)
    tibble(estimate = est, se = se, statistic = stat, df = dfres,
           n = nrow(d), r = r,
           conf_low = est - qt(0.975, dfres) * se,
           conf_high = est + qt(0.975, dfres) * se,
           ok = TRUE)
  }, error = function(e) tibble(estimate = NA, se = NA, statistic = NA, df = NA,
                                n = NA, r = NA, conf_low = NA, conf_high = NA, ok = FALSE))
  bind_cols(as_tibble(row), out)
}

beta_res <- map(seq_len(nrow(grid)), \(i) fit_one_beta(grid[i, ])) |> bind_rows()

beta_res <- beta_res |> filter(ok) |> select(-ok) |>
  mutate(positive_outcome = outcome %in% c("mcosmo", "mutil", "mpos"),
         sig = abs(statistic) > qt(0.975, df),
         supports_claim = if_else(positive_outcome, estimate < 0, estimate > 0),
         baseline = FALSE,   # the baseline cell is Gaussian; the beta twin is not it
         family = "beta")

full <- bind_rows(gauss |> mutate(family = "gaussian"), beta_res) |>
  relocate(family, .after = weights)

datawizard::data_write(full, out_path("spec_grid_family.csv"), verbose = FALSE)

# the beta twin of the baseline cell: z = -4.075, r = -0.258
twin <- full |> filter(family == "beta", outcome == "mcosmo", pred_form == "raw",
                       copredictor == "none", estimator == "fe_twoway",
                       sample == "all", weights == "none")
stopifnot("a beta fit failed" = nrow(full) == 1680,
          "the beta twin of the baseline is not reproduced" = abs(twin$statistic + 4.075) < 5e-4)
