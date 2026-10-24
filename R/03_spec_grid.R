##############################################################################
# Title:        03_spec_grid.R
# Purpose:      Fits the canonical aggregated multiverse: 840 distinct linear
#               specifications of the unemployment claim on the course panel,
#               crossing outcome scale, predictor form, co-predictor set,
#               estimator, sample window and weights. Grand-mean centring and
#               standardising are affine transforms of the raw and logged
#               predictor and leave b, se, t, r and p unchanged, so they are
#               not distinct analyses; only raw and logged survive as
#               pred_form, and the baseline flag sits on the raw row. Random
#               effects does not support weights, and that combination is
#               dropped rather than approximated.
#               Coefficient extraction is package-native (fixest::coeftable,
#               plm summary) rather than going through the parameters
#               interface, because the partial-correlation conversion needs
#               the residual degrees of freedom under the definition of each
#               estimator, and those are pinned here explicitly.
#               Standard errors: conventional for the fixed- and random-effects
#               designs, clustered by country for pooled OLS, with the test and
#               the conversion on the residual degrees of freedom throughout.
# Reads:        data/EUframes_cy.csv
# Writes:       data/spec_grid.csv (840 rows)
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(fixest); library(plm)
})
source("R/_paths.R")

panel <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)

outcomes     <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
pred_forms   <- c("raw", "log")
copredictors <- c("none", "growth")
estimators   <- c("fe_twoway", "fe_country", "re", "pooled_cl")
samples      <- c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES")
weights_o    <- c("none", "n_cy")

# plm random effects does not support weights; that cell is dropped
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

fit_one <- function(row) {
  d <- panel |>
    prep_sample(row$sample) |>
    prep_pred_form(row$pred_form) |>
    mutate(y = .data[[row$outcome]],
           w = if (row$weights == "n_cy") n_cy else 1)
  fml_rhs <- if (row$copredictor == "growth") "x + growth" else "x"

  out <- tryCatch({
    if (row$estimator == "re") {
      m <- plm(as.formula(paste("y ~", fml_rhs)), data = d,
               index = c("cntry", "year"), model = "random")
      s <- summary(m)$coefficients
      est <- s["x", 1]; se <- s["x", 2]; stat <- s["x", 3]
      dfres <- nrow(d) - length(coef(m))
    } else {
      fml <- switch(row$estimator,
        fe_twoway  = as.formula(paste("y ~", fml_rhs, "| cntry + year")),
        fe_country = as.formula(paste("y ~", fml_rhs, "| cntry")),
        pooled_cl  = as.formula(paste("y ~", fml_rhs)))
      vc <- if (row$estimator == "pooled_cl") ~cntry else "iid"
      m <- feols(fml, data = d, weights = d$w, vcov = vc)
      ct <- coeftable(m)
      est <- ct["x", "Estimate"]; se <- ct["x", "Std. Error"]; stat <- ct["x", "t value"]
      dfres <- degrees_freedom(m, type = "resid")
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

res <- map(seq_len(nrow(grid)), \(i) fit_one(grid[i, ])) |> bind_rows()

# the claim expects negative effects on positive framings, positive on negative
res <- res |> select(-ok) |>
  mutate(positive_outcome = outcome %in% c("mcosmo", "mutil", "mpos"),
         sig = abs(statistic) > qt(0.975, df),
         supports_claim = if_else(positive_outcome, estimate < 0, estimate > 0),
         baseline = outcome == "mcosmo" & pred_form == "raw" & copredictor == "none" &
                    estimator == "fe_twoway" & sample == "all" & weights == "none")

datawizard::data_write(res, out_path("spec_grid.csv"), verbose = FALSE)

# the baseline is the reproduction target of the course: b = -0.00347, t = -3.804
b <- res |> filter(baseline)
stopifnot("a canonical fit failed" = !anyNA(res$estimate),
          "the baseline is not reproduced" = abs(b$statistic + 3.804) < 5e-4)
