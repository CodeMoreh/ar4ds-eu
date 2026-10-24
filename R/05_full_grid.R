##############################################################################
# Title:        05_full_grid.R
# Purpose:      Builds the full aggregated universe of 2,520 specifications:
#               the family universe of 1,680 imported verbatim (predictor =
#               "unemp") plus 840 rows in which GDP growth carries the claim
#               (predictor = "growth"), alone or with raw unemployment as
#               co-predictor, across both outcome families. The predictor axis
#               opens a choice that Task 2 of Multi100 held fixed: Task 1 let
#               the same analysts choose freely, and three of the five reached
#               for a GDP measure. Growth enters in one form only – raw annual
#               per cent, the World Bank series already in the panel – since it
#               takes negative values, so that the log is undefined, and
#               centred or standardised forms are affine twins of raw with
#               identical t, p and partial r. Claim direction reverses for
#               growth: the claim predicts POSITIVE growth coefficients on the
#               positive framings. Estimator, weight, df and significance
#               conventions follow 03_spec_grid.R and 04_family_grid.R.
# Reads:        data/EUframes_cy.csv, data/spec_grid_family.csv
# Writes:       data/spec_grid_full.csv (2,520 rows)
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr)
  library(fixest); library(plm)
  library(betareg); library(glmmTMB); library(sandwich); library(lmtest)
})
source("R/_paths.R")

panel <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)
fam   <- datawizard::data_read(out_path("spec_grid_family.csv"), verbose = FALSE)

outcomes     <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
copredictors <- c("none", "unemp")
estimators   <- c("fe_twoway", "fe_country", "re", "pooled_cl")
samples      <- c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES")
weights_o    <- c("none", "n_cy")
families     <- c("gaussian", "beta")

grid <- expand_grid(outcome = outcomes, pred_form = "raw", copredictor = copredictors,
                    estimator = estimators, sample = samples, weights = weights_o,
                    family = families) |>
  filter(!(estimator == "re" & weights == "n_cy"))

prep_sample <- function(d, s) {
  switch(s,
    all        = d,
    y2004_2008 = filter(d, year <= 2008),
    y2009_2013 = filter(d, year >= 2009),
    no_bailout = filter(d, !cntry %in% unique(d$cntry[d$bailout == 1])),
    no_GR_ES   = filter(d, !cntry %in% c("GR", "ES")))
}

fit_one <- function(row) {
  d <- panel |>
    prep_sample(row$sample) |>
    mutate(x = growth, y = .data[[row$outcome]],
           w_raw = if (row$weights == "n_cy") n_cy else 1,
           # the linear family takes the weight as it is, the beta family as
           # case weights normalised to the sample size
           w = if (row$family == "gaussian") w_raw else w_raw * n() / sum(w_raw))
  fml_rhs <- if (row$copredictor == "unemp") "x + unemp" else "x"

  out <- tryCatch({
    if (row$family == "gaussian") {
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
    } else {
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

growth_res <- map(seq_len(nrow(grid)), \(i) fit_one(grid[i, ])) |> bind_rows()

growth_res <- growth_res |>
  filter(ok) |>
  select(-ok) |>
  mutate(positive_outcome = outcome %in% c("mcosmo", "mutil", "mpos"),
         sig = abs(statistic) > qt(0.975, df),
         supports_claim = if_else(positive_outcome, estimate > 0, estimate < 0),
         baseline = FALSE,
         predictor = "growth")

full <- bind_rows(fam |> mutate(predictor = "unemp"), growth_res) |>
  relocate(predictor, .after = family)

datawizard::data_write(full, out_path("spec_grid_full.csv"), verbose = FALSE)

# the family universe must travel unchanged; the growth mirror of the Task 1
# specification (mpos ~ growth + unemp, two-way FE) is b = +0.00167, t = 1.59
reimport <- datawizard::data_read(out_path("spec_grid_full.csv"), verbose = FALSE) |>
  filter(predictor == "unemp") |>
  select(-predictor)
mirror <- full |>
  filter(predictor == "growth", family == "gaussian", outcome == "mpos",
         copredictor == "unemp", estimator == "fe_twoway", sample == "all",
         weights == "none")
stopifnot("a growth fit failed" = nrow(full) == 2520,
          "the family universe changed in transit" =
            isTRUE(all.equal(as.data.frame(reimport), as.data.frame(fam), tolerance = 1e-12)),
          "the growth mirror is not reproduced" = abs(mirror$statistic - 1.59) < 5e-3)
