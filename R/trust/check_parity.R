################################################################################
# Title:        check_parity.R
# Purpose:      Checks the hand-written estimator of R/trust/_tools.R against
#               fixest (coefficients, conventional and clustered errors, on every
#               specification of the curve) and sandwich (clustered errors from
#               lm() with dummies, on every eighth specification and the
#               off-curve fits), and the committed curve against a fresh fit.
# Reads:        cases/trust/data/trust_ess.csv, cases/trust/data/spec_curve.csv
# Writes:       nothing; stops on any disagreement
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
library(fixest)
library(sandwich)
source("R/trust/_tools.R")

ess <- read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
curve <- read_csv("cases/trust/data/spec_curve.csv", show_col_types = FALSE,
                  col_types = cols(stat_type = col_character()))

rhs_of <- \(controls, fe) {
  terms <- paste(c("x", CONTROL_TERMS[[controls]]), collapse = " + ")
  switch(fe, country_round = paste(terms, "| cr"), country_and_round = paste(terms, "| cntry + essround"),
         pooled = terms)
}

# fixest: same coefficient, conventional t, and CR1 errors by country with its
# default small-sample correction
fixest_fit <- function(outcome, controls, weights, items, fe) {
  frame <- spec_frame(ess, outcome, controls, weights, items)
  fit <- feols(as.formula(paste("y ~", rhs_of(controls, fe))), data = frame, weights = ~w, cluster = ~cntry)
  tibble(b_fixest = coef(fit)[["x"]], se_fixest = se(fit)[["x"]],
         t_conv_fixest = coef(fit)[["x"]] / se(fit, vcov = "iid")[["x"]])
}

check_fixest <- curve |>
  mutate(ref = pmap(list(outcome, controls, weights, items, fe), fixest_fit)) |>
  unnest(ref)

# sandwich: CR0 meat from lm() with dummies for the absorbed groups, scaled by
# G / (G - 1) inside vcovCL() and by (n - 1) / (n - K) here, K being the slopes
# and the intercept, as fit_spec() counts them
sandwich_fit <- function(outcome, controls, weights, items, fe, sample = "paper", concern = "index",
                         extra = "none") {
  frame <- spec_frame(ess, outcome, controls, weights, items, sample, concern, extra) |>
    as.data.frame()
  terms <- c("x", CONTROL_TERMS[[controls]], if (extra != "none") extra)
  dummies <- switch(fe, country_round = "factor(cr)", country_and_round = "factor(cntry) + factor(essround)",
                    pooled = NULL)
  fit <- lm(as.formula(paste("y ~", paste(c(terms, dummies), collapse = " + "))), data = frame, weights = w)
  k_ssc <- length(terms) + 1 + (fe == "country_and_round") * 3
  n <- nobs(fit)
  v <- vcovCL(fit, cluster = ~cntry, type = "HC0", cadjust = TRUE)["x", "x"] * (n - 1) / (n - k_ssc)
  tibble(b_lm = coef(fit)[["x"]], se_lm = sqrt(v))
}

every_eighth <- curve |>
  filter(spec_id %% 8 == 1) |>
  select(outcome, controls, weights, items, fe) |>
  mutate(sample = "paper", concern = "index", extra = "none")
off_curve <- tribble(
  ~outcome,     ~controls, ~weights,          ~items,     ~fe,             ~sample,           ~concern, ~extra,
  "parliament", "paper",   "none",            "complete", "pooled",        "paper",           "index",  "none",
  "parliament", "paper",   "post_population", "complete", "country_round", "paper",           "index",  "none",
  "parliament", "paper",   "none",            "complete", "country_round", "all_respondents", "index",  "none",
  "parliament", "paper",   "none",            "complete", "country_round", "paper",           "place",  "gov_dissat"
)
check_sandwich <- bind_rows(every_eighth, off_curve) |>
  mutate(ours = pmap(pick(everything()), \(...) fit_spec(ess, ...)[, c("b", "se")]),
         ref = pmap(pick(outcome:extra), sandwich_fit)) |>
  unnest(c(ours, ref))

# The committed curve against a fresh fit of the estimator
check_fresh <- curve |>
  mutate(fresh = pmap(list(outcome, controls, weights, items, fe),
                      \(...) fit_spec(ess, ...)[, c("b", "stat", "r")])) |>
  unnest(fresh, names_sep = "_")

gaps <- tibble(
  b_vs_fixest = max(abs(check_fixest$b - check_fixest$b_fixest)),
  se_vs_fixest = max(abs(check_fixest$se / check_fixest$se_fixest - 1)),
  t_conv_vs_fixest = max(abs(check_fixest$t_conv / check_fixest$t_conv_fixest - 1)),
  b_vs_lm = max(abs(check_sandwich$b - check_sandwich$b_lm)),
  se_vs_sandwich = max(abs(check_sandwich$se / check_sandwich$se_lm - 1)),
  b_vs_fresh = max(abs(check_fresh$b - check_fresh$fresh_b)),
  stat_vs_fresh = max(abs(check_fresh$stat - check_fresh$fresh_stat))
)
print(as.data.frame(gaps))

stopifnot(gaps$b_vs_fixest < 1e-9, gaps$se_vs_fixest < 1e-8, gaps$t_conv_vs_fixest < 1e-8,
          gaps$b_vs_lm < 1e-9, gaps$se_vs_sandwich < 1e-8,
          gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on", nrow(check_fixest), "specifications against fixest and on",  # r-style: keep – a checker, whose output is its report
    nrow(check_sandwich), "fits against sandwich.\n")
