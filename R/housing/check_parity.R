################################################################################
# Title:        check_parity.R
# Purpose:      Checks the hand-written estimator of R/housing/_tools.R against
#               estimatr (HC2 errors) and marginaleffects (average marginal
#               effects with delta-method errors) on every specification, and
#               the committed curve against a fresh fit.
# Reads:        cases/housing/data/housing_audit.csv, cases/housing/data/spec_curve.csv
# Writes:       nothing; stops on any disagreement
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
library(estimatr)
library(marginaleffects)
source("R/housing/_tools.R")

audit <- read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)
curve <- read_csv("cases/housing/data/spec_curve.csv", show_col_types = FALSE)

reference_fit <- function(comparison, population, uncoded, model, adjust) {
  terms <- ADJUST_TERMS[[adjust]]
  # marginaleffects refuses a column called `group`, so only the model columns go in
  frame <- spec_frame(audit, comparison, population, uncoded) |>
    filter(if_all(all_of(terms), \(x) !is.na(x))) |>
    select(y, hisp, all_of(terms))
  form <- as.formula(paste("y ~", paste(c("hisp", terms), collapse = " + ")))
  if (model == "lpm") {
    fit <- lm_robust(form, data = frame, se_type = "HC2")
    tibble(b_ref = 100 * coef(fit)[["hisp"]], se_ref = 100 * fit$std.error[["hisp"]])
  } else {
    fit <- glm(form, family = binomial(), data = frame)
    ame <- avg_comparisons(fit, variables = list(hisp = 0:1))
    tibble(b_ref = 100 * ame$estimate, se_ref = 100 * ame$std.error)
  }
}

check <- curve |>
  mutate(ref = pmap(list(comparison, population, uncoded, model, adjust), reference_fit)) |>
  unnest(ref) |>
  mutate(fresh = pmap(list(comparison, population, uncoded, model, adjust),
                      \(...) fit_spec(audit, ...)[, c("b_pp", "stat")])) |>
  unnest(fresh, names_sep = "_")

gaps <- check |>
  summarise(b_vs_reference = max(abs(b_pp - b_ref)),
            se_vs_reference = max(abs(se_pp - se_ref)),
            b_vs_fresh = max(abs(b_pp - fresh_b_pp)),
            stat_vs_fresh = max(abs(stat - fresh_stat)))
print(as.data.frame(gaps))

stopifnot(gaps$b_vs_reference < 1e-6, gaps$se_vs_reference < 1e-3,
          gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on all", nrow(check), "specifications.\n")  # r-style: keep – a checker, whose output is its report
