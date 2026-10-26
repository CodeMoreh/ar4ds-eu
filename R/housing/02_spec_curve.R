################################################################################
# Title:        02_spec_curve.R
# Purpose:      The curve of the claim: every specification of the menu
#               fitted once, with a randomisation test of each as the check.
# Reads:        cases/housing/data/housing_audit.csv
# Writes:       cases/housing/data/spec_curve.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
source("R/housing/_tools.R")

audit <- read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)

# The randomisation test reshuffles the names among the analysed emails and
# refits, so it asks how often an arbitrary relabelling gives a gap as large as
# the one observed. It is stored as a check beside the test of each model.
N_DRAWS <- 1999

ri_p <- function(audit, comparison, population, uncoded, model, adjust, observed, seed) {
  set.seed(seed)
  terms <- ADJUST_TERMS[[adjust]]
  frame <- spec_frame(audit, comparison, population, uncoded) |>
    filter(if_all(all_of(terms), \(x) !is.na(x)))
  form <- as.formula(paste("y ~", paste(c("hisp", terms), collapse = " + ")))
  draws <- map_dbl(seq_len(N_DRAWS), \(i) {
    shuffled <- mutate(frame, hisp = sample(hisp))
    if (model == "lpm") {
      unname(coef(lm(form, data = shuffled))["hisp"])
    } else {
      unname(coef(glm(form, family = binomial(), data = shuffled))["hisp"])
    }
  })
  (1 + sum(abs(draws) >= abs(observed))) / (N_DRAWS + 1)
}

grid <- expand_grid(!!!SPEC_AXES)

curve <- grid |>
  pmap(\(comparison, population, uncoded, model, adjust)
       fit_spec(audit, comparison, population, uncoded, model, adjust)) |>
  bind_rows() |>
  mutate(spec_id = row_number(), .before = 1)

curve <- curve |>
  mutate(p_ri = pmap_dbl(list(comparison, population, uncoded, model, adjust, coef, spec_id),
                         \(comparison, population, uncoded, model, adjust, coef, spec_id)
                         ri_p(audit, comparison, population, uncoded, model, adjust, coef,
                              seed = 20261025 + spec_id))) |>
  mutate(
    supports = b_pp < 0,
    sig = p < 0.05,
    is_reference = comparison == "white" & population == "replies" & uncoded == "drop" &
      model == "lpm" & adjust == "none"
  )

stopifnot(nrow(curve) == 64, sum(curve$is_reference) == 1)

write_csv(curve, "cases/housing/data/spec_curve.csv")
