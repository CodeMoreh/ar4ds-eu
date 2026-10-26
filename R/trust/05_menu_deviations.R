################################################################################
# Title:        05_menu_deviations.R
# Purpose:      What each option of the menu does to the estimate: every
#               specification against its twin that differs on that decision
#               alone, for the decision map.
# Reads:        cases/trust/data/spec_curve.csv
# Writes:       cases/trust/data/menu_deviations.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
source("R/trust/_tools.R")

# A column of the single value "t" is read as logical unless typed
curve <- read_csv("cases/trust/data/spec_curve.csv", show_col_types = FALSE,
                  col_types = cols(stat_type = col_character()))
axes <- names(SPEC_AXES)

# For each decision, the first option is the baseline; every other option is
# compared with it on pairs of specifications that agree on all other decisions.
deviations <- imap(SPEC_AXES, \(options, axis) {
  others <- setdiff(axes, axis)
  base <- curve |>
    filter(.data[[axis]] == options[1]) |>
    select(all_of(others), b_base = b, p_base = p, n_base = n)
  curve |>
    inner_join(base, by = others) |>
    group_by(option = .data[[axis]]) |>
    summarise(
      n_pairs = n(),
      shift_mean = mean(b - b_base),
      shift_min = min(b - b_base),
      shift_max = max(b - b_base),
      b_mean = mean(b),
      n_mean = mean(n),
      n_shift_mean = mean(n - n_base),
      sig_share = mean(p < 0.05),
      flips = sum((p < 0.05) != (p_base < 0.05)),
      .groups = "drop"
    ) |>
    mutate(axis = axis, baseline = options[1], .before = 1)
}) |>
  bind_rows()

write_csv(deviations, "cases/trust/data/menu_deviations.csv")
