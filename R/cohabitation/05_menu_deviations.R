################################################################################
# Title:        05_menu_deviations.R
# Purpose:      What each option of the menu does to the estimate: every
#               specification against its twin that differs on that decision
#               alone, for the decision map.
# Reads:        cases/cohabitation/data/spec_curve.csv
# Writes:       cases/cohabitation/data/menu_deviations.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/cohabitation/_tools.R")

# A column of the single value "t" is read as logical unless typed
curve <- readr::read_csv("cases/cohabitation/data/spec_curve.csv", show_col_types = FALSE,
                         col_types = readr::cols(stat_type = readr::col_character()))
axes <- names(SPEC_AXES)

# For each decision, the first option is the baseline; every other option is
# compared with it on pairs of specifications that agree on all other
# decisions. dplyr rather than datawizard inside the lambda, because datawizard
# evaluates its conditions in a fresh environment and so cannot see `axis` or
# `options`.
deviations <- purrr::imap(SPEC_AXES, \(options, axis) {
  others <- setdiff(axes, axis)
  base <- curve |>
    dplyr::filter(.data[[axis]] == options[1]) |>
    dplyr::select(dplyr::all_of(others), b_base = b, p_base = p, n_base = n)
  curve |>
    dplyr::inner_join(base, by = others) |>
    dplyr::group_by(option = .data[[axis]]) |>
    dplyr::summarise(
      n_pairs = dplyr::n(),
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
    dplyr::mutate(axis = axis, baseline = options[1], .before = 1)
}) |>
  dplyr::bind_rows()

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(deviations, "cases/cohabitation/data/menu_deviations.csv")
