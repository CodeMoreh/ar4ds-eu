################################################################################
# Title:        05_menu_deviations.R
# Purpose:      What each option of the menu does to the estimate: every
#               specification against its twin that differs on that decision
#               alone, for the decision map.
# Reads:        cases/trust/data/spec_curve.csv
# Writes:       cases/trust/data/menu_deviations.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/trust/_tools.R")

# A column of the single value "t" is read as logical unless typed; readr
# rather than datawizard::data_read(), which reads empty cells as "" rather than NA
curve <- readr::read_csv("cases/trust/data/spec_curve.csv", show_col_types = FALSE,
                         col_types = readr::cols(stat_type = readr::col_character()))
axes <- names(SPEC_AXES)

# For each decision, the first option is the baseline; every other option is
# compared with it on pairs of specifications that agree on all other decisions.
# dplyr inside the lambda, because datawizard cannot see its arguments.
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

# readr writes the bytes of the committed file; datawizard::data_write() does not
readr::write_csv(deviations, "cases/trust/data/menu_deviations.csv")
