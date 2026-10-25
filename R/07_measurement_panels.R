##############################################################################
# Title:        07_measurement_panels.R
# Purpose:      Builds, from the respondent file, the country-year quantities
#               that the measurement decisions need and the course panel does
#               not carry. Two files result.
#               teney_panel_weights.csv is wide: design-weighted and
#               count-built cell means, Kish's effective sample size, the
#               no-opinion counts by type, and the adult population.
#               teney_panel_measurement.csv is long: one row per cell, outcome
#               and measurement variant, where a variant is one choice on each
#               of four decisions. Construction is share (the mentions of a
#               respondent in the scale over that respondent's own total) or
#               count. Item set is all 13 items or the sensitivity set of the
#               original code without "unemployment". No-opinion handling is
#               zero (respondents with no mention scored 0), drop (removed,
#               which is what the 0/0 of the original Stata code did) or split
#               (drop the "don't know" and blank respondents, keep "other" at
#               0, since an answer off the list is still an answer). Cell
#               weighting is as interviewed or under the GESIS weight W1.
#               Under the reduced item set the no-mention group is defined on
#               the reduced total, so a respondent whose only mention was
#               "unemployment" joins it and counts as blank for the split
#               rule. Multiple imputation is not a variant here; it is the
#               separate tier of 08_mi_tier.R.
#               Three different things travel under the word "weight". Inside
#               a cell, W1 corrects the achieved sample towards its country on
#               sex, age, region and community size, a measurement decision.
#               Across cells, weighting the regression by respondent count or
#               effective sample size is a precision argument; weighting by
#               population changes whose average the slope describes. Kish's
#               effective sample size is kept because it, and not n, is the
#               precision of a weighted mean.
# Reads:        _planning_data/person_level.rds (GESIS-derived, local only),
#               data/macro/population.csv, data/EUframes_cy.csv
# Writes:       data/teney_panel_weights.csv (270 rows, wide),
#               data/teney_panel_measurement.csv (38,880 rows, long; aggregates
#               only)
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(purrr) })
source("R/_paths.R")

person <- readRDS(person_file)
pop    <- datawizard::data_read("data/macro/population.csv", verbose = FALSE)
base   <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)

outcomes <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")

## ---- the wide file ------------------------------------------------------------

shares <- c(mcosmo = "cosmo", mutil = "util", mcomm = "comm",
            mlib = "lib", mpos = "pos", mneg = "neg")
person <- person |> mutate(n_pos = n_cosmo + n_util, n_neg = n_comm + n_lib)
counts <- c(mcosmo = "n_cosmo", mutil = "n_util", mcomm = "n_comm",
            mlib = "n_lib", mpos = "n_pos", mneg = "n_neg")

# the summaries carry the respondent-level names, and to_scale() maps each to
# the name of its scale in place, so the column order holds
to_scale <- function(prefix, lookup) {
  \(n) paste0(prefix, names(lookup)[match(sub(paste0("^", prefix), "", n), lookup)])
}

cells <- person |>
  summarise(
    across(all_of(unname(shares)), \(x) weighted.mean(x, w1, na.rm = TRUE), .names = "w_{.col}"),
    across(all_of(unname(counts)), \(x) mean(x, na.rm = TRUE), .names = "c_{.col}"),
    across(all_of(unname(counts)), \(x) weighted.mean(x, w1, na.rm = TRUE), .names = "wc_{.col}"),
    n_cy    = n(),
    # how many of the 13 items the respondents of a cell mention on average:
    # what a count-built outcome tracks and a share-built one does not
    mean_items = mean(n_all),
    n_eff   = sum(w1, na.rm = TRUE)^2 / sum(w1^2, na.rm = TRUE),
    n_noop  = sum(n_all == 0),
    n_dk    = sum(noop_type == "dk"),
    n_other = sum(noop_type == "other"),
    n_blank = sum(noop_type == "blank"),
    .by = c(cntry, year)
  ) |>
  rename_with(to_scale("w_", shares), starts_with("w_")) |>
  rename_with(to_scale("c_", counts), starts_with("c_")) |>
  rename_with(to_scale("wc_", counts), starts_with("wc_"))

panel_w <- cells |>
  left_join(pop, by = c("cntry", "year")) |>
  mutate(noop_rate = n_noop / n_cy, deff = n_cy / n_eff) |>
  arrange(cntry, year) |>
  select(cntry, year, starts_with("w_"), starts_with("c_"), starts_with("wc_"),
         n_cy, mean_items, n_eff, deff, n_noop, n_dk, n_other, n_blank, noop_rate,
         pop_total, pop_15plus)

datawizard::data_write(panel_w, out_path("teney_panel_weights.csv"), verbose = FALSE)

## ---- the long measurement panel ----------------------------------------------
## For each item set the six numerators, the denominator and the no-mention
## type are put on common names; each variant then keeps the respondents that
## its no-opinion rule allows, builds the outcome and averages within cells.

set_vars <- function(d, item_set) {
  if (item_set == "all") {
    d |> transmute(cntry, year, w1, den = n_all, noop_type,
                   mcosmo = n_cosmo, mutil = n_util, mcomm = n_comm, mlib = n_lib,
                   mpos = n_cosmo + n_util, mneg = n_comm + n_lib)
  } else {
    d |> transmute(cntry, year, w1, den = n_all_un,
                   noop_type = if_else(n_all_un == 0 & n_all > 0, "blank", noop_type),
                   mcosmo = n_cosmo, mutil = n_util, mcomm = n_comm_un, mlib = n_lib,
                   mpos = n_cosmo + n_util, mneg = n_comm_un + n_lib)
  }
}

keep_rows <- function(d, noop) {
  switch(noop,
    zero  = d,
    drop  = filter(d, den > 0),
    split = filter(d, den > 0 | noop_type == "other"))
}

build_outcome <- function(d, construction) {
  if (construction == "share") {
    d |> mutate(across(all_of(outcomes), \(x) ifelse(x == 0, 0, x / den)))
  } else d
}

aggregate_cells <- function(d, cell_weight) {
  d |>
    mutate(.w = if (cell_weight == "design") w1 else 1) |>
    summarise(across(all_of(outcomes), \(x) weighted.mean(x, .w, na.rm = TRUE)),
              n_cell = n(),
              n_eff_cell = sum(.w, na.rm = TRUE)^2 / sum(.w^2, na.rm = TRUE),
              .by = c(cntry, year))
}

variants <- expand_grid(construction = c("share", "count"),
                        item_set     = c("all", "un"),
                        noop         = c("zero", "drop", "split"),
                        cell_weight  = c("unweighted", "design"))

measurement <- variants |>
  mutate(cells = pmap(list(construction, item_set, noop, cell_weight),
                      \(construction, item_set, noop, cell_weight) {
                        person |>
                          set_vars(item_set) |>
                          keep_rows(noop) |>
                          build_outcome(construction) |>
                          aggregate_cells(cell_weight)
                      })) |>
  unnest(cells) |>
  pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "value") |>
  arrange(construction, item_set, noop, cell_weight, outcome, cntry, year)

datawizard::data_write(measurement, out_path("teney_panel_measurement.csv"), verbose = FALSE)

## ---- checks -------------------------------------------------------------------
## The reference variant (share, all items, zero rule, cells as interviewed) is
## the course panel itself, to machine precision.

ref <- measurement |>
  filter(construction == "share", item_set == "all", noop == "zero",
         cell_weight == "unweighted") |>
  select(cntry, year, outcome, value) |>
  inner_join(base |> pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "panel") |>
               select(cntry, year, outcome, panel),
             by = c("cntry", "year", "outcome"))
stopifnot("the panel has not 270 cells in 27 countries" =
            nrow(panel_w) == 270 && n_distinct(panel_w$cntry) == 27,
          "the long panel has the wrong size" =
            nrow(measurement) == nrow(variants) * length(outcomes) * 270,
          "the reference variant does not reproduce the course panel" =
            nrow(ref) == 1620 && max(abs(ref$value - ref$panel)) < 1e-12)

tibble(median_deff = median(panel_w$deff), max_deff = max(panel_w$deff),
       noop_share = sum(panel_w$n_noop) / sum(panel_w$n_cy),
       max_abs_design_shift_mcosmo = max(abs(panel_w$w_mcosmo - base$mcosmo[
         match(paste(panel_w$cntry, panel_w$year), paste(base$cntry, base$year))]))) |>
  print()
