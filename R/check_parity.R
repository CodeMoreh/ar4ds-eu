##############################################################################
# Title:        check_parity.R
# Purpose:      Compares a rebuild of the grids, written to a separate folder,
#               with the files committed under data/, before anything is
#               overwritten. Rows are matched on the axis columns of each
#               grid, never on row order, and the report gives, for every
#               numeric column, the largest absolute difference between the two
#               builds, plus the number of rows whose significance or claim
#               flag differs. The printed tables are the output of the script;
#               it stops if a grid gains or loses rows, or if any flag differs.
#               The rebuild folder is the one that _paths.R points to through
#               COURSE_OUT_DIR, so the same setting that redirects the build
#               points the check at it.
# Reads:        data/spec_grid.csv, data/spec_grid_family.csv,
#               data/spec_grid_full.csv, data/fork_importance.csv,
#               data/macro/analyst_unemployment.csv,
#               data/macro/predictor_measures.csv, and the same files in the
#               rebuild folder
# Writes:       nothing
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tidyr) })
source("R/_paths.R")

stopifnot("COURSE_OUT_DIR points at data/: set it to the rebuild folder" =
            normalizePath(out_dir, mustWork = FALSE) != normalizePath("data", mustWork = FALSE))

axes_core <- c("outcome", "pred_form", "copredictor", "estimator", "sample", "weights")
checks <- list(
  list(file = "spec_grid.csv",        keys = axes_core),
  list(file = "spec_grid_family.csv", keys = c(axes_core, "family")),
  list(file = "spec_grid_full.csv",   keys = c(axes_core, "family", "predictor")),
  list(file = "fork_importance.csv",  keys = "term"),
  list(file = file.path("macro", "analyst_unemployment.csv"),
       keys = c("analyst_id", "series", "cntry", "year")),
  list(file = file.path("macro", "predictor_measures.csv"),
       keys = c("measure", "predictor", "source", "cntry", "year"))
)

compare_one <- function(file, keys) {
  # the committed fork-importance file predates the schema of the grid and
  # names two of its axes by their earlier labels
  committed <- datawizard::data_read(file.path("data", file), verbose = FALSE) |>
    mutate(across(any_of("term"), \(x) recode(x, exposure = "pred_form", control = "copredictor")))
  rebuilt   <- datawizard::data_read(out_path(file), verbose = FALSE)
  joined <- full_join(committed, rebuilt, by = keys, suffix = c("_c", "_r"),
                      relationship = "one-to-one")
  num <- setdiff(names(committed)[map_lgl(committed, is.numeric)], keys)
  flags <- intersect(c("sig", "supports_claim", "baseline", "positive_outcome"),
                     names(committed))
  diffs <- map_dbl(num, \(v) max(abs(joined[[paste0(v, "_c")]] - joined[[paste0(v, "_r")]])))
  flag_diff <- map_int(flags, \(v) sum(joined[[paste0(v, "_c")]] != joined[[paste0(v, "_r")]]))
  list(
    rows = tibble(file, committed = nrow(committed), rebuilt = nrow(rebuilt),
                  matched = sum(complete.cases(joined[paste0(num[1], c("_c", "_r"))])),
                  flag_differences = sum(flag_diff)),
    diffs = tibble(file, column = num, max_abs_diff = diffs)
  )
}

res <- map(checks, \(x) compare_one(x$file, x$keys))
rows  <- map(res, "rows")  |> list_rbind()
diffs <- map(res, "diffs") |> list_rbind()

print(as.data.frame(rows), row.names = FALSE)
diffs |>
  mutate(max_abs_diff = signif(max_abs_diff, 3)) |>
  pivot_wider(names_from = column, values_from = max_abs_diff) |>
  as.data.frame() |>
  print(row.names = FALSE)

# where the largest differences sit: the five rows of the full universe whose
# estimate moves most between the two builds
full_c <- datawizard::data_read("data/spec_grid_full.csv", verbose = FALSE)
full_r <- datawizard::data_read(out_path("spec_grid_full.csv"), verbose = FALSE)
full_c |>
  inner_join(full_r, by = c(axes_core, "family", "predictor"), suffix = c("_c", "_r")) |>
  mutate(d_estimate = abs(estimate_c - estimate_r), d_statistic = abs(statistic_c - statistic_r),
         d_r = abs(r_c - r_r)) |>
  slice_max(d_statistic, n = 5) |>
  select(all_of(c(axes_core, "family", "predictor")), d_estimate, d_statistic, d_r) |>
  as.data.frame() |>
  print(row.names = FALSE)

stopifnot("a grid gained or lost rows" = all(rows$committed == rows$rebuilt & rows$rebuilt == rows$matched),
          "a significance or claim flag differs" = all(rows$flag_differences == 0))
