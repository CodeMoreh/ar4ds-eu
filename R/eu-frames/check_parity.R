##############################################################################
# Title:        check_parity.R
# Purpose:      Compares a rebuild of the files that the pipeline writes into
#               cases/eu-frames/data/, written to a separate folder, with the
#               files committed under cases/eu-frames/data/, before anything is
#               overwritten. Rows are matched on the key columns of each file,
#               never on row order. For every numeric column the report gives
#               the largest absolute difference between the two builds and the
#               number of cells that are missing in one build and not in the
#               other; for every other non-key column, the number of rows whose
#               value differs; and the number of rows whose significance, claim
#               or reference flag differs. The printed tables are the output of
#               the script. A file that the rebuild folder does not hold is
#               listed as not rebuilt and skipped, so that one stage can be
#               checked on its own. The script stops if a compared file gains or
#               loses rows, or if any flag differs. The rebuild folder is the
#               one that _paths.R points to through COURSE_OUT_DIR, so the same
#               setting that redirects the build points the check at it.
# Reads:        the files listed in `checks` below, under cases/eu-frames/data/
#               and in the rebuild folder
# Writes:       nothing
# Author:       Chris Moreh
# Last updated: 2026-10-27
##############################################################################

source("R/eu-frames/_paths.R")

committed_dir <- "cases/eu-frames/data"
stopifnot("COURSE_OUT_DIR points at the committed data: set it to the rebuild folder" =
            normalizePath(out_dir, mustWork = FALSE) != normalizePath(committed_dir, mustWork = FALSE))

axes_curve <- c("outcome", "predictor", "copredictor", "estimator", "sample", "weights",
                "construction", "noop", "item_set", "cell_weight", "measure", "pred_form", "family")
checks <- list(
  list(file = file.path("macro", "analyst_unemployment.csv"),
       keys = c("analyst_id", "series", "cntry", "year")),
  list(file = file.path("macro", "predictor_measures.csv"),
       keys = c("measure", "predictor", "source", "cntry", "year")),
  list(file = "teney_panel_weights.csv", keys = c("cntry", "year")),
  list(file = "teney_panel_measurement.csv",
       keys = c("construction", "item_set", "noop", "cell_weight", "cntry", "year", "outcome")),
  list(file = "spec_grid_mi.csv",
       keys = c("engine", "outcome", "copredictor", "sample", "estimator", "reg_weight", "se")),
  list(file = "mi_diagnostics.csv", keys = c("engine", "quantity")),
  list(file = "spec_curve.csv",          keys = axes_curve),
  list(file = "spec_curve_negative.csv", keys = axes_curve),
  list(file = "spec_curve_forks.csv",    keys = c("curve", "axis")),
  list(file = "menu_deviations.csv",     keys = c("curve", "axis", "option", "rule")),
  list(file = "display_between_within.csv", keys = c("outcome", "predictor")),
  list(file = "display_country_means.csv",  keys = "cntry"),
  list(file = "display_periods.csv",
       keys = c("outcome", "predictor", "pred_form", "copredictor", "estimator", "sample")),
  list(file = "display_loo.csv",       keys = c("estimator", "left_out")),
  list(file = "display_bailout.csv",
       keys = c("outcome", "predictor", "estimator", "group")),
  list(file = "display_inference.csv", keys = c("estimator", "inference")),
  list(file = "reference_marks.csv",   keys = "id"),
  list(file = "analyst_KQXUE.csv",     keys = c("cntry", "year")),
  list(file = "analyst_018OL.csv",     keys = c("cntry", "wave"))
)

flag_names <- c("sig", "supports_claim", "supports", "baseline", "is_reference", "positive_outcome")

compare_one <- function(file, keys) {
  committed <- datawizard::data_read(file.path(committed_dir, file), verbose = FALSE)
  rebuilt   <- datawizard::data_read(out_path(file), verbose = FALSE)
  joined <- dplyr::inner_join(committed, rebuilt, by = keys, suffix = c("_c", "_r"),
                              relationship = "one-to-one")
  shared <- setdiff(intersect(names(committed), names(rebuilt)), keys)
  num   <- shared[purrr::map_lgl(shared, \(v) is.numeric(committed[[v]]))]
  flags <- intersect(flag_names, shared)
  other <- setdiff(shared, c(num, flags))
  pair  <- \(v) list(joined[[paste0(v, "_c")]], joined[[paste0(v, "_r")]])
  diffs <- purrr::map_dbl(num, \(v) {
    p <- pair(v)
    d <- abs(p[[1]] - p[[2]])
    if (all(is.na(d))) 0 else max(d, na.rm = TRUE)
  })
  na_mismatch <- purrr::map_int(num, \(v) {
    p <- pair(v)
    sum(is.na(p[[1]]) != is.na(p[[2]]))
  })
  differs <- \(v) {
    p <- pair(v)
    sum(!(p[[1]] == p[[2]] | (is.na(p[[1]]) & is.na(p[[2]]))), na.rm = TRUE)
  }
  list(
    rows = tibble::tibble(file, committed = nrow(committed), rebuilt = nrow(rebuilt),
                          matched = nrow(joined),
                          na_mismatches = sum(na_mismatch),
                          text_differences = sum(purrr::map_int(other, differs)),
                          flag_differences = sum(purrr::map_int(flags, differs))),
    diffs = tibble::tibble(file, column = num, max_abs_diff = diffs)
  )
}

# a file is compared only when both builds hold it: a stage may be rebuilt on
# its own, and a new file has no committed copy until it is first accepted
both    <- \(x) file.exists(out_path(x$file)) && file.exists(file.path(committed_dir, x$file))
present <- purrr::keep(checks, both)
skipped <- purrr::map_chr(purrr::discard(checks, both), "file")
if (length(skipped) > 0) print(tibble::tibble(not_compared = skipped), n = Inf)

res <- purrr::map(present, \(x) compare_one(x$file, x$keys))
rows  <- purrr::map(res, "rows")  |> purrr::list_rbind()
diffs <- purrr::map(res, "diffs") |> purrr::list_rbind()

print(as.data.frame(rows), row.names = FALSE)
diffs |>
  dplyr::summarise(columns = dplyr::n(), largest_abs_diff = signif(max(max_abs_diff), 3),
                   in_column = column[which.max(max_abs_diff)], .by = file) |>
  as.data.frame() |>
  print(row.names = FALSE)

stopifnot("a file gained or lost rows" =
            all(rows$committed == rows$rebuilt & rows$rebuilt == rows$matched),
          "a significance, claim or reference flag differs" = all(rows$flag_differences == 0))
