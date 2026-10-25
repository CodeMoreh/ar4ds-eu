################################################################################
# Title        : _chart_multiverse.R
# Purpose      : Builds the interactive specification curves of the results
#                page: the payload of one curve (the specifications ranked by
#                the claim-aligned partial correlation, the option of every
#                decision, the plain-language labels of the menu and the
#                reference marks with the lines of their pop-ups) and the raw
#                HTML that places it on the page. The drawing and the
#                interaction live in assets/chart/multiverse.js, the styles in
#                assets/chart/multiverse.css and the markup of one chart in
#                assets/chart/multiverse.template.html. The design follows the
#                chart of the blog post on the same claim, reimplemented here
#                so that the course depends on no file outside this repository.
# Author       : Chris Moreh
# Last updated : 2026-10-25
# Sourced by   : results.qmd
################################################################################

library(dplyr)

# The decisions that vary on the two curves, in the order of the "Colour by"
# and "Show only" menus. The keys are the short names that the payload and
# assets/chart/multiverse.js use.
mv_axes <- tibble::tribble(
  ~key, ~axis,         ~name,
  "o",  "outcome",     "Outcome",
  "x",  "predictor",   "Predictor",
  "c",  "copredictor", "Adjustment",
  "e",  "estimator",   "Estimator",
  "nr", "noop",        "No-opinion rule",
  "it", "item_set",    "Item set",
  "cw", "cell_weight", "Cell weighting",
  "ms", "measure",     "Series",
  "pf", "pred_form",   "Predictor form"
)

# Where each reference result stands in relation to the menu of the curve.
mv_menu_relation <- tibble::tribble(
  ~id,     ~menu_relation,
  "OA",    "This result comes from outside the menu: a model of respondents rather than country-years, with individual controls, GDP growth and interactions with education.",
  "KQXUE", "This design is on the menu: the nearest point of the curve is country fixed effects on the Eurostat series of the 20–64 population, with respondents who mention nothing left out.",
  "C6HJR", "This is the reproduced specification, a point of the curve itself.",
  "KEVF1", "This result comes from outside the menu: a model of respondents, the count of the five cosmopolitan items rather than their share, and an unemployment series that covers only 135 of the 270 country-years.",
  "PRL47", "This result comes from outside the menu: a model of respondents within country-quarters within countries, with individual and macro controls and random slopes.",
  "018OL", "This result comes from outside the menu: 420 country-wave cells rather than 270 country-years, and the log of the mean of the five cosmopolitan items as the outcome."
)

# ---- formatting ----------------------------------------------------------------
# A typographic minus for negative numbers, and an en-dash in ranges, in text
# that the data files write with hyphens.
mv_minus <- \(x, d = 3) sub("^-", "−", formatC(x, format = "f", digits = d))
mv_pct <- \(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
mv_df <- \(x) sub("\\.0$", "", sprintf("%.1f", x))
mv_clean <- \(x) x |>
  stringr::str_replace_all("(?<=\\d)-(?=\\d)", "–") |>
  stringr::str_replace_all("(?<![\\w.])-(?=[0-9.])", "−") |>
  stringr::str_replace_all("whose bootstrap p is p_cluster_check", "whose bootstrap p gives the shape of this mark") |>
  stringr::str_replace_all("p_wcr", "bootstrap p") |>
  stringr::str_replace_all("p_cluster_check", "the bootstrap check") |>
  stringr::str_replace_all("se_natural", "The standard error")
mv_p_label <- \(p) if_else(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))

# ---- the labels of the menu ------------------------------------------------------
# The options of one decision that occur on a curve, in the order of
# data/menu_options.csv, with their plain-language labels.
mv_levels <- function(curve, options, axis_code) {
  lev <- options |>
    filter(.data$axis == axis_code, .data$option %in% curve[[axis_code]]) |>
    select("option", "label", "phrase")
  stopifnot(all(curve[[axis_code]] %in% lev$option))
  lev
}

# ---- the reference marks ---------------------------------------------------------
# Each mark is placed by rank among the specifications at r_placement, the
# result converted on the degrees of freedom of its own inference. Its shape
# follows the rule of the curve where the file gives the bootstrap check of
# the design on the country-year cells, and the p-value as reported where the
# design cannot be fitted on those cells.
mv_marks <- function(marks, curve_r) {
  n <- length(curve_r)
  marks |>
    left_join(mv_menu_relation, by = "id") |>
    mutate(
      rank      = findInterval(.data$r_placement, sort(curve_r)),
      p_num     = suppressWarnings(as.numeric(sub("^<\\s*", "", .data$p_recorded))),
      sg        = as.integer(coalesce(.data$p_cluster_check, .data$p_num) < 0.05),
      tag       = if_else(.data$id == "OA", "OA design", .data$id),
      title     = case_when(
        .data$id == "OA"    ~ "The design of the original study (2016), refitted by the course without its education interactions",
        .data$id == "C6HJR" ~ "Multi100 reanalyst C6HJR, the specification that you reproduce in Task 3",
        .data$id == "PRL47" ~ "Multi100 reanalyst PRL47, model B8",
        .default            = paste("Multi100 reanalyst", .data$id)
      ),
      stat      = paste0(
        sprintf("%s = %s %s, %s", .data$statistic_type,
                sub("^-", "−", as.character(.data$statistic_recorded)),
                if_else(.data$id == "OA", "as printed in the paper, for the low-education band of the model with education interactions",
                        "as recorded by Multi100"),
                if_else(startsWith(.data$p_recorded, "<"), paste("p", .data$p_recorded),
                        mv_p_label(.data$p_num))),
        # the mark of OA is the refit without the interactions, so its statistic is shown too
        if_else(.data$id == "OA",
                sprintf("; placed by the refit without the interactions, t = %s",
                        sub("^-", "−", sprintf("%.2f", .data$statistic_placement))), "")),
      shape     = if_else(.data$sg == 1L, "Circle", "Cross"),
      shape_why = if_else(
        is.na(.data$p_cluster_check),
        sprintf("%s, from the p-value as reported, which rests on the errors of the model itself; this design cannot be refitted on the country-year cells, so the bootstrap of the curve cannot test it", .data$shape),
        sprintf("%s, from the bootstrap check of the design on the country-year cells (p = %.3f), the rule that classes every point of the curve", .data$shape, .data$p_cluster_check)
      ),
      lines     = purrr::pmap(
        list(.data$unit, .data$design, .data$df_recorded, .data$r_recorded, .data$df_placement,
             .data$df_basis, .data$rank, .data$r_233, .data$shape_why),
        \(unit, design, df_rec, r_rec, df_pl, basis, rank, r233, why) list(
          c("Unit", mv_clean(unit)),
          c("Design", mv_clean(design)),
          c("Recorded df", if (is.na(df_rec)) "none reported" else mv_df(df_rec)),
          c("Recorded r", if (is.na(r_rec)) "none recorded" else mv_minus(r_rec)),
          c("Placed on", sprintf("%s df (%s)", mv_df(df_pl), basis)),
          c("Placement", sprintf("%d of the %d specifications lie further below zero", rank, n)),
          c("On 233 df", sprintf("r = %s, the common denominator of the conversion lesson; the mark is not drawn there", mv_minus(r233))),
          c("Shape", why)
        )
      ),
      notes     = purrr::map2(.data$menu_relation, .data$note, \(rel, note) I(c(rel, mv_clean(note))))
    )
}

# ---- the payload of one curve ----------------------------------------------------
# curve: one of data/spec_curve.csv or data/spec_curve_negative.csv;
# options: data/menu_options.csv; marks: data/reference_marks.csv or NULL.
mv_payload <- function(curve, options, marks = NULL, reversed = FALSE, file = "spec-curve.csv",
                       ref_text = "The reproduced specification of Task 3", target = "claim") {
  curve <- curve |>
    arrange(.data$r_ca) |>
    mutate(t_ca = if_else(sign(.data$r_ca) == sign(.data$r), .data$t, -.data$t))
  levels <- purrr::map(mv_axes$axis, \(a) mv_levels(curve, options, a)) |>
    rlang::set_names(mv_axes$key)
  code <- purrr::map2(mv_axes$axis, levels, \(a, lev) match(curve[[a]], lev$option) - 1L) |>
    rlang::set_names(mv_axes$key)
  an <- if (is.null(marks)) data.frame() else mv_marks(marks, curve$r_ca)
  r_all <- c(curve$r_ca, an$r_placement)
  y_min <- floor((min(r_all) - 0.03) * 20) / 20
  y_max <- max(0.25, ceiling((max(r_all) + 0.12) * 20) / 20)
  list(
    summary = sprintf(paste(
      "Specification curve of %d specifications, ranked by claim-aligned partial correlation from %s to %s;",
      "%s lie below zero, in the direction of the %s, and %s lie below zero with bootstrap p below 0.05."),
      nrow(curve), mv_minus(min(curve$r_ca)), mv_minus(max(curve$r_ca)),
      mv_pct(mean(curve$r_ca < 0)), target, mv_pct(mean(curve$r_ca < 0 & curve$p_wcr < 0.05))),
    r = round(curve$r_ca, 4),
    st = round(curve$t_ca, 3),
    df = curve$df,
    p = signif(curve$p_wcr, 3),
    sg = as.integer(curve$p_wcr < 0.05),
    ref = if (any(curve$is_reference)) which(curve$is_reference) - 1L else -1L,
    refText = ref_text,
    reversed = reversed,
    target = target,
    code = code,
    lv = purrr::map(levels, \(lev) I(lev$label)),
    cd = purrr::map(levels, \(lev) I(lev$option)),
    gloss = I(paste0("Compares ", levels$e$phrase, ".")),
    adjOther = I(paste("Adjusted for", c(unemp = "GDP growth", growth = "unemployment")[levels$x$option])),
    axes = c(
      list(list(key = "", name = "None", levels = I(character(0)))),
      purrr::pmap(list(mv_axes$key, mv_axes$name), \(k, nm) list(key = k, name = nm, levels = I(levels[[k]]$label)))
    ),
    csvAxes = purrr::map2(mv_axes$key, mv_axes$axis, \(k, a) c(k, a)),
    y = list(min = y_min, max = y_max,
             ticks = I(round(seq(ceiling(y_min * 5) / 5, floor(y_max * 5) / 5, by = 0.2), 1))),
    file = file,
    an = if (nrow(an) == 0) list() else purrr::pmap(
      list(an$tag, an$title, an$r_placement, an$rank, an$sg, an$stat, an$lines, an$notes),
      \(tag, title, r, rank, sg, stat, lines, notes) list(
        label = tag, title = title, r = round(r, 4), rank = rank, sg = sg, stat = stat,
        lines = lines, notes = notes
      )
    )
  )
}

# ---- the HTML ------------------------------------------------------------------------
# One chart as a raw HTML block for a chunk with `output: asis`. The first
# chart of a page carries the links to the script and the styles; prefix is
# the path from the page to the root of the site.
mv_chart_html <- function(id, payload, title, sub, notes, src, assets = TRUE, prefix = "") {
  template <- readr::read_file("assets/chart/multiverse.template.html") |>
    sub(pattern = "(?s)^\\s*<!--.*?-->\\s*", replacement = "", perl = TRUE)
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA)
  values <- c(
    "__MV_ID__"    = id,
    "__MV_TITLE__" = htmltools::htmlEscape(title),
    "__MV_SUB__"   = htmltools::htmlEscape(sub),
    "__MV_NOTES__" = notes,
    "__MV_SRC__"   = src,
    "__MV_DATA__"  = as.character(json)
  )
  # split at each placeholder rather than substituting, so that nothing in the
  # data is read as a regular expression or a back-reference
  filled <- purrr::reduce2(names(values), unname(values),
                           \(acc, ph, v) paste(strsplit(acc, ph, fixed = TRUE)[[1]], collapse = v),
                           .init = template)
  head <- if (assets) sprintf(
    '<link rel="stylesheet" href="%1$sassets/chart/multiverse.css">\n<script src="%1$sassets/chart/multiverse.js"></script>\n',
    prefix) else ""
  paste0("\n```{=html}\n", head, filled, "```\n\n")
}
