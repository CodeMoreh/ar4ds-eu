################################################################################
# Title:        _chart_multiverse.R
# Purpose:      Builds the interactive specification curve of the multiverse
#               page of the cohabitation case: the payload of the curve (the
#               144 specifications ranked by the coefficient of the later
#               cohort, the option of every decision, the lines of each pop-up
#               and the reference marks) and the raw HTML that places it on the
#               page. The drawing and the interaction live in
#               assets/chart/multiverse.js, the styles in
#               assets/chart/multiverse.css and the markup of one chart in
#               assets/chart/multiverse.template.html, shared by the cases of
#               the course; the optional fields of the payload that adapt the
#               chart to this curve are listed at the top of multiverse.js.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   cases/cohabitation/multiverse.qmd
################################################################################

library(dplyr)

# The decisions that vary on the curve, in the order of the "Colour by" and
# "Show only" menus. The keys are the short names that the payload and
# assets/chart/multiverse.js use.
mv_axes <- tibble::tribble(
  ~key, ~axis,     ~name,
  "co", "cohorts", "Cohorts compared",
  "ho", "horizon", "Time frame",
  "ad", "adjust",  "Adjustment",
  "wt", "weights", "Weights",
  "ti", "time",    "Time since the start",
  "mo", "model",   "Model"
)

# The tag, the title and the relation to the menu of each reference result
# that the chart draws. Neither is a point of the curve: the published result
# comes from a model with an interaction that the curve leaves out, and 456AP
# compared other surveys on the file that SCORE built.
mv_menu_relation <- tibble::tribble(
  ~mark,   ~tag,       ~title,
  ~relation,
  "OA",    "OAs 2016", "The original authors (2016), Table 2, Model 2",
  "Outside the menu: Model 2 of the paper holds an interaction of race-ethnicity and period, so its coefficient of the period is the change for non-Hispanic white women alone, a narrower estimand than the change for all cohabitors that the curve estimates. The course refit of the model on the rebuilt unions gives −0.686 (SE 0.138).",
  "456AP", "456AP",    "Multi100 analyst 456AP",
  "Outside the menu: the unions of NSFG 2002 against those of 2013–2015, on the person-month file that SCORE built, without weights and without a term for the time since the start. The analyst left out the 1995 rows of the file as faulty, so the analysis compares other cohorts than the claim."
)

# ---- formatting ----------------------------------------------------------------
# A typographic minus for negative numbers, and an en-dash in ranges, in text
# that the data files write with hyphens.
mv_minus <- \(x, d = 3) sub("^-", "−", formatC(x, format = "f", digits = d))
mv_p_label <- \(p) if_else(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
mv_big <- \(x) format(round(x), big.mark = ",", trim = TRUE)
mv_clean <- \(x) x |>
  stringr::str_replace_all("(?<=\\d)-(?=\\d)", "–") |>
  stringr::str_replace_all("(?<![\\w.])-(?=[0-9.])", "−")
mv_upper_first <- \(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
mv_placement <- \(rank, n) case_when(
  rank == 0 ~ sprintf("None of the %d specifications lies further below zero", n),
  rank == 1 ~ sprintf("1 of the %d specifications lies further below zero", n),
  .default = sprintf("%d of the %d specifications lie further below zero", rank, n)
)

# ---- the labels of the menu ------------------------------------------------------
# The options of one decision that occur on the curve, in the order of
# menu_options.csv, with their labels.
mv_levels <- function(curve, options, axis_code) {
  lev <- options |>
    filter(.data$axis == axis_code, .data$option %in% curve[[axis_code]]) |>
    select("option", "label")
  stopifnot(all(curve[[axis_code]] %in% lev$option))
  lev
}

# ---- the reference marks ---------------------------------------------------------
# Each mark is placed by its coefficient on the log scale of the curve and,
# among the specifications, after those that lie further below zero. Its
# class comes from its own test: the z of the published coefficient and its
# standard error, and the z of the multinomial model of 456AP, refitted on the
# file of the analysts, which reproduces the value that Multi100 recorded.
mv_marks <- function(marks, analysts, curve) {
  # not n: a lambda built inside mutate() would find the column n of the marks first
  n_spec <- nrow(curve)
  out <- marks |>
    filter(.data$on_chart) |>
    left_join(mv_menu_relation, by = "mark") |>
    left_join(analysts |> select("mark" = "id", "comparison_txt" = "comparison", "unit", "outcome_model",
                                 "controls", "task1"),
              by = "mark") |>
    mutate(
      rank = purrr::map_int(.data$b, \(x) sum(curve$b < x - 1e-9)),
      sg = as.integer(.data$p < 0.05),
      stat = case_when(
        .data$mark == "OA" ~ sprintf("b = −0.683 with SE 0.14, as published; z = %s, as Multi100 recorded it; %s",
                                     mv_minus(.data$stat, 2), mv_p_label(.data$p)),
        .default = sprintf("z = %s, as recorded by Multi100 and reproduced on the file of the analysts; %s",
                           mv_minus(.data$stat, 3), mv_p_label(.data$p))
      ),
      lines = purrr::pmap(
        list(.data$mark, .data$comparison_txt, .data$unit, .data$outcome_model, .data$controls,
             .data$n, .data$task1, .data$rank),
        \(mark, cmp, unit, model, controls, n_pm, task1, rank) {
          rows <- if (mark == "OA") {
            list(
              c("Surveys", "NSFG 1995 and 2006–2010, the unions begun in 1990–1995 against those begun in 2005–2010"),
              c("Model", "Multinomial logit on person-months, weighted, with the interaction of race-ethnicity and period"),
              c("Coefficient", "The change for non-Hispanic white women, the reference group of the interaction"),
              c("Person-months", sprintf("%s, as published", mv_big(n_pm)))
            )
          } else {
            list(
              c("Surveys", mv_clean(cmp)),
              c("Unit", mv_upper_first(mv_clean(unit))),
              c("Model", mv_upper_first(mv_clean(model))),
              c("Controls", mv_upper_first(mv_clean(controls))),
              c("Person-months", mv_big(n_pm)),
              c("Task 1 conclusion", task1)
            )
          }
          c(rows, list(c("Placement", mv_placement(rank, n_spec))))
        }
      ),
      notes = purrr::map(.data$relation, \(rel) I(rel))
    )
  stopifnot(nrow(out) == sum(marks$on_chart), !anyNA(out$tag))
  out
}

# ---- the payload of the curve ----------------------------------------------------
# curve: spec_curve.csv; options: menu_options.csv; marks: reference_marks.csv;
# analysts: analysts5.csv. The curve is sorted from the most negative
# coefficient up, so that the largest decline, the strongest support for the
# claim, sits on the left; values below zero lie in the direction of the claim,
# which is the default of the chart. The mark settings suit a curve of 144.
mv_payload <- function(curve, options, marks, analysts, file = "spec-curve-cohabitation.csv",
                       size = c(4, 5), sw = "0.9",
                       op = list(sig = "0.45", ns = "0.3", ghostSig = "0.1", ghostNs = "0.05"),
                       y = list(min = -1, max = 0.2, step = 0.2)) {
  curve <- curve |> arrange(.data$b, .data$spec_id)
  levels <- purrr::map(mv_axes$axis, \(a) mv_levels(curve, options, a)) |>
    rlang::set_names(mv_axes$key)
  code <- purrr::map2(mv_axes$axis, levels, \(a, lev) match(curve[[a]], lev$option) - 1L) |>
    rlang::set_names(mv_axes$key)
  an <- mv_marks(marks, analysts, curve)
  lab_of <- \(k, i) levels[[k]]$label[code[[k]][i] + 1L]

  stat_line <- sprintf("t = %s on %d df, design-based errors · %s",
                       mv_minus(curve$stat, 2), as.integer(curve$df), mv_p_label(curve$p))
  detail <- purrr::map(seq_len(nrow(curve)), \(i) list(
    c("Cohorts compared", lab_of("co", i)),
    c("Time frame", lab_of("ho", i)),
    c("Adjustment", lab_of("ad", i)),
    c("Weights", lab_of("wt", i)),
    c("Time since the start", lab_of("ti", i)),
    c("Model", lab_of("mo", i)),
    c("Ratio", sprintf("The later cohort married at %.2f times the monthly chance of the earlier one", curve$ratio[i])),
    c("Person-months", sprintf("%s of %s unions, %s marriages", mv_big(curve$n[i]), mv_big(curve$n_unions[i]),
                               mv_big(curve$n_marriages[i]))),
    c("Clustered by woman", sprintf("t = %s on %s df", mv_minus(curve$t_woman[i], 2), mv_big(curve$df_woman[i]))),
    c("Conventional errors", sprintf("z = %s", mv_minus(curve$z_model[i], 2))),
    c("Partial correlation", sprintf("r = %s, by the rule of Multi100", mv_minus(curve$r[i], 3)))
  ))

  list(
    summary = sprintf(paste(
      "Specification curve of %d specifications, ranked by the coefficient of the later cohort on the log",
      "scale from %s to %s; %d lie below zero, in the direction of the claim, and %d of them with p below",
      "0.05 under design-based errors."),
      nrow(curve), mv_minus(min(curve$b)), mv_minus(max(curve$b)),
      sum(curve$b < 0), sum(curve$b < 0 & curve$p < 0.05)),
    r = round(curve$b, 4),
    st = round(curve$stat, 3),
    df = curve$df,
    p = signif(curve$p, 3),
    sg = as.integer(curve$p < 0.05),
    sl = I(stat_line),
    det = detail,
    ref = if (any(curve$is_reference)) which(curve$is_reference) - 1L else -1L,
    refText = "The reference specification: the model of the paper without its interaction of race-ethnicity and period",
    reversed = FALSE,
    target = "claim",
    yLabel = "Coefficient of the later cohort (log scale)",
    yDec = 1L,
    vfmt = list(pre = "b = ", suf = " on the log scale", d = 3L),
    tagGap = 0.1,
    xStep = 36L,
    xPad = 8L,
    size = size,
    sw = sw,
    op = op,
    code = code,
    lv = purrr::map(levels, \(lev) I(lev$label)),
    cd = purrr::map(levels, \(lev) I(lev$option)),
    axes = c(
      list(list(key = "", name = "None", levels = I(character(0)))),
      purrr::pmap(list(mv_axes$key, mv_axes$name),
                  \(k, nm) list(key = k, name = nm, levels = I(levels[[k]]$label)))
    ),
    csvAxes = purrr::map2(mv_axes$key, mv_axes$axis, \(k, a) c(k, a)),
    csvHead = c("rank", "b", "t_design", "df", "p", "significant_5pc"),
    y = list(min = y$min, max = y$max, ticks = I(round(seq(y$min, y$max, by = y$step), 2))),
    file = file,
    an = purrr::pmap(
      list(an$tag, an$title, an$b, an$rank, an$sg, an$stat, an$lines, an$notes),
      \(tag, title, b, rank, sg, stat, lines, notes) list(
        label = tag, title = title, r = round(b, 4), rank = rank, sg = sg, stat = stat,
        lines = lines, notes = notes
      )
    )
  )
}

# ---- the HTML ------------------------------------------------------------------------
# One chart as a raw HTML block for a chunk with `output: asis`. root is the
# path from the page to the root of the site, for reading the template and for
# the links to the script and the styles, which the first chart of a page
# carries.
mv_chart_html <- function(id, payload, title, sub, notes, src, root = "../../", assets = TRUE) {
  template <- readr::read_file(file.path(root, "assets/chart/multiverse.template.html")) |>
    sub(pattern = "(?s)^\\s*<!--.*?-->\\s*", replacement = "", perl = TRUE)
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA, na = "null")
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
    root) else ""
  paste0("\n```{=html}\n", head, filled, "```\n\n")
}
