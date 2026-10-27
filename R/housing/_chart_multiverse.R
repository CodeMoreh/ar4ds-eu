################################################################################
# Title:        _chart_multiverse.R
# Purpose:      Builds the interactive specification curve of the multiverse
#               page of the housing-audit case: the payload of the curve (the
#               64 specifications ranked by the gap in percentage points, the
#               option of every decision, the lines of each pop-up and the
#               reference marks) and the raw HTML that places it on the page.
#               The drawing and the interaction live in assets/chart/multiverse.js,
#               the styles in assets/chart/multiverse.css and the markup of one
#               chart in assets/chart/multiverse.template.html, shared by the
#               cases of the course; the optional fields of the payload that
#               adapt the chart to this curve are listed at the top of
#               multiverse.js.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   cases/housing/multiverse.qmd
################################################################################

# The decisions that vary on the curve, in the order of the "Colour by" and
# "Show only" menus. The keys are the short names that the payload and
# assets/chart/multiverse.js use.
mv_axes <- tibble::tribble(
  ~key, ~axis,        ~name,
  "cp", "comparison", "Comparison",
  "po", "population", "Emails counted",
  "un", "uncoded",    "Uncoded replies",
  "mo", "model",      "Model",
  "ad", "adjust",     "Adjustment"
)

# Where each reference result stands in relation to the menu. Results that are
# a point of the curve name their twin, and mv_marks() refuses to build the
# payload unless the gap of the result and of its twin agree, so that a pop-up
# never claims a match that the files do not show.
mv_menu_relation <- tibble::tribble(
  ~id,     ~comparison,   ~population, ~uncoded,    ~model,  ~adjust, ~relation,
  "OA",    NA,            NA,          NA,          NA,      NA,      "Outside the menu: a logistic regression of all three groups of names with the continuous census covariates of the community, which the public file does not hold.",
  "8PTRZ", "nonhispanic", "replies",   "drop",      "lpm",   "none",  "A point of the curve: Hispanic against white and Black names among the replies, as a difference in proportions without covariates. The chi-square test of the table is a check of that specification.",
  "UYV3F", "white",       "replies",   "drop",      "logit", "none",  "A point of the curve: a logistic regression without covariates on the replies to Hispanic and white names, whose average marginal effect is the raw gap.",
  "EWFMI", NA,            NA,          NA,          NA,      NA,      "Outside the menu: a model of all three groups, adjusted for the sex of the name and for the poverty third entered as a number, which the menu treats as a check or does not offer. Its z is positive because its model takes the Hispanic names as the base.",
  "25N5D", "white",       "all",       "not_named", "lpm",   "none",  "A point of the curve: Hispanic against white names among all delivered emails, the uncoded replies counted as no greeting. The two-proportion test is a check of that specification.",
  "SCORE", NA,            NA,          NA,          NA,      NA,      "New data: the SCORE replication emailed 3,050 authorities in 2020 with four male aliases, two Hispanic and two white. Placed by the raw comparison of the reference specification, the gap among all replies.",
  "P9W8Y", "white",       "replies",   "drop",      "lpm",   "none",  "Beside the curve: without covariates the three-group model gives exactly the raw gap of the reference specification, but its omnibus test asks whether the three groups differ at all, a broader question than the claim."
)

# ---- formatting ----------------------------------------------------------------
# A typographic minus for negative numbers, and an en-dash in ranges, in text
# that the data files write with hyphens.
mv_minus <- \(x, d = 1) sub("^-", "−", formatC(x, format = "f", digits = d))
mv_p_label <- \(p) dplyr::if_else(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
mv_clean <- \(x) x |>
  stringr::str_replace_all("(?<=\\d)-(?=\\d)", "–") |>
  stringr::str_replace_all("(?<![\\w.])-(?=[0-9.])", "−")
mv_upper_first <- \(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
mv_placement <- \(rank, n) dplyr::case_when(
  rank == 0 ~ sprintf("None of the %d specifications lies further below zero", n),
  rank == 1 ~ sprintf("1 of the %d specifications lies further below zero", n),
  .default = sprintf("%d of the %d specifications lie further below zero", rank, n)
)

# ---- the labels of the menu ------------------------------------------------------
# The options of one decision that occur on the curve, in the order of
# menu_options.csv, with their labels and phrases.
mv_levels <- function(curve, options, axis_code) {
  lev <- options |>
    dplyr::filter(.data$axis == axis_code, .data$option %in% curve[[axis_code]]) |>
    dplyr::select("option", "label", "phrase")
  stopifnot(all(curve[[axis_code]] %in% lev$option))
  lev
}

# ---- the reference marks ---------------------------------------------------------
# Each mark is placed by its gap in points, and by rank among the specifications
# at that gap. The p-value comes from the statistic as published or recorded
# (a chi-square on 1 df, the omnibus test of P9W8Y on 2 df, a z otherwise).
mv_marks <- function(marks, analysts, curve) {
  # not n: a lambda built inside mutate() would find the column n of the marks first
  n_spec <- nrow(curve)
  out <- marks |>
    dplyr::filter(.data$on_chart) |>
    dplyr::left_join(mv_menu_relation, by = "id") |>
    dplyr::left_join(analysts |> dplyr::select("id", "comparison_txt" = "comparison", "population_txt" = "population",
                                 "method", "task1"),
              by = "id") |>
    dplyr::mutate(
      # strictly below, as try_spec() and the slides count: a tie is not further below zero
      rank  = purrr::map_int(.data$b_pp, \(x) sum(curve$b_pp < x - 1e-9)),
      p_num = dplyr::case_when(
        .data$statistic_type == "chi2" ~ pchisq(.data$statistic, 1, lower.tail = FALSE),
        grepl("2 df", .data$statistic_type) ~ pchisq(.data$statistic, 2, lower.tail = FALSE),
        .default = 2 * pnorm(-abs(.data$statistic))
      ),
      sg    = as.integer(.data$p_num < 0.05),
      tag   = dplyr::case_when(.data$id == "OA" ~ "OAs 2017", .data$id == "SCORE" ~ "SCORE 2020",
                        .default = .data$id),
      title = dplyr::case_when(.data$id == "OA" ~ "The original authors (2017), Table 1, model 3",
                        .data$id == "SCORE" ~ "The SCORE replication, a new audit (2020)",
                        .default = paste("Multi100 analyst", .data$id)),
      stat  = dplyr::case_when(
        .data$id == "OA" ~ sprintf("z = %s as published; %s", mv_minus(.data$statistic, 2),
                                   mv_p_label(.data$p_num)),
        .data$id == "SCORE" ~ sprintf("t = %s with HC2 errors, from the course refit of the replication data; %s",
                                      mv_minus(.data$statistic, 2), mv_p_label(.data$p_num)),
        grepl("2 df", .data$statistic_type) ~ sprintf(
          "chi-square = %s on 2 df, from the course refit (Multi100 recorded none); %s",
          formatC(.data$statistic, format = "f", digits = 3), mv_p_label(.data$p_num)),
        .data$statistic_type == "chi2" ~ sprintf("chi-square = %s, as recorded by Multi100; %s",
                                                 formatC(.data$statistic, format = "f", digits = 3),
                                                 mv_p_label(.data$p_num)),
        .default = sprintf("z = %s, as recorded by Multi100; %s", mv_minus(.data$statistic, 3),
                           mv_p_label(.data$p_num))
      ),
      comparison_txt = dplyr::case_when(
        !is.na(.data$comparison_txt) ~ sub(" vs ", " against ", .data$comparison_txt),
        .data$id == "SCORE" ~ "Hispanic against white names, four new male names",
        .default = "Hispanic against white names, in a model of all three groups"),
      population_txt = dplyr::coalesce(.data$population_txt, "replies"),
      method = dplyr::case_when(
        !is.na(.data$method) ~ .data$method,
        .data$id == "SCORE" ~ "difference in proportions on the replication data",
        .default = "logit with the continuous 2012 census covariates of the community and the hard-to-find address"),
      lines = purrr::pmap(
        list(.data$comparison_txt, .data$population_txt, .data$method, .data$n, .data$task1, .data$rank),
        \(cmp, pop, method, n_em, task1, rank) {
          rows <- list(
            c("Comparison", mv_upper_first(cmp)),
            c("Emails counted", mv_upper_first(pop)),
            c("Method", mv_upper_first(method)),
            c("Emails analysed", format(n_em, big.mark = ",")),
            if (!is.na(task1)) c("Task 1 conclusion", task1),
            c("Placement", mv_placement(rank, n_spec))
          )
          purrr::compact(rows)
        }
      ),
      notes = purrr::map2(.data$relation, .data$note, \(rel, note) I(c(rel, mv_clean(note))))
    )

  # every mark that claims a twin on the curve must sit on it
  twins <- out |>
    dplyr::filter(!is.na(.data$comparison)) |>
    dplyr::left_join(curve |> dplyr::select("comparison", "population", "uncoded", "model", "adjust",
                              "b_twin" = "b_pp"),
              by = c("comparison", "population", "uncoded", "model", "adjust"))
  stopifnot(nrow(twins) > 0, all(abs(twins$b_pp - twins$b_twin) < 1e-6))
  out
}

# ---- the payload of the curve ----------------------------------------------------
# curve: spec_curve.csv; options: menu_options.csv; marks: reference_marks.csv;
# analysts: analysts5.csv. The mark settings suit a curve of 64, whose marks
# barely overlap, so they are drawn larger and less faint than on a curve of
# hundreds, and remain translucent.
mv_payload <- function(curve, options, marks, analysts, file = "spec-curve-housing.csv",
                       size = c(4.5, 6), sw = "1",
                       op = list(sig = "0.55", ns = "0.3", ghostSig = "0.12", ghostNs = "0.06")) {
  curve <- curve |> dplyr::arrange(.data$b_pp)
  levels <- purrr::map(mv_axes$axis, \(a) mv_levels(curve, options, a)) |>
    rlang::set_names(mv_axes$key)
  code <- purrr::map2(mv_axes$axis, levels, \(a, lev) match(curve[[a]], lev$option) - 1L) |>
    rlang::set_names(mv_axes$key)
  an <- mv_marks(marks, analysts, curve)
  lab_of <- \(k, i) levels[[k]]$label[code[[k]][i] + 1L]
  phrase_of <- \(k, i) levels[[k]]$phrase[code[[k]][i] + 1L]

  stat_line <- dplyr::if_else(
    curve$stat_type == "t",
    sprintf("t = %s on %d df, %s", mv_minus(curve$stat, 2), as.integer(curve$df), mv_p_label(curve$p)),
    sprintf("z = %s, %s", mv_minus(curve$stat, 2), mv_p_label(curve$p))
  )
  detail <- purrr::map(seq_len(nrow(curve)), \(i) list(
    c("Comparison", lab_of("cp", i)),
    c("Emails counted", lab_of("po", i)),
    c("Uncoded replies", lab_of("un", i)),
    c("Model", lab_of("mo", i)),
    c("", paste0(mv_upper_first(phrase_of("mo", i)), "."), "n"),
    c("Adjustment", lab_of("ad", i)),
    c("Emails analysed", sprintf("%s, %s with Hispanic names", format(curve$n[i], big.mark = ","),
                                 format(curve$n_hisp[i], big.mark = ","))),
    c("Greeted by name", sprintf("%.1f%% of the Hispanic names, %.1f%% of the comparison names",
                                 curve$rate_hisp[i], curve$rate_comp[i])),
    c("Randomisation test", sprintf("p = %s", formatC(curve$p_ri[i], format = "fg", digits = 2)))
  ))

  b_all <- c(curve$b_pp, an$b_pp)
  y_min <- floor((min(b_all) - 2) / 5) * 5
  y_max <- max(5, ceiling((max(b_all) + 2) / 5) * 5)
  list(
    summary = sprintf(paste(
      "Specification curve of %d specifications, ranked by the gap in greetings by name from %s to %s",
      "percentage points; %d lie below zero, in the direction of the claim, and %d of them with p below 0.05."),
      nrow(curve), mv_minus(min(curve$b_pp)), mv_minus(max(curve$b_pp)),
      sum(curve$b_pp < 0), sum(curve$b_pp < 0 & curve$p < 0.05)),
    r = round(curve$b_pp, 3),
    st = round(curve$stat, 3),
    df = curve$df,
    p = signif(curve$p, 3),
    sg = as.integer(curve$p < 0.05),
    sl = I(stat_line),
    det = detail,
    ref = if (any(curve$is_reference)) which(curve$is_reference) - 1L else -1L,
    refText = "The reference specification: the raw gap of the paper",
    reversed = FALSE,
    target = "claim",
    yLabel = "Gap in greetings by name (percentage points)",
    yDec = 0L,
    vfmt = list(pre = "", suf = " points", d = 2L),
    tagGap = 4,
    xStep = 16L,
    xPad = 12L,
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
    csvHead = c("rank", "b_pp", "statistic", "df", "p", "significant_5pc"),
    y = list(min = y_min, max = y_max, ticks = I(seq(y_min, y_max, by = 5))),
    file = file,
    an = purrr::pmap(
      list(an$tag, an$title, an$b_pp, an$rank, an$sg, an$stat, an$lines, an$notes),
      \(tag, title, b, rank, sg, stat, lines, notes) list(
        label = tag, title = title, r = round(b, 3), rank = rank, sg = sg, stat = stat,
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
