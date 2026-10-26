################################################################################
# Title:        _chart_multiverse.R
# Purpose:      Builds the interactive specification curve of the multiverse
#               page of the immigration-and-trust case: the payload of the
#               curve (the 192 specifications ranked by the coefficient of
#               concern about immigration, the option of every decision, the
#               lines of each pop-up and the reference marks) and the raw HTML
#               that places it on the page. The drawing and the interaction
#               live in assets/chart/multiverse.js, the styles in
#               assets/chart/multiverse.css and the markup of one chart in
#               assets/chart/multiverse.template.html, shared by the cases of
#               the course; the optional fields of the payload that adapt the
#               chart to this curve are listed at the top of multiverse.js.
# Author:       Claude for CM
# Last updated: 2026-10-26
# Sourced by:   cases/trust/multiverse.qmd
################################################################################

# The decisions that vary on the curve, in the order of the "Colour by" and
# "Show only" menus. The keys are the short names that the payload and
# assets/chart/multiverse.js use.
mv_axes <- tibble::tribble(
  ~key, ~axis,      ~name,
  "oc", "outcome",  "Outcome",
  "ad", "controls", "Adjustment",
  "wt", "weights",  "Weights",
  "it", "items",    "Unanswered items",
  "fe", "fe",       "Countries and rounds"
)

# The tag, the title and the relation to the menu of each reference result.
# None of the ten is a point of the curve: the paper, the SCORE teams and the
# analysts fitted multilevel models with country-level variables, or used
# options that the menu keeps off the curve.
mv_menu_relation <- tibble::tribble(
  ~id,               ~tag,             ~title,
  ~relation,
  "OA",              "OA 2012",        "The original author (2012), Table 3, parliament",
  "Outside the menu: a three-level model with random intercepts for countries and country-rounds, the controls of the paper and its country-level variables, fitted in HLM on 110,732 respondents, without saying how it treated missing answers.",
  "SCORE_jp7tr",     "SCORE jp7tr",    "SCORE reproduction from the ESS files (osf.io/jp7tr)",
  "Outside the menu: the model of Table 3 refitted in lme4 on data rebuilt from the ESS. The reference specification of the curve, fitted to the same 69,522 respondents, comes within 0.001 of it.",
  "SCORE_65wm6",     "SCORE e48gd",    "SCORE reproduction from the ESS files (osf.io/e48gd)",
  "Outside the menu: the model of Table 3 in lme4 on a second rebuild of the data, the file that Multi100 later gave the analysts. Switzerland drops out for want of an unemployment rate.",
  "N8P2J",           "N8P2J",          "Multi100 analyst N8P2J",
  "Outside the menu: the model of Table 3 on the file of the analysts, which reproduces the SCORE reproduction osf.io/e48gd.",
  "FQ1G7",           "FQ1G7",          "Multi100 analyst FQ1G7",
  "Outside the menu: a random intercept for each country and a short list of controls, without the evaluations of the economy and of public services or the vote controls. It sits among the specifications that hold outlook and ideology constant.",
  "LT39B",           "LT39B",          "Multi100 analyst LT39B",
  "Outside the menu: concern measured by one item, and satisfaction with the government among the controls, two options that the menu treats as weaker answers.",
  "456AD",           "456AD",          "Multi100 analyst 456AD",
  "Outside the menu: a shorter list of controls with satisfaction with the government, and country intercepts with random intercepts for country-rounds.",
  "6U3LL",           "6U3LL",          "Multi100 analyst 6U3LL",
  "Outside the menu: the model of Table 3 without unemployment on the file of the analysts, the reproduction target of Task 3.",
  "SCORE_1634",      "SCORE 2010",     "SCORE replication on ESS round 5 (osf.io/rpumq)",
  "New data: the model fitted to the next round of the ESS, fielded in 2010, with post-stratification weights.",
  "SCORE_65wm6_DAR", "SCORE 2010–18",  "SCORE replication on ESS rounds 5–9 (osf.io/e48gd)",
  "New data: the three-level model fitted to the five rounds of the ESS fielded from 2010 to 2018."
)

# ---- formatting ----------------------------------------------------------------
# A typographic minus for negative numbers, and an en-dash in ranges, in text
# that the data files write with hyphens.
mv_minus <- \(x, d = 1) sub("^-", "−", formatC(x, format = "f", digits = d))
mv_big <- \(x) format(round(x), big.mark = ",", trim = TRUE)
mv_p_label <- \(p) dplyr::if_else(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
mv_clean <- \(x) x |>
  stringr::str_replace_all("(?<=\\d)-(?=\\d)", "–") |>
  stringr::str_replace_all("(?<![\\w.])-(?=[0-9.])", "−")
mv_upper_first <- \(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
mv_placement <- \(rank, n) dplyr::case_when(
  rank == 0 ~ sprintf("None of the %d specifications has a larger coefficient", n),
  rank == 1 ~ sprintf("1 of the %d specifications has a larger coefficient", n),
  .default = sprintf("%d of the %d specifications have a larger coefficient", rank, n)
)

# ---- the labels of the menu ------------------------------------------------------
# The options of one decision that occur on the curve, in the order of
# menu_options.csv, with their labels.
mv_levels <- function(curve, options, axis_code) {
  lev <- options |>
    dplyr::filter(.data$axis == axis_code, .data$option %in% curve[[axis_code]]) |>
    dplyr::select("option", "label")
  stopifnot(all(curve[[axis_code]] %in% lev$option))
  lev
}

# ---- the reference marks ---------------------------------------------------------
# Each mark is placed by its coefficient on the scale of the curve and, among
# the specifications, after those with a larger coefficient. Its class comes
# from the statistic as published or recorded, which treats respondents as
# independent or fits random intercepts; the paper prints p = 0.000.
mv_marks <- function(marks, analysts, curve) {
  # not n: a lambda built inside dplyr::mutate() would find the column n of the marks first
  n_spec <- nrow(curve)
  out <- marks |>
    dplyr::filter(.data$on_chart) |>
    dplyr::left_join(mv_menu_relation, by = "id") |>
    dplyr::left_join(analysts |> dplyr::select("id", "outcome_txt" = "outcome", "concern_txt" = "concern",
                                 "method", "task1", "df_f" = "df2_recorded"),
              by = "id") |>
    dplyr::mutate(
      rank = purrr::map_int(.data$b, \(x) sum(curve$b > x + 1e-9)),
      p_num = dplyr::case_when(
        is.na(.data$statistic) ~ 0,
        .data$statistic_type == "F" ~ pf(.data$statistic, 1, .data$df_f, lower.tail = FALSE),
        .default = 2 * pnorm(-abs(.data$statistic))
      ),
      sg = as.integer(.data$p_num < 0.05),
      source = dplyr::case_when(
        startsWith(.data$id, "SCORE") ~ "as published by SCORE",
        .default = "as recorded by Multi100"
      ),
      stat = dplyr::case_when(
        .data$id == "OA" ~ "b = 0.17 with a standard error printed as 0.00 and p = 0.000, as published; no test statistic can be recovered",
        .data$statistic_type == "F" ~ sprintf("F = %s on 1 and %s df, %s; %s",
                                              formatC(.data$statistic, format = "f", digits = 2),
                                              format(.data$df_f, big.mark = ","), .data$source,
                                              mv_p_label(.data$p_num)),
        .default = sprintf("%s = %s, %s; %s", .data$statistic_type,
                           formatC(.data$statistic, format = "f", digits = 3), .data$source,
                           mv_p_label(.data$p_num))
      ),
      outcome_txt = dplyr::case_when(
        is.na(.data$outcome_txt) ~ "distrust in parliament",
        .data$outcome_txt == "trust in parliament" ~ "trust in parliament, turned round to distrust for the chart",
        .default = .data$outcome_txt
      ),
      concern_txt = dplyr::case_when(
        is.na(.data$concern_txt) ~ "index of the three items",
        .data$concern_txt == "index (any item)" ~ "index of the three items, over the items answered",
        .default = .data$concern_txt
      ),
      method = dplyr::case_when(
        !is.na(.data$method) ~ .data$method,
        .data$id == "OA" ~ "three-level model of Table 3, HLM",
        .data$id == "SCORE_1634" ~ "two-level model with post-stratification weights, 13 of the 16 countries",
        .default = "three-level model of Table 3, lme4"
      ),
      lines = purrr::pmap(
        list(.data$outcome_txt, .data$concern_txt, .data$method, .data$n, .data$task1, .data$rank),
        \(outcome, concern, method, n_resp, task1, rank) {
          rows <- list(
            c("Outcome", mv_upper_first(outcome)),
            c("Concern", mv_upper_first(concern)),
            c("Model", mv_upper_first(method)),
            c("Respondents", mv_big(n_resp)),
            if (!is.na(task1)) c("Task 1 conclusion", task1),
            c("Placement", mv_placement(rank, n_spec))
          )
          purrr::compact(rows)
        }
      ),
      notes = purrr::map2(.data$relation, .data$note, \(rel, note) I(c(rel, mv_clean(note))))
    )
  stopifnot(nrow(out) == sum(marks$on_chart), !anyNA(out$tag))
  out
}

# ---- the payload of the curve ----------------------------------------------------
# curve: spec_curve.csv; options: menu_options.csv; marks: reference_marks.csv;
# analysts: analysts5.csv. The curve is sorted from the largest coefficient
# down, so that the strongest support for the claim sits on the left, and `up`
# tells the chart that values above zero lie in the direction of the claim.
# The vertical range leaves room above the curve for the tags of the ten
# reference marks, eight of which fall within twenty ranks of one another.
mv_payload <- function(curve, options, marks, analysts, file = "spec-curve-trust.csv",
                       size = c(4, 5), sw = "0.9",
                       op = list(sig = "0.45", ns = "0.3", ghostSig = "0.1", ghostNs = "0.05"),
                       y = list(min = -0.05, max = 0.45, step = 0.05)) {
  curve <- curve |> dplyr::arrange(dplyr::desc(.data$b), .data$spec_id)
  levels <- purrr::map(mv_axes$axis, \(a) mv_levels(curve, options, a)) |>
    rlang::set_names(mv_axes$key)
  code <- purrr::map2(mv_axes$axis, levels, \(a, lev) match(curve[[a]], lev$option) - 1L) |>
    rlang::set_names(mv_axes$key)
  an <- mv_marks(marks, analysts, curve)
  lab_of <- \(k, i) levels[[k]]$label[code[[k]][i] + 1L]

  stat_line <- sprintf("t = %s on %d df, errors clustered by country · %s",
                       mv_minus(curve$stat, 2), as.integer(curve$df), mv_p_label(curve$p))
  detail <- purrr::map(seq_len(nrow(curve)), \(i) list(
    c("Outcome", lab_of("oc", i)),
    c("Adjustment", lab_of("ad", i)),
    c("Weights", lab_of("wt", i)),
    c("Unanswered items", lab_of("it", i)),
    c("Countries and rounds", lab_of("fe", i)),
    c("Respondents", sprintf("%s in %d country-rounds", mv_big(curve$n[i]), curve$n_country_rounds[i])),
    c("Conventional errors", sprintf("t = %s on %s df", mv_minus(curve$t_conv[i], 2), mv_big(curve$df_conv[i]))),
    c("Clustered by country-round", sprintf("t = %s on %d df", mv_minus(curve$t_cr[i], 2), as.integer(curve$df_cr[i]))),
    c("Partial correlation", sprintf("r = %s, by the rule of Multi100", mv_minus(curve$r[i], 3)))
  ))

  list(
    summary = sprintf(paste(
      "Specification curve of %d specifications, ranked by the coefficient of concern about immigration",
      "from %s down to %s points of distrust per point of concern; %d lie above zero, in the direction of",
      "the claim, and %d of them with p below 0.05 under errors clustered by country."),
      nrow(curve), mv_minus(max(curve$b), 3), mv_minus(min(curve$b), 3),
      sum(curve$b > 0), sum(curve$b > 0 & curve$p < 0.05)),
    r = round(curve$b, 4),
    st = round(curve$stat, 3),
    df = curve$df,
    p = signif(curve$p, 3),
    sg = as.integer(curve$p < 0.05),
    sl = I(stat_line),
    det = detail,
    ref = if (any(curve$is_reference)) which(curve$is_reference) - 1L else -1L,
    refText = "The reference specification: the model of the paper, with fixed effects for country-round",
    reversed = FALSE,
    target = "claim",
    up = TRUE,
    yLabel = "Points of distrust per point of concern",
    yDec = 2L,
    vfmt = list(pre = "", suf = " points of distrust per point of concern", d = 3L),
    tagGap = 0.02,
    xStep = 50L,
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
    csvHead = c("rank", "b", "t_clustered", "df", "p", "significant_5pc"),
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
