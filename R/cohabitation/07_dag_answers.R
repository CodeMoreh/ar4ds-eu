################################################################################
# Title:        07_dag_answers.R
# Purpose:      The answers that the causal-graph task of the browser lab
#               prints (Task 2), computed with dagitty (Textor et al. 2016,
#               International Journal of Epidemiology) for every graph to
#               which the graph cell of the lab leads: the graph as the cell
#               draws it, and the same graph with the arrow of the claim
#               (period -> marriage), the arrow from the period to the age at
#               the start (period -> age_start), or both switched on. For each
#               graph, the minimal adjustment sets for the total and for the
#               direct effect of the period on marriage, and, under the
#               conditioning sets that the task and the menu use, every path
#               between the two with whether it is open, and whether the set
#               is an adjustment set. The lab reads these answers because
#               dagitty cannot run there: its graph algorithms need a
#               JavaScript engine that a browser grants only to a page served
#               with cross-origin isolation headers, which the course site
#               does not send.
# Reads:        nothing; the arrows are those of the graph cell of
#               cases/cohabitation/lab.qmd and of R/dag_starter.R in the
#               workspace
# Writes:       cases/cohabitation/data/dag_answers.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

# The arrows of the graph cell, written "cause -> effect" as the cell writes
# them: the seven that the cell draws, and the two that it leaves switched off
# for the participant. A change to the cell needs the same change here.
drawn <- c("period -> cohabits", "education -> cohabits", "race -> cohabits", "education -> marriage",
           "race -> marriage", "education -> age_start", "age_start -> marriage")
switched_off <- c("period -> age_start", "period -> marriage")
roles <- list(exposure = "period", outcome = "marriage", latent = character(0))

# The conditioning sets that the questions of the task give to paths() and
# isAdjustmentSet(): none, as the workspace asks it first; the selection into
# cohabitation, which every comparison of the data holds fixed, alone and with
# each adjustment set of the menu (the age at the start, "age"; and with
# education and race-ethnicity, "composition") and with education and
# race-ethnicity alone, as the task asks; and the same variables without the
# selection, for the comparison among all women
conditioning <- list(character(0), "cohabits", c("age_start", "cohabits"), c("cohabits", "education", "race"),
                     c("age_start", "cohabits", "education", "race"), "age_start", c("education", "race"),
                     c("age_start", "education", "race"))

# Every combination of the switched-off arrows, from none of them to both
added <- purrr::map(0:length(switched_off), \(k) utils::combn(switched_off, k, simplify = FALSE)) |>
  purrr::list_flatten()

# One block of rows per graph. The sets and paths keep the order in which
# dagitty returns them, which is the order in which it prints them. A graph
# without any valid set would keep one row with item 0, so that the lab can
# tell it apart from a graph that the table does not hold; no graph of the
# task has one. dplyr::mutate(), because the columns are built from the
# arguments of the lambda, which datawizard cannot see.
answers <- purrr::imap(added, \(extra, graph) {
  arrows <- c(drawn, extra)
  dag <- dagitty::dagitty(paste0("dag {\n", paste(c(
    paste(roles$exposure, "[exposure]"), paste(roles$outcome, "[outcome]"), arrows), collapse = "\n"), "\n}"))
  set_rows <- purrr::map(c("total", "direct"), \(effect) {
    sets <- dagitty::adjustmentSets(dag, effect = effect)
    tibble::tibble(
      call = "adjustmentSets", effect = effect, Z = NA_character_,
      item = if (length(sets) > 0) seq_along(sets) else 0L,
      answer = if (length(sets) > 0) unname(purrr::map_chr(sets, \(s) paste(s, collapse = ", "))) else NA_character_,
      open = NA)
  }) |>
    purrr::list_rbind()
  path_rows <- purrr::map(conditioning, \(z) {
    found <- dagitty::paths(dag, Z = z)
    tibble::tibble(call = "paths", effect = NA_character_,
                   Z = paste(sort(z, method = "radix"), collapse = ", "),
                   item = seq_along(found$paths), answer = found$paths, open = found$open)
  }) |>
    purrr::list_rbind()
  check_rows <- purrr::map(conditioning, \(z) {
    tibble::tibble(call = "isAdjustmentSet", effect = "total",
                   Z = paste(sort(z, method = "radix"), collapse = ", "),
                   item = 1L, answer = as.character(dagitty::isAdjustmentSet(dag, z)), open = NA)
  }) |>
    purrr::list_rbind()
  purrr::list_rbind(list(set_rows, path_rows, check_rows)) |>
    dplyr::mutate(graph = graph, added = paste(extra, collapse = "; "),
                  arrows = paste(sort(arrows, method = "radix"), collapse = "; "),
                  exposure = roles$exposure, outcome = roles$outcome,
                  latent = paste(sort(roles$latent, method = "radix"), collapse = ", "),
                  .before = 1)
}) |>
  purrr::list_rbind()

readr::write_csv(answers, "cases/cohabitation/data/dag_answers.csv")  # r-style: keep – datawizard::data_write() writes other bytes
