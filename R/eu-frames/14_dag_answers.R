##############################################################################
# Title:        14_dag_answers.R
# Purpose:      The answers that the causal-graph task of the browser lab
#               prints (Task 2), computed with dagitty (Textor et al. 2016,
#               International Journal of Epidemiology) for every graph to
#               which the graph cell of the lab leads: the five committed
#               arrows with any combination of the five arguable arrows that
#               the cell leaves commented out, 32 graphs in all. For each
#               graph, the minimal adjustment sets for the total effect of
#               unemployment on framing. The lab reads these answers because
#               dagitty cannot run there: its graph algorithms need a
#               JavaScript engine that a browser grants only to a page served
#               with cross-origin isolation headers, which the course site
#               does not send.
# Reads:        nothing; the arrows are those of the graph cell of
#               cases/eu-frames/lab.qmd
# Writes:       cases/eu-frames/data/dag_answers.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_paths.R")

# The arrows of the graph cell, written "cause -> effect": the cell writes
# them as formulas, effect ~ cause, so `framing ~ unemployment` is
# "unemployment -> framing". The five committed arrows, then the five that the
# cell leaves commented out, in its order. A change to the cell needs the same
# change here.
committed <- c("unemployment -> framing", "country -> framing", "country -> unemployment",
               "year -> framing", "year -> unemployment")
commented <- c("growth -> framing", "growth -> unemployment", "unemployment -> bailout",
               "bailout -> framing", "politicisation -> framing")
roles <- list(exposure = "unemployment", outcome = "framing")

# Every combination of the commented arrows, from none of them to all five
added <- purrr::map(0:length(commented), \(k) utils::combn(commented, k, simplify = FALSE)) |>
  purrr::list_flatten()

# One block of rows per graph, one row per minimal set, in the order in which
# dagitty returns and prints them. A graph without any valid set would keep
# one row with item 0, so that the lab can tell it apart from a graph that the
# table does not hold; no graph of the task has one. dplyr::mutate(), because
# the columns are built from the arguments of the lambda, which datawizard
# cannot see.
answers <- purrr::imap(added, \(extra, graph) {
  arrows <- c(committed, extra)
  dag <- dagitty::dagitty(paste0("dag {\n", paste(c(
    paste(roles$exposure, "[exposure]"), paste(roles$outcome, "[outcome]"), arrows),
    collapse = "\n"), "\n}"))
  sets <- dagitty::adjustmentSets(dag, effect = "total")
  tibble::tibble(
    effect = "total",
    item = if (length(sets) > 0) seq_along(sets) else 0L,
    answer = if (length(sets) > 0) unname(purrr::map_chr(sets, \(s) paste(s, collapse = ", "))) else NA_character_) |>
    dplyr::mutate(graph = graph, added = paste(extra, collapse = "; "),
                  arrows = paste(sort(arrows, method = "radix"), collapse = "; "),
                  exposure = roles$exposure, outcome = roles$outcome, .before = 1)
}) |>
  purrr::list_rbind()

readr::write_csv(answers, out_path("dag_answers.csv"))  # r-style: keep – datawizard::data_write() writes other bytes
