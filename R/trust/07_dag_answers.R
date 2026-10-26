################################################################################
# Title:        07_dag_answers.R
# Purpose:      The answers that the causal-graph task of the browser lab
#               prints (Task 2), computed with dagitty (Textor et al. 2016,
#               International Journal of Epidemiology) for every graph to
#               which the graph cell of the lab leads: the graph as the cell
#               draws it, with the arrow of the claim switched off, switched
#               on (concern -> distrust) or reversed (distrust -> concern),
#               each with the far-right vote and the verdict on services
#               either a common effect of concern and distrust, as drawn, or a
#               cause of distrust. For each graph, the minimal adjustment sets
#               for the total and for the direct effect of concern on
#               distrust, and, under the four adjustment sets of the menu,
#               every path between the two with whether it is open, and
#               whether the set is an adjustment set. The lab reads these
#               answers because dagitty cannot run there: its graph
#               algorithms need a JavaScript engine that a browser grants only
#               to a page served with cross-origin isolation headers, which
#               the course site does not send.
# Reads:        nothing; the arrows are those of the graph cell of
#               cases/trust/lab.qmd and of R/dag_starter.R in the workspace
# Writes:       cases/trust/data/dag_answers.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

# The arrows of the graph cell, written "cause -> effect" as the cell writes
# them: the ten that the cell draws, the arrow of the claim, which it leaves
# switched off, and the two arrows that it offers to turn round. A change to
# the cell needs the same change here.
drawn <- c("background -> concern", "background -> distrust", "pessimism -> concern",
           "pessimism -> distrust", "ideology -> concern", "ideology -> distrust",
           "concern -> far_right", "distrust -> far_right", "concern -> services", "distrust -> services")
claim <- list(character(0), "concern -> distrust", "distrust -> concern")
swaps <- c("distrust -> far_right" = "far_right -> distrust", "distrust -> services" = "services -> distrust")
roles <- list(exposure = "concern", outcome = "distrust", latent = character(0))

# The conditioning sets that the questions of the task give to paths() and
# isAdjustmentSet(): none, as the workspace asks it first, and the four
# adjustment sets of the menu written in the nodes of the graph – the
# background ("demog"), with pessimism and ideology ("outlook"), and with the
# far-right vote and the verdict on services as well ("paper")
conditioning <- list(character(0), "background", c("background", "ideology", "pessimism"),
                     c("background", "far_right", "ideology", "pessimism", "services"))

# Every graph: the arrow of the claim in each of its three states, crossed with
# every combination of the two arrows turned round
graphs <- tidyr::expand_grid(claim = claim,
                             turned = purrr::map(0:length(swaps), \(k) utils::combn(names(swaps), k,
                                                                                     simplify = FALSE)) |>
                               purrr::list_flatten())

# One block of rows per graph. The sets and paths keep the order in which
# dagitty returns them, which is the order in which it prints them. A graph
# without any valid set keeps one row with item 0, so that the lab can tell it
# apart from a graph that the table does not hold: the graphs with the arrow
# of the claim reversed have none for the total effect. dplyr::mutate(),
# because the columns are built from the arguments of the lambda, which
# datawizard cannot see.
answers <- purrr::pmap(list(graphs$claim, graphs$turned, seq_len(nrow(graphs))), \(claim, turned, graph) {
  arrows <- c(replace(drawn, match(turned, drawn), swaps[turned]), claim)
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
    dplyr::mutate(graph = graph, added = paste(setdiff(arrows, drawn), collapse = "; "),
                  removed = paste(setdiff(drawn, arrows), collapse = "; "),
                  arrows = paste(sort(arrows, method = "radix"), collapse = "; "),
                  exposure = roles$exposure, outcome = roles$outcome,
                  latent = paste(sort(roles$latent, method = "radix"), collapse = ", "),
                  .before = 1)
}) |>
  purrr::list_rbind()

readr::write_csv(answers, "cases/trust/data/dag_answers.csv")  # r-style: keep – datawizard::data_write() writes other bytes
