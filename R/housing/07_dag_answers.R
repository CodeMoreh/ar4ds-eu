################################################################################
# Title:        07_dag_answers.R
# Purpose:      The answers that the causal-graph task of the browser lab
#               prints (Task 2), computed with dagitty (Textor et al. 2016,
#               International Journal of Epidemiology) for every graph to
#               which the graph cell of the lab leads: the graph as the cell
#               draws it, and the same graph with the arrow of the claim,
#               name -> greeting, switched on. For each graph, the minimal
#               adjustment sets for the total effect of the name on the
#               greeting, and every path between the two with whether it is
#               open given nothing, the reply, and the reply and the community.
#               The lab reads these answers because dagitty cannot run there:
#               its graph algorithms need a JavaScript engine that a browser
#               grants only to a page served with cross-origin isolation
#               headers, which the course site does not send.
# Reads:        nothing; the arrows are those of the graph cell of
#               cases/housing/lab.qmd and of R/dag_starter.R in the workspace
# Writes:       cases/housing/data/dag_answers.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

# The arrows of the graph cell, written "cause -> effect" as the cell writes
# them: the six that the cell draws, and the one that it leaves switched off
# for the participant. A change to the cell needs the same change here.
drawn <- c("name -> reply", "reply -> greeting", "official -> reply", "official -> greeting",
           "community -> reply", "community -> greeting")
switched_off <- "name -> greeting"
roles <- list(exposure = "name", outcome = "greeting", latent = "official")

# The conditioning sets that the questions of the task give to paths(): none,
# as the workspace asks it first, the reply, and the reply with the community
conditioning <- list(character(0), "reply", c("reply", "community"))

# Every combination of the switched-off arrows, from none of them to all
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
    paste(roles$exposure, "[exposure]"), paste(roles$outcome, "[outcome]"),
    paste(roles$latent, "[latent]"), arrows), collapse = "\n"), "\n}"))
  sets <- dagitty::adjustmentSets(dag, effect = "total")
  set_rows <- tibble::tibble(
    call = "adjustmentSets", effect = "total", Z = NA_character_,
    item = if (length(sets) > 0) seq_along(sets) else 0L,
    answer = if (length(sets) > 0) unname(purrr::map_chr(sets, \(s) paste(s, collapse = ", "))) else NA_character_,
    open = NA)
  path_rows <- purrr::map(conditioning, \(z) {
    found <- dagitty::paths(dag, Z = z)
    tibble::tibble(call = "paths", effect = NA_character_,
                   Z = paste(sort(z, method = "radix"), collapse = ", "),
                   item = seq_along(found$paths), answer = found$paths, open = found$open)
  }) |>
    purrr::list_rbind()
  purrr::list_rbind(list(set_rows, path_rows)) |>
    dplyr::mutate(graph = graph, added = paste(extra, collapse = "; "),
                  arrows = paste(sort(arrows, method = "radix"), collapse = "; "),
                  exposure = roles$exposure, outcome = roles$outcome,
                  latent = paste(sort(roles$latent, method = "radix"), collapse = ", "),
                  .before = 1)
}) |>
  purrr::list_rbind()

readr::write_csv(answers, "cases/housing/data/dag_answers.csv")  # r-style: keep – datawizard::data_write() writes other bytes
