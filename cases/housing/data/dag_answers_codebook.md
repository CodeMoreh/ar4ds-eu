# Codebook: `dag_answers.csv`

`dag_answers.csv` holds every answer that dagitty, the R package for causal graphs (Textor et al. 2016, *International Journal of Epidemiology*), gives to the questions of Task 2 of the [browser lab](../lab.qmd) for the two graphs to which the graph cell of the lab leads: the graph as the cell draws it, with six arrows, and the same graph with the arrow of the claim, `name -> greeting`, switched on. The downloadable workspace asks dagitty itself. The browser lab cannot, because dagitty runs its graph algorithms in a JavaScript engine that a browser grants only to a page served with cross-origin isolation headers, and the course site does not send them. The lab therefore reads this table: `make_dag()` turns the arrows of the cell into the key in the column `arrows`, and `adjustmentSets()` and `paths()` print the stored answers in the form in which dagitty prints them. A graph or a question that the table does not hold gets a message that points to the workspace. The file is written by `R/housing/07_dag_answers.R` with dagitty 0.3.4.

Each graph has one row for its adjustment set and one row for each path between the name and the greeting under three conditioning sets: none, the reply, and the reply with the community. The graph as drawn has three paths and the graph with the arrow of the claim four, so the file has 23 rows.

| Column | Meaning |
|:--|:--|
| `graph` | The number of the graph: 1 for the graph as the cell draws it, 2 with the arrow of the claim switched on |
| `added` | The arrows switched on beyond the six that the cell draws; empty for graph 1 |
| `arrows` | Every arrow of the graph, written `cause -> effect`, sorted in the C locale and separated by `; `, the key by which the lab finds the graph |
| `exposure`, `outcome`, `latent` | The roles declared in the call to `make_dag()`: the name, the greeting, and the official, whom nobody observed |
| `call` | The question that the row answers, `adjustmentSets` or `paths` |
| `effect` | For an adjustment set, the effect for which it is valid, `total`; `NA` for a path |
| `Z` | For a path, the variables on which the comparison conditions, sorted and separated by commas, and empty when it conditions on nothing; `NA` for an adjustment set |
| `item` | The position of the set or the path in the output of dagitty. A graph for which no adjustment set exists would have one row with 0, which no graph of the task has |
| `answer` | For an adjustment set, its variables separated by commas, empty for the empty set, which dagitty prints as `{}`; for a path, the path as dagitty writes it, for example `name -> reply <- official -> greeting` |
| `open` | For a path, `TRUE` when it is open given the variables in `Z` and `FALSE` when they block it; `NA` for an adjustment set |

Both graphs have a single minimal adjustment set for the total effect of the name on the greeting, the empty set, because nothing points into a name that was assigned at random. With nothing conditioned on, the only open paths are those that run forward from the name: through the reply, and directly once the arrow of the claim is switched on. Conditioning on the reply, as a comparison among replies does, blocks the path through the reply and opens the two paths that meet at the reply, a common effect of the name and of each of the community and the official. Adding the community to the conditioning set blocks the path through the community, and the path through the official stays open, because the official was not observed and so cannot be adjusted for.
