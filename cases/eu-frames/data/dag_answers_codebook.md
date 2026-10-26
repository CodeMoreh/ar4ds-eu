# Codebook: `dag_answers.csv`

`dag_answers.csv` holds the adjustment sets that dagitty, the R package for causal graphs (Textor et al. 2016, *International Journal of Epidemiology*), gives for every graph to which the graph cell of Task 2 of the [browser lab](../lab.qmd) leads. The cell draws five committed arrows and leaves five arguable ones commented out, one claim per line: growth as a cause of framing and of unemployment, the bailout as a mediator between unemployment and framing, and politicisation as a cause of framing. Any combination of the five can be switched on, which makes 32 graphs. The downloadable workspace asks dagitty itself. The browser lab cannot, because dagitty runs its graph algorithms in a JavaScript engine that a browser grants only to a page served with cross-origin isolation headers, and the course site does not send them. The lab therefore reads this table: `dagify()` turns the formulas of the cell into the key in the column `arrows`, and `adjustmentSets()` prints the stored sets in the form in which dagitty prints them. A graph that the table does not hold gets a message that points to the workspace. The file is written by `R/eu-frames/14_dag_answers.R` with dagitty 0.3.4.

Each graph has one row for each minimal adjustment set for the total effect of unemployment on framing. Every graph of the task has exactly one, so the file has 32 rows.

| Column | Meaning |
|:--|:--|
| `graph` | The number of the graph, from 1 (the five committed arrows only) to 32 (all ten arrows) |
| `added` | The arguable arrows switched on, written `cause -> effect` in the order of the cell and separated by `; `; empty for graph 1 |
| `arrows` | Every arrow of the graph, written `cause -> effect`, sorted in the C locale and separated by `; `, the key by which the lab finds the graph. The cell writes each arrow as a formula, effect on the left, so `framing ~ unemployment` is `unemployment -> framing` |
| `exposure`, `outcome` | The roles declared in the call to `dagify()`: unemployment and framing |
| `effect` | The effect for which the set is valid, `total` |
| `item` | The position of the set in the output of dagitty. A graph for which no adjustment set exists would have one row with 0, which no graph of the task has |
| `answer` | The variables of the set, separated by commas, which dagitty prints between braces, for example `{ country, year }` |

The minimal adjustment set is `{ country, year }`, which is what two-way fixed effects absorb, in 24 of the 32 graphs. In the other eight, both growth arrows are switched on, and the set becomes `{ country, growth, year }`: growth is then a common cause of unemployment and framing that varies within a country over time, which fixed effects for country and year do not remove. One growth arrow alone leaves the set unchanged, because growth is then a cause of only one of the two. The bailout arrows never change it, because the bailout lies on a path from unemployment to framing, and the arrow from politicisation never changes it, because politicisation has no route into unemployment.
