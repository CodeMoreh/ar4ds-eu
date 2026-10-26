# Codebook: `menu_options.csv`

`menu_options.csv` lists every option of the specification menu of the immigration-and-trust case, one row per option, 47 rows over 12 decisions. For each option it records whether it is on the curve of the claim, the 192 specifications of `spec_curve.csv` ([codebook](spec_curve_codebook.md)), which page of the course teaches it, and the reasoning on whether it answers the claim under test, "... concerns about the effect of immigration on the national community have a(n) [reducing] impact on trust in politics", in the wording that SCORE took from the paper of the original author (OA). The file is written by hand, not by a script. It follows the schema of the menu files of the housing-audit and EU-frames cases.

Three parts of the course read it. The decision map (`cases/trust/map.qmd`) builds its stages, its selectable options, the estimand statement and the description of each option from it. The specification menu (`cases/trust/menu.qmd`) builds its tables of the three levels from it. And the helpers of the browser lab and of the workspace read `axis`, `option`, `on_curve`, `taught_on` and `reason` to explain why an option off the curve is refused, so those five columns and the codes in them must not change without a matching change in the helpers.

## Columns

| Column | Meaning |
|:--|:--|
| `axis` | The decision, as a code: `outcome`, `controls`, `weights`, `sample` (Level 1); `concern`, `items`, `education`, `votes`, `missing` (Level 2); `fe`, `errors`, `scale` (Level 3). The five decisions that vary on the curve, `outcome`, `controls`, `weights`, `items` and `fe`, carry the codes of the columns of `spec_curve.csv` and of the arguments of `fit_spec()` and `try_spec()` |
| `level` | `estimand`, `measurement` or `estimation`, the level of the menu at which the decision sits |
| `option` | The option code, as the curve, the deviation file and the helpers write it |
| `label` | A short label for the option, used on the decision map, in the tables of the menu and in messages |
| `on_curve` | `TRUE` for an option that the curve of 192 keeps (22 rows: the 15 options of the five varying decisions and the one option that each of the other seven decisions holds fixed), `FALSE` for one that the course teaches off the curve (25 rows) |
| `taught_on` | The page that teaches the option, as a path from the project root: `cases/trust/index.qmd`, `cases/trust/menu.qmd`, `cases/trust/multiverse.qmd`, or one of the companion modules `companion/dags.qmd`, `companion/measurement.qmd`, `companion/weighting.qmd`, `companion/missingness.qmd` and `companion/stats-methods.qmd`. A page in `cases/trust/` links to it two levels up |
| `reason` | One sentence on why the option is or is not on the curve, used by the helpers in their messages and by the verdict of the decision map |
| `decision` | The name of the decision, as the decision map and the menu print it |
| `stage` | The stage of the decision map at which the decision is made: `data` (the respondents and the weights), `outcome`, `predictor` (the measure of concern and the rule for unanswered items), `adjustment` (the controls and how they are coded: education, the vote controls, missing answers), `model` (countries and rounds) or `inference` (the errors and the effect scale) |
| `judgement` | The verdict on the option for this claim: `kept` (on the curve, a defensible answer; 22 rows); `other_question` (it answers a different question, about another outcome or another population; 4); `weaker_answer` (it answers this question, but less well than the options on the curve; 7); `check` (a diagnostic or a second fit of an answer that the curve already holds; 12); `unavailable` (it would answer the question, but the data cannot support it; 2: the seven-category education scale of the paper, and its handling of missing answers, which the paper does not report). The decision map labels the last "Not in the data" |
| `phrase` | The phrase that the option writes into the estimand statement of the decision map |
| `estimates` | What the option estimates, in one sentence |
| `explanation` | Two or three sentences on whether and why the option answers the claim, for the description that the decision map opens when the option is selected |

## The phrases of the estimand statement

The decision map can assemble the statement from the `phrase` column in a fixed order: "[Respondents], [weights], the change in [outcome] that goes with one more point of [measure of concern] ([unanswered items]), [adjustment] ([education], [vote controls], [missing answers])." The respondent phrases begin with "among", which the page capitalises. A second sentence carries the Level 3 decisions: "Estimated by [countries and rounds], tested [standard errors] and reported [effect scale]." The order is a proposal for the page that builds the map.

## Options with numbers elsewhere

Only the options on the curve are fitted across the curve. Eleven options off the curve are fitted once, at the reference settings, by `R/trust/06_displays.R` into `display_options.csv` ([codebook](displays_codebook.md)): all respondents; the three single immigration items; the controls of the paper with satisfaction with the government, and without unhappiness; the post-stratification weights alone and with the population weight; the pooled regression and the random intercepts; and the vote controls counted for voters only. The conventional errors, the clustering by country-round and the wild cluster bootstrap are in `display_inference.csv` and, for the first two, in every row of the curve; the standardised coefficient and the partial correlation are columns of the curve (`b_std`, `r`); the maximum effect is ten times `b`. The coding of the vote controls in the Multi100 file (`largest_party`) is reflected in the five analyses refitted on that file (`analysts5.csv`). The model of the paper with its country-level variables is in `display_reproduction.csv`. Six options have no number: the two outcomes that the extract does not carry (`parties`, `european_parliament`), the countries of Central and Eastern Europe (`all_countries`), multiple imputation, and the two `unavailable` options.

## Editing

Keep the existing columns and codes, and keep `reason` to one sentence. Every prose column is participant-facing, so it follows the writing conventions of the course: British spelling, spaced en-dashes, double quotation marks for quotation and single ones for terms held at a distance, relative clauses written out in full, and no possessive on an institution or an artefact. A new option needs a row here, a decision in the helpers about how to treat it, and, if it joins the curve, an entry in `SPEC_AXES` of `R/trust/_tools.R` and a refit of `spec_curve.csv` and `menu_deviations.csv`.
