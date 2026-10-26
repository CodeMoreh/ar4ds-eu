---
aliases:
  - /data/menu_options_codebook.html
---

# Codebook: `menu_options.csv`

`menu_options.csv` lists every option of the fourteen-decision specification menu of the course, one row per option, 52 rows in all. It records for each option whether it is on the curve of the claim, the 864 specifications of `spec_curve.csv`, which page of the course teaches it, and the reasoning on whether it is a justifiable way of answering the claim under test, "poor national economic performances significantly decrease positive EU-framing dimensions" (Teney 2016, *European Sociological Review*), read as a claim about change within countries over time. The file is written by hand, not by a script, and the reasoning in it follows the audit of the menu that settled which options the curve keeps.

Three parts of the course read it. The decision map (`cases/eu-frames/map.qmd`) builds its stages, its selectable options, the estimand statement and the description of each option from it. The specification menu (`cases/eu-frames/menu.qmd`) teaches the same options at length. And the helpers of the browser lab and of the workspace read `axis`, `option`, `on_curve`, `taught_on` and `reason` to explain why an option that is off the curve is refused or labelled, so those five columns and the codes in them must not change without a matching change in the helpers.

## Columns

| Column | Meaning |
|:--|:--|
| `axis` | The decision, as a code: `outcome`, `construction`, `predictor`, `copredictor`, `sample`, `weights`, `estimator` (Level 1); `noop`, `item_set`, `cell_weight`, `measure` (Level 2); `pred_form`, `family`, `se` (Level 3). The codes are those of the curve files and the helpers |
| `level` | `estimand`, `measurement` or `estimation`, the level of the menu at which the decision sits. Inference (`se`) sits inside Level 3 |
| `option` | The option code, as the curve files, the deviation files and the helpers write it |
| `label` | A short label for the option, used on the decision map and in messages |
| `on_curve` | `TRUE` for an option that the curve of 864 keeps, `FALSE` for one that the course teaches off the curve |
| `taught_on` | The page that teaches the option, as a path from the project root |
| `reason` | One sentence on why the option is or is not on the curve, used by the helpers in their messages |
| `decision` | The name of the decision, as the decision map and the menu print it |
| `stage` | The stage of the decision map at which the decision is made: `data` (the data and the sample), `outcome`, `predictor`, `comparison`, `average` (whose average), `model` (the model form) or `inference` |
| `judgement` | The verdict on the option for this claim: `kept` (on the curve, a justifiable answer); `other_question` (it answers a different question, about another outcome, construct, population or comparison); `weaker_answer` (it answers this question, but less well than the options on the curve, because an assumption that it needs fails on these data); `check` (a diagnostic or a second fit of an answer that the curve already holds) |
| `phrase` | The phrase that the option writes into the estimand statement of the decision map |
| `estimates` | What the option estimates, in one sentence |
| `explanation` | Two or three sentences on whether and why the option is a justifiable way of answering the claim, for the description that the decision map opens when the option is selected |

## The phrases of the estimand statement

The decision map assembles the statement from the `phrase` column in a fixed order: "Among [sample], with [whose average], the association between [predictor] ([series]) and [frame set and scoring rule] ([item set], [respondents who mention nothing], [cell means]), comparing [estimator], [adjustment]." A second sentence carries the Level 3 decisions: "Answered with [outcome family], [predictor form], and tested by [inference]." Four placeholders are filled by the page:

- `{frames}` in the phrases of the scoring rule takes the phrase of the frame set (for example "cosmopolitan");
- `{k}` in the phrase of the rate takes the number of items in the scale as a word, which depends on the frame set and the item set;
- `{other}` in the phrase of the adjustment takes the economic measure that is not the predictor ("GDP growth" or "the unemployment rate");
- `{pred}` in the phrase of the raw form takes the predictor ("unemployment" or "GDP growth").

Two series also rewrite the phrase of the sample, because they do not cover the whole panel: the Eurostat table tipsun20 (`es_1574_tips`) and Eurostat growth (`es_growth`). The page takes the number of country-years and countries that each leaves from `menu_deviations_rows.csv.gz`.

## Codes that differ between files

The inference options use the codes of the helpers. In `menu_deviations.csv` and `menu_deviations_rows.csv.gz` the same rules carry the codes of the deviation pipeline, and in `spec_curve.csv` they are columns:

| `menu_options.csv` | `menu_deviations.csv` (`axis` = `se`) | Column of `spec_curve.csv` |
|:--|:--|:--|
| `wcr` | (the rule of the curve, no row) | `p_wcr` |
| `cr2` | `cr2` | `p_cr2` |
| `cr1` | `cr1_g1` | `p_cr1` |
| `conventional` | `conventional` | `p_conv` |
| `wcr_webb` | `wcr_webb` | `p_wcr_webb` |

The within–between model (`estimator` = `rewb`) and the bootstrap with Webb signs (`se` = `wcr_webb`) are options off the curve that appear in the deviation files; both are listed here so that the decision map can show them. The rate (`construction` = `rate`) has no pairs of its own in the deviation files, because in a linear model it gives the test of the count, and the decision map draws the pairs of the count for it.

## Numbers quoted in the text

The prose columns avoid numbers that the data files hold, which the pages compute when they render. A few figures are quoted in them from the audit of the menu, because no committed data file holds them: the correlation of the residuals of a country from one year to the next in the reproduced model (0.53); the share of the unexplained variation in that model that is sampling noise (about 7%); the share of the variation of growth within countries that is common to all countries in a year (59%); the share of the variation of unemployment within countries that year effects absorb (29%); and the claim-aligned correlation of the communitarian scale at the reproduced specification with and without the item "unemployment" (−0.495 and −0.290). The p-values of the reproduced specification under conventional errors and under the bootstrap (0.0002 and 0.110) are those of `display_inference.csv`, and the ratio of the largest to the smallest adult population of a country-year (215) is that of `teney_panel_weights.csv`.

## Editing

Keep the existing columns and codes, and keep `reason` to one sentence. Every prose column is participant-facing, so it follows the writing conventions of the course: British spelling, spaced en-dashes, double quotation marks for quotation and single ones for terms held at a distance, relative clauses written out in full, and no possessive on an institution or an artefact. A new option needs a row here, its fits in the deviation files, and a decision in the helpers about how to treat it.
