---
aliases:
  - /data/reference_marks_codebook.html
---

# Codebook: the reference marks

The multiverse chart of the course marks six results that come from outside the curve: the design of the original study (Teney 2016, *European Sociological Review*; the original author, OA, from here on) and the five reanalyses of the same claim that Multi100 recorded. `reference_marks.csv` places each of them on the scale of the curve, the partial correlation, and keeps beside it what the record says and why the two may differ. It is written by `R/eu-frames/13_reference_marks.R`. Two of the six placements rest on refits of a respondent-level design, whose aggregate results are kept in `companion/_model-outputs/reference_refits.csv`. Neither file holds any respondent-level data.

## How each result is placed

A test statistic becomes a partial correlation through r = t / sqrt(t^2 + df). The conversion is sound when the degrees of freedom count the units that carry the evidence under the error structure that produced the t, so each result is placed on the degrees of freedom of its own inference. Where the software of an analyst counted those units, the value that Multi100 recorded stands: KQXUE and C6HJR, whose residual df count country-years, and KEVF1 and 018OL, whose Satterthwaite df count cells rather than respondents. PRL47 reported no degrees of freedom (Stata printed a z), and Multi100 converted its statistic on the respondent count, which compresses any statistic towards zero; it is converted on the Satterthwaite degrees of freedom of a refit of its design on the rebuilt respondents. The original study is placed differently. The coefficient that the paper prints belongs to a model with interactions between education and the economic measures, so it is the slope of unemployment in the low-education band alone, while the five reanalyses estimate one slope for everyone. Its mark is therefore the refit of its design without the interactions, whose slope is the average over the education bands, with the statistic and the Satterthwaite degrees of freedom of that refit. The model with the interactions is taught in the individual-level module.

`r_233` converts every statistic on 233 df, the residual degrees of freedom of the two-way design on the 270 country-years. It is a teaching device for the lesson on conversion, which shows how much of the spread between the six results comes from the choice of df; it is not where the marks are drawn.

## Columns of `reference_marks.csv`

| Column | Meaning |
|:--|:--|
| `id` | `OA`, `KQXUE`, `C6HJR`, `KEVF1`, `PRL47` or `018OL` |
| `label` | A short label for the chart |
| `unit` | The units of the analysis and their number as recorded |
| `design` | The design in a phrase |
| `statistic_type` | `t` or `z` |
| `statistic_recorded` | The statistic as the paper printed it (OA) or as Multi100 recorded it |
| `df_recorded` | The degrees of freedom of the record; empty where none was reported |
| `r_recorded` | The partial correlation that Multi100 recorded; empty for OA, for which Multi100 recorded no effect size |
| `statistic_placement` | The statistic that is placed: the recorded one, except for OA, whose mark is the refit of its design without the education interactions |
| `df_placement` | The degrees of freedom on which the mark is placed |
| `df_basis` | Where `df_placement` comes from |
| `r_placement` | `statistic_placement` converted on `df_placement`: where the mark sits on the curve |
| `r_233` | `statistic_placement` converted on 233 df |
| `b_natural`, `se_natural`, `b_unit` | The slope in the units of its outcome, its standard error and the unit, where the record or the paper reports the slope. For OA they are those of the refit without the interactions, the slope averaged over the education bands; for C6HJR the slope and standard error are those of the reproduced specification, which reproduces the recorded t exactly |
| `p_recorded` | The p-value as reported, as text, because two analysts reported "0.000" and the paper reports a threshold |
| `p_cluster_check` | Where the design can be fitted on the country-year cells, the p-value of the course refit of that design under the rule of the curve (the wild cluster bootstrap by country); empty where it cannot |
| `note` | What the record does not show: the value reported before cleaning, the refit behind a placement, and what the refit could not reproduce |

## The six marks

| Mark | Statistic | df of the placement | Placed r | r on 233 df | Bootstrap check |
|:--|--:|--:|--:|--:|--:|
| OA | t = −8.39 (refit without the interactions; the paper prints t = −4.03) | 403.9 (refit) | −0.385 | −0.482 | – |
| KQXUE | t = −7.169 | 242 | −0.419 | −0.425 | 0.015 |
| C6HJR | t = −3.804 | 233 | −0.242 | −0.242 | 0.110 |
| KEVF1 | t = −5.899 | 135.6 | −0.452 | −0.360 | 0.075 |
| PRL47 | z = −3.594 | 404.5 (refit) | −0.176 | −0.229 | – |
| 018OL | t = −1.206 | 396.9 | −0.060 | −0.079 | – |

The checks of the three cell-level designs read as follows. For C6HJR it is the reproduced specification itself. For KQXUE it is the point of the curve that matches the design most closely: country fixed effects on the series that KQXUE merged (Eurostat, the 20–64 population), with respondents who mention nothing left out, as the code of KQXUE leaves them out. For KEVF1 it is the country fixed-effects analogue of its random-intercept design, fitted on the 135 country-years in 26 countries that its series covers, with the count of the five cosmopolitan items as the outcome. Its slope (−0.0303) and partial correlation (−0.452) match the record; its bootstrap p is 0.075. The designs of OA, PRL47 and 018OL cannot be fitted on the country-year cells, because their units are respondents or country-waves.

## The two refits

`companion/_model-outputs/reference_refits.csv` has one row per refit and the coefficient on unemployment in each.

| Column | Meaning |
|:--|:--|
| `refit_id` | `OA_published`, the design of Table 3, model 1; `OA_zero_rule`, the same with respondents who mention nothing scored 0; `OA_no_interactions`, the same without the interactions of growth and unemployment with education; `PRL47_B8_fixed_slopes`, the design of PRL47 model B8 as far as the files allow; `PRL47_individual_only`, without its macro controls; `PRL47_zero_rule`, with respondents who mention nothing scored 0 |
| `design`, `noop`, `term` | OA or PRL47; `drop` or `zero`; the name of the unemployment term |
| `b`, `se`, `t` | The coefficient, its standard error and its t |
| `df_satterthwaite`, `p` | The Satterthwaite degrees of freedom of `lmerTest` and the p-value on them |
| `r_on_df` | The refit t converted on its own Satterthwaite df |
| `sd_country`, `sd_country_wave`, `sd_residual` | The standard deviations of the random intercepts and of the residual |
| `respondents`, `country_waves`, `countries` | The respondents with every variable of the model, the 420 country-waves and the 27 countries |

Both designs are fitted by maximum likelihood, as the `mixed` command of Stata fits them by default, with random intercepts for countries and for country-waves within them. The OA design takes the cosmopolitan share of the respondents who mention at least one item, with sex, age (centred at 48), three education bands (low as reference), eight occupation groups (manual workers as reference), three community types (large towns as reference), GDP growth and unemployment (centred at 1.41 and 8.76), and the interactions of the two economic measures with education. Its unemployment coefficient is therefore the slope in the low-education band. The refit `OA_no_interactions` drops the interactions, so its coefficient is the slope averaged over the bands; it is the refit by which the mark of OA is placed. The PRL47 design takes the same share, with the unemployment rate of the 25–64 population from the Eurostat labour force survey, sex, age centred within the wave, education with the middle band as reference, occupation, community type (small towns as reference), GDP growth and an indicator for the years from 2009. The survey wave stands in for the calendar quarter of PRL47, and the annual series stand in for its quarterly ones. The refit leaves out what the files cannot supply: GDP per capita, the change in unemployment, the random slopes of the individual variables in model B8, and the Candidate Countries wave of 2004 that PRL47 added.

## The Multi100 record: `analysts5.csv`

One row per Multi100 analyst of this claim, as Multi100 recorded the Task 2 result: `analyst_id`, `statistic_type`, `statistic`, `df1` (the degrees of freedom of the record), `n` (the sample size of the record), `r` (the partial correlation that Multi100 computed), `d` (its Cohen's d), `tool` and `document` (the software and the format of the analysis), and `note`. The effect size of 018OL is d = −0.121, the value of the processed Multi100 data; copies of this file made before 25 October 2026 give +0.121. The analyst ticked "Opposite as claimed by the original study" for a negative estimate, and Multi100 carried that tick into the sign of its d.
