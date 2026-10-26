# Codebook: `reference_marks.csv`

The multiverse chart of the immigration-and-trust case marks results that come from outside the curve: the published result of the original author (OA), the two SCORE reproductions from source data, the five reanalyses of the same claim that Multi100 recorded, and the two SCORE replications on later rounds of the ESS. `reference_marks.csv` places each of them on the scale of the curve, the coefficient of concern about immigration on distrust in parliament, and keeps beside it the statistic of the record and a note on how the mark was placed: 10 rows. It is written by `R/trust/04_reference_marks.R`, together with `analysts5.csv` ([codebook](analysts5_codebook.md)) and `display_reproduction.csv` ([codebook](displays_codebook.md)).

## How each result is placed

Every mark is a coefficient on the scale of the curve: points of distrust in parliament (0–10) per point of concern about immigration (0–10). All ten results are positive, and all are significant.

- **OA.** Table 3 of the paper reports 0.17 for parliament, with a standard error printed as 0.00 and p = 0.000, from a three-level model fitted in HLM on 110,732 respondents, 59 country-rounds and 16 countries. The printed standard error leaves no test statistic to recover, so the mark carries none.
- **The SCORE reproductions.** Both rebuilt the data from the ESS files and fitted the model of Table 3 with lme4. The reproduction osf.io/jp7tr found 0.1780 (t = 41.583) on 69,522 respondents; the reproduction osf.io/e48gd, whose file Multi100 later gave the analysts, found 0.1812 (t = 42.148) on 68,113, without Switzerland. The SCORE outcome files record both as approximate reproductions of the coefficient that fall outside the tolerance for the sample size.
- **The five analysts.** Each is placed by the course refit of the analysis on the file that the analyst used, the file of the reproduction osf.io/e48gd. The refits reproduce four recorded statistics exactly; the fifth, FQ1G7, ran in jamovi with a control, 'year', that the report names and that no variable of the file reproduces on the recorded sample, and its refit matches the recorded sample and comes within 0.1% of the recorded F. FQ1G7 analysed trust rather than distrust, so its coefficient is turned round to the scale of distrust. LT39B measured concern with one item, so its coefficient is per point of that item.
- **The SCORE replications.** Both are taken from the SCORE outcome files: 0.1829 (t = 21.17) on 17,134 respondents of round 5 (2010), and 0.1857 (t = 50.11) on 90,286 respondents of rounds 5–9 (2010–2018). They come from new data, so the figures of the original data leave them out. The replication on round 5 covers 13 of the 16 countries, because the integrated file of the round holds no Austrian or Italian sample and the Portuguese sample lacks household income.

## Columns

| Column | Meaning |
|:--|:--|
| `id` | `OA`, `SCORE_jp7tr`, `SCORE_65wm6`, the five analyst codes (`N8P2J`, `FQ1G7`, `LT39B`, `456AD`, `6U3LL`), `SCORE_1634`, `SCORE_65wm6_DAR` |
| `label` | A longer label for the marks of the OA and of SCORE, and the code for the analysts. The chart prints the mark of `OA` as "OA 2012" |
| `statistic_type` | `t`, `z` (LT39B and 456AD, from Stata), `F` (FQ1G7, one numerator df), or empty for `OA` |
| `statistic` | The statistic as SCORE or Multi100 recorded it; empty for `OA` |
| `n` | The respondents in the analysis |
| `b` | The coefficient by which the result is placed (points of distrust per point of concern) |
| `on_chart` | `TRUE` for every row |
| `data` | `original` for the analyses of rounds 1–4, `new` for the two replications |
| `note` | How the mark was placed |

## The marks

| Mark | Statistic | Respondents | Coefficient |
|:--|--:|--:|--:|
| OA | – | 110,732 | 0.170 |
| SCORE_jp7tr | t = 41.583 | 69,522 | 0.178 |
| SCORE_65wm6 | t = 42.148 | 68,113 | 0.181 |
| N8P2J | t = 42.1 | 68,113 | 0.181 |
| FQ1G7 | F = 3151.34 | 77,384 | 0.235 |
| LT39B | z = 31.747 | 67,358 | 0.115 |
| 456AD | z = 43.14 | 85,241 | 0.157 |
| 6U3LL | t = 46.625 | 72,733 | 0.191 |
| SCORE_1634 | t = 21.17 | 17,134 | 0.183 |
| SCORE_65wm6_DAR | t = 50.11 | 90,286 | 0.186 |

Seven of the eight marks of the original data lie between 0.115 and 0.191, around the specifications of the curve with the full controls of the paper (0.117 to 0.179): LT39B, which measured concern with a single item, sits at the bottom, just below the lowest point of the curve, and 6U3LL at the top. FQ1G7, with a shorter set of controls, sits at 0.235, among the specifications with controls for outlook and ideology (0.183 to 0.249). The two replications on later rounds land at 0.183 and 0.186.
