# Codebook: `reference_marks.csv`

The multiverse chart of the cohabitation case marks results that come from outside the curve: the published result of the original authors (OAs), its refit on the course data, the reference specification, the five reanalyses that Multi100 recorded, the SCORE replication and the result that SCORE called its reproduction, and the reference specification fitted to later surveys. `reference_marks.csv` holds each of them with its statistic and a note on its source, 12 rows, and `on_chart` says whether the chart of the curve draws it. It is written by `R/cohabitation/04_reference_marks.R`, together with `analysts5.csv` ([codebook](analysts5_codebook.md)) and `display_reproduction.csv` ([codebook](displays_codebook.md)).

## The marks

- **OA** (on the chart). Table 2 of the paper, Model 2: the coefficient of the period 2005–2010 in the marriage equation, −0.683 (SE 0.14, p < 0.001), on 54,291 person-months; z = −4.88, which Multi100 recorded as −4.8786 (r = −0.0209). Model 2 includes the interaction of race-ethnicity and period, so the coefficient is the change for non-Hispanic white women, not for all cohabitors; the chart mark reads "OAs 2016".
- **OA_refit.** Model 2 refitted on the rebuilt unions with design-based errors: −0.686 (SE 0.138). This is the second part of the reproduction of Task 3, after the test of 5X3KG.
- **reference.** The reference specification of the curve, the same model without the interaction: −0.730 (SE 0.111).
- **73FDT, N8ZYW, 456AP, 5X3KG, JDA15.** The Multi100 analyses refitted on the file that SCORE built and Multi100 gave them, with the statistic and the estimate of each (see `analysts5.csv`). Their estimates are on different scales, and only 456AP (−0.526, a multinomial coefficient on person-months of 2002 and 2013–2015) is on the scale of the curve; the chart draws it and leaves the other four off. N8ZYW (+2.93, the log odds ratio of having married by the interview, later survey against earlier) would sit far to the right of every specification.
- **SCORE_replication.** The claim that SCORE tested in its replication is the interaction of some college and period of Model 1 (−0.725, SE 0.27 in the paper), not the period coefficient of Multi100. On the person-months of 2002 against 2011–2013 and 2013–2015 of its file, without weights, it found −0.365 (SE 0.226, z = −1.62, p = 0.106) on 46,239 person-months of 2,437 women, "not replicated". The refit of its script reproduces these values.
- **SCORE_reproduction.** The same script on 2002 against 2006–2010, which the report of the replication calls its reproduction: −0.779 (SE 0.258, z = −3.03). The report gives the p-value as 0.02484; the refit gives 0.0025.
- **course_2002_2011, course_1995_2011.** The reference specification fitted to other surveys of the NSFG rebuilt in the same way: 2002 against 2011–2015 gives −0.450 (SE 0.105) and 1995 against 2011–2015 gives −0.979 (SE 0.102). Both are replications of the change between the cohorts on surveys that the OAs did not use (2002 and 2011–2015).

## Columns

| Column | Meaning |
|:--|:--|
| `mark` | The code of the mark |
| `label` | The label for the chart |
| `source` | Where the result comes from, and how it was obtained |
| `b` | The estimate: for the course fits and the OA, the coefficient of the later cohort (or of the interaction) on the log scale; for the analysts, the estimate described in `analysts5.csv` |
| `se` | Its standard error, where one exists (design-based for the course fits; model-based for the SCORE refits, which are unweighted) |
| `stat_type`, `stat`, `df` | The statistic (`t` with its df for the course fits, `z`, `F` with 3 df, or `chi2`) |
| `n` | The person-months, rows or women analysed |
| `on_chart` | TRUE for the marks that the chart of the curve draws |
| `z_model` | For the course fits, the conventional z, from which `r` is computed as on the curve |
| `p` | The two-sided p-value (for 73FDT the p-value of its regression on five points; for 5X3KG that of the chi-square on one df) |
| `r` | The partial correlation by the Multi100 rule: from the recorded or refitted statistic, and for the course fits from `z_model` and the person-months |
