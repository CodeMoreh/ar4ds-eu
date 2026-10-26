# Codebook: `analysts5.csv`

The five Multi100 reanalyses of the claim of the original authors (OAs), "cohabitors have increasingly become less likely to progress to marriage", one row per analyst: what Multi100 recorded about each analysis, the course refit of it on the file that the analysts were given, and the same analysis on that file with its 1995 cohort rebuilt. Written by `R/cohabitation/04_reference_marks.R` from the five-row extract of the Multi100 record (`data-raw/cohabitation/multi100/`) and the public scripts and reports of the analysts on OSF (osf.io/b69nv).

All five analysts worked from the same file, which SCORE had built for its replication of the paper from five NSFG surveys (1995, 2002, 2006–2010, 2011–2013, 2013–2015; osf.io/b6p3j): 70,188 person-months of 3,963 women, without weights or design variables. Its 1995 cohort is faulty. It holds 376 unions instead of the 929 of the paper, and 369 of them end in separation and 4 in marriage, because SCORE took the end of each 1995 union from the date on which the woman stopped living with a former partner, a date that exists only for unions that dissolved (`display_score_file.csv`, [codebook](displays_codebook.md)). One analyst, 456AP, noticed it and compared 2002 with 2013–2015 instead: "the data for 1995 in the OSF dataset seemed faulty, since there were only 4 marriages on record (in the paper 340 marriages were listed for 1995)". N8ZYW reported the 4 marriages of 1995 without questioning them. Multi100 told every analyst, in Task 2: "You should not include interactions between education and period in your analysis."

The course refits each analysis on the SCORE file, which the course ships unchanged as `score_person_months.csv` ([codebook](score_person_months_codebook.md)), and runs it again on a repaired copy of the file in which the 1995 rows are replaced by the 929 unions of the course rebuild, written in the conventions of the SCORE file (an intact union contributes as many months as it lasted and a union that ended one more, with the outcome in the last; age rounded up; education with the associate degree as college). The other four surveys are left as SCORE built them, so that the difference between the two refits is the 1995 cohort alone.

## The five analyses

- **73FDT** (psychology, R, 14 hours): one row per woman (her last month), a multinomial logit of the outcome on five survey dummies with the total duration of the union and age, then a linear regression of the five marriage coefficients on the middle year of each survey: F(1, 3) = 12.86, which Multi100 converted to r = 0.900. The five coefficients include the faulty 1995 one, and the conversion treats five points as the sample.
- **N8ZYW** (psychology, jamovi, 4 hours): one row per woman of 1995 and 2006–2010 (1,526), a logit of marriage against no marriage on the later survey, two education and two race-ethnicity dummies, the total duration and age: odds ratio 18.68 (z = 5.71), marriage "more likely" in the later cohort. Multi100 recorded no statistic.
- **456AP** (political science, R, 7 hours): every person-month of 2002 and 2013–2015 (34,514), a multinomial logit of marrying or separating against staying together on the later survey, education, race-ethnicity and age, without a term for time: −0.526, z = −5.02.
- **5X3KG** (anthropology, R, 5 hours): the share of person-month rows that record a marriage in 1995 against 2006–2010 (23,949 rows), compared by `prop.test()` with the continuity correction: chi-square 47.47, marriage more common in the later rows.
- **JDA15** (public policy, R, 2 hours): one row per woman of the five surveys (3,963), a logit of marriage against no marriage on education × race-ethnicity, age, the total duration and years since 1995: −0.008 per year, z = −1.13.

## Columns

| Column | Meaning |
|:--|:--|
| `id` | The Multi100 identifier of the analyst |
| `discipline` | The primary discipline that the analyst gave |
| `hours` | Total hours that the analyst reported for both tasks |
| `task1` | The verdict of Task 1, as Multi100 plotted it: "Same conclusion", "Opposite effect" or "No effect/inconclusive" |
| `direction` | The direction of the Task 2 result, as recorded: "Same as claimed by the original study" or "Opposite as claimed by the original study" (JDA15 recorded "Same" with p = 0.257 and a verdict of no evidence either way) |
| `software` | The software of Task 2 |
| `statistic_type`, `statistic_recorded`, `df1_recorded`, `df2_recorded`, `n_recorded` | The test statistic that Multi100 recorded for Task 2, its degrees of freedom and the sample size (empty for N8ZYW) |
| `r_recorded`, `d_recorded` | The partial correlation and Cohen's d that Multi100 computed from the recorded statistic |
| `comparison` | The surveys or years that the analysis compares |
| `unit` | The unit of analysis: one row per woman or person-months |
| `outcome_model` | The model or test |
| `controls` | The covariates of the model |
| `stat_type_refit`, `stat_refit`, `n_refit` | The statistic of the course refit on the SCORE file and the rows that it analyses (F for 73FDT, chi2 for 5X3KG, z otherwise) |
| `estimate_refit` | The estimate behind the statistic: the slope per year of the five coefficients (73FDT), the coefficient of the later survey or of the year (N8ZYW, 456AP, JDA15, on the log-odds scale), or the difference between the shares of rows that record a marriage, later minus earlier (5X3KG). Negative values point the way of the claim |
| `p_refit`, `r_refit` | The two-sided p-value of the refit and its partial correlation by the Multi100 rule |
| `stat_repaired`, `n_repaired`, `estimate_repaired`, `p_repaired`, `r_repaired` | The same analysis on the SCORE file with its 1995 cohort rebuilt |
| `matches_record` | TRUE when the refit reproduces the recorded statistic (to within 0.006 in absolute value) on the recorded sample size; empty for N8ZYW, whose statistic Multi100 did not record (the refit gives z = 5.71 and an odds ratio of 18.68, the values of the report of the analyst) |

## What the refits show

The four recorded statistics reproduce on the SCORE file: F = 12.859 (12.86 recorded), z = −5.020 (−5.02), chi-square 47.474 (47.474) and z = −1.134 (−1.13). With the 1995 cohort rebuilt, every analysis that uses 1995 points the way of the claim: N8ZYW moves from z = +5.71 to −9.65, 5X3KG from more marriage in the later rows (+0.010) to less (−0.013, chi-square 87.7), JDA15 from z = −1.13 to −12.28, and 73FDT keeps a negative slope (F = 12.74). The analysis of 456AP does not use 1995 and is unchanged. The disagreement that Multi100 recorded on this claim, two analysts finding the opposite of the claim and one finding no evidence, comes from the faulty 1995 cohort of the file and from the choice of an analysis on one row per woman, not from the data of the paper.
