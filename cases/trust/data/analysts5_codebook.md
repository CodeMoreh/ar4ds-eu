# Codebook: `analysts5.csv`

The five Multi100 reanalyses of the claim of the original author (OA), "... concerns about the effect of immigration on the national community have a(n) [reducing] impact on trust in politics", one row per analyst: what Multi100 recorded about each analysis, and the course refit of it. Written by `R/trust/04_reference_marks.R` from the five-row extract of the Multi100 record (`data-raw/trust/multi100/`) and the public scripts and reports of the analysts on OSF (osf.io/9jvbt).

All five analysts worked from the same file, which SCORE had built from ESS rounds 1–4 for its own reproduction from source data (osf.io/e48gd): 110,732 respondents, with the variables of the paper already coded. The course refits each analysis on that file, which `R/trust/04_reference_marks.R` fetches from the OSF and keeps out of the repository, because it holds person-level survey data. Multi100 told every analyst, in Task 2, to "include unhappiness in your set of controls variables", to "focus on trust in parliament", and not to "control for long-term country of immigration interaction with concern about immigration and popular far-right party interaction with concern about immigration".

## The analyses

| Analyst | Discipline | Hours | Software | Outcome | Concern | Model | Recorded | Respondents | Coefficient |
|:--|:--|--:|:--|:--|:--|:--|--:|--:|--:|
| N8P2J | psychology | 4 | R | distrust in parliament | index | the model of Table 3: random intercepts for country and country-round, every control and country-level variable | t = 42.1 | 68,113 | 0.181 |
| FQ1G7 | psychology | 5 | jamovi | trust in parliament | index | country random intercept; happiness, age, sex, education, income, interpersonal distrust, round, far-right share, GDP, unemployment | F = 3151.34 | 77,384 | 0.235 |
| LT39B | political science | 4 | Stata | distrust in parliament | one item: worse or better place to live | most controls of Table 3 (several unreversed) and satisfaction with the government, the governance index and economic variables, country dummies with random intercepts | z = 31.747 | 67,358 | 0.115 |
| 456AD | political science | 10 | Stata | distrust in parliament | index | a shorter set of controls with satisfaction with the government, country-level variables, country dummies with random intercepts | z = 43.14 | 85,241 | 0.157 |
| 6U3LL | psychology | 4 | R | distrust in parliament | index | the model of Table 3 without unemployment, education as coded by the ESS | t = 46.625 | 72,733 | 0.191 |

Every analyst ticked "The results show evidence for the relationship/effect as described in the claim provided in your task", and every one reported a result in the direction of the claim. The coefficient of FQ1G7 is turned round from trust to distrust. The recorded partial correlations run from 0.122 (LT39B) to 0.198 (FQ1G7).

## Columns

| Column | Meaning |
|:--|:--|
| `id` | The Multi100 analyst code |
| `discipline`, `hours` | The primary discipline of the analyst and the hours spent on the two tasks, as recorded |
| `task1` | The verdict of Task 1: "evidence for the claim" for all five |
| `direction` | The direction of the Task 2 result relative to the claim, as recorded |
| `software` | The software of Task 2, as recorded |
| `statistic_type`, `statistic_recorded`, `df1_recorded`, `df2_recorded`, `n_recorded` | The Task 2 statistic, its degrees of freedom and the sample size, as Multi100 recorded them |
| `r_recorded`, `d_recorded` | The partial correlation and Cohen's d that Multi100 computed from the record |
| `outcome`, `concern`, `method` | The outcome, the measure of concern and the model, in short, from the scripts and reports |
| `b_refit`, `se_refit` | The coefficient of concern in the refit, on the scale of distrust (points per point of concern), and its standard error |
| `statistic_refit`, `df_refit`, `n_refit` | The statistic of the refit (t with Satterthwaite df from lmerTest, F for FQ1G7, z for the two Stata models fitted by maximum likelihood), its denominator degrees of freedom where there are any, and the respondents. The results of the multilevel fits are kept to six significant digits, because lme4 can return slightly different values from one run to the next in the eighth |
| `matches_record` | `TRUE` when the refit gives the recorded statistic to the decimals that Multi100 recorded, on the recorded number of respondents |
| `r_refit` | The partial correlation of the refit by the rules of Multi100 |

## What the refits show

Four analyses reproduce exactly. N8P2J reproduces the reproduction osf.io/e48gd itself (0.1812, t = 42.148), because the analyst fitted its model on its file; Multi100 recorded the degrees of freedom of this t as 62,088, where the refit and the SCORE record give 68,082, an apparent transposition of digits that raises the recorded partial correlation from 0.159 to 0.167. FQ1G7 reproduces the recorded sample of 77,384 but not the F to the decimal (3,152.8 against 3,151.34): the report lists "year" among the controls, and the file holds no variable of that name that keeps the sample at 77,384.

The file that the analysts received differs from the paper in ways that none of them flagged: its indicator of long-term immigration countries is coded the wrong way round (1 for Greece, Spain, Portugal, Italy and Ireland, which the paper codes 0), which leaves the coefficient of concern untouched in a model without interactions but turns the sign of the interaction of Table 4; and its far-right vote counts some voters of mainstream parties as far right (among them Dutch liberals in rounds 3 and 4, British Liberal Democrats in round 3, Norwegian Christian Democrats in rounds 3 and 4 and two Belgian parties in round 4), as the course rebuild finds when it compares the two files respondent by respondent.
