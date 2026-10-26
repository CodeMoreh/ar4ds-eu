# Codebook: the display files of the cohabitation case

Seven small files that the pages of the case use beside the curve. Six are written by `R/cohabitation/06_displays.R` and one, `display_reproduction.csv`, by `R/cohabitation/04_reference_marks.R`, all from the union file `cohab_unions.csv` ([codebook](cohab_unions_codebook.md)) and, for `display_score_file.csv`, from the person-month file that SCORE built. Coefficients are on the scale of the curve, the log of the ratio of the monthly chances of marrying in the later and the earlier cohort, and the reference specification is that of `spec_curve.csv` ([codebook](spec_curve_codebook.md)): the cohorts of the paper of the original authors (OAs), every observed month, age, race-ethnicity and education held constant, weighted, months since the start as a straight line, a multinomial logit and design-based errors, −0.730 on 33,197 person-months.

## `display_reproduction.csv`

Models 1 and 2 of Table 2 of the paper refitted on the rebuilt unions, every coefficient of both equations beside its published value: 56 rows. Model 1 has the interaction of education and period, Model 2 that of race-ethnicity and period; both control for age at the start (with its square) and the month since the start, and both are weighted, with design-based errors. The paper does not print the coefficients of age and duration, so those rows have no published value.

| Column | Meaning |
|:--|:--|
| `model` | `1` or `2` |
| `equation` | `marriage` (marrying against staying together) or `separation` (separating against staying together) |
| `term` | The coefficient: `(Intercept)`, `later` (the period 2005–2010), `age`, `age2`, `black`, `hispanic`, `other` (against non-Hispanic white), `less_hs`, `hs`, `some_college` (against a bachelor's degree or more), the interactions `later_*`, and `month` |
| `b`, `se` | The course refit and its design-based error |
| `b_published`, `se_published` | The values of Table 2 |

The refit comes close throughout. The period coefficient of Model 2, the coefficient of the claim, is −0.686 (SE 0.138) against −0.683 (0.14); the interaction of some college and period in Model 1, the claim of the SCORE replication, is −0.729 (0.275) against −0.725 (0.27). The largest gaps are in the constants of the marriage equations (−6.13 against −5.82 in Model 2) and in the interactions of education and period in Model 1 (−0.79 against −0.96 for less than high school, −1.11 against −1.02 for a high school diploma); apart from the constants, every error agrees with the published one to within about 0.01 (the largest gap, 0.010, is in the separation equation of Model 1).

## `display_lifetable.csv`

The multi-decrement life table of the two cohorts of the paper, overall, by education and by race-ethnicity: for every month of the union, the weighted share of the unions still intact at its start that married or separated in it, and the cumulative shares that had married, separated or were still cohabiting by its end. Separation competes with marriage, so the three cumulative shares sum to one. 1,018 rows (2 cohorts × 9 groups × up to 61 months). The table equals the Aalen–Johansen estimator of `survival::survfit()` with the same weights, which `check_parity.R` checks.

| Column | Meaning |
|:--|:--|
| `cohort` | `1990-1995` or `2005-2010` |
| `group_type`, `group` | `all`; `education` with `less_hs`, `hs`, `some_college`, `college`; or `race` with `white`, `black`, `hispanic`, `other` |
| `month` | The month of the union, 1 being the month in which the couple moved in |
| `at_risk`, `w_at_risk` | The unions still intact at the start of the month, unweighted and weighted |
| `marriages`, `separations` | The unions that married or separated in the month (unweighted counts) |
| `h_marriage`, `h_separation` | The weighted shares of the unions at risk that married or separated in the month |
| `cohabiting`, `married`, `separated` | The cumulative shares by the end of the month |

By the end of month 36, 45.3% of the unions of 1990–1995 had become a marriage and 37.6% had dissolved; of the unions of 2005–2010, 28.7% and 45.6%. The paper quotes 41% and 35%, and 24% and 41%, from a life table that it did not publish ("available upon request"). The course table is higher by 2.6 to 4.7 points in every cell, and its difference between the cohorts in marriage (−16.6 points) is close to the published one (−17). The unweighted table gives 40.4% and 21.9% for marriage.

## `display_shares.csv`

The shares within three years from the life table (month 36), beside the counts of each group and the share of unions that had become a marriage by the interview: 18 rows (2 cohorts × 9 groups).

| Column | Meaning |
|:--|:--|
| `cohort`, `group_type`, `group` | As in the life table |
| `married_3y`, `separated_3y`, `cohabiting_3y` | The weighted shares married, separated and still cohabiting by the end of month 36 |
| `unions` | The unions of the group (unweighted) |
| `months_mean` | The mean number of months that a union of the group contributes |
| `married_by_interview` | The weighted share of the unions that had become a marriage by the interview, whatever their length: the measure that three Multi100 analysts used |

Within three years, marriage fell among cohabitors without a bachelor's degree (from 47% to 20% for a high school diploma, from 45% to 24% for some college, from 35% to 17% for less than high school) and not among graduates (50% to 55%), the divergence that is the main finding of the paper. By race-ethnicity it fell in every group. The groups `other` hold 26 and 63 unions, too few for their shares to mean much.

## `display_subgroups.csv`

The change for each group in the interaction models of the paper: in Model 1, the coefficient of the later cohort plus its interaction with each group of education; in Model 2, plus its interaction with each group of race-ethnicity. Design-based errors of the sums, t on the df of the model. 8 rows; columns `model` (`educ_period` or `race_period`), `group`, `b`, `se`, `t`, `df`, `p`.

The change is −0.13 (not significant) for graduates, −0.86 for some college, −1.24 for a high school diploma and −0.92 for less than high school; −0.69 for non-Hispanic white women, −0.40 (not significant) for Black women, −1.14 for Hispanic women and −1.61 for the other group. The period coefficient of Model 1 is therefore the change for graduates only, and that of Model 2, which the claim quotes, the change for white women only.

## `display_options.csv`

Eleven options off the curve, each fitted once at the reference settings with every other decision held there. `changed` names the argument of `fit_spec()` that differs from the reference and `option` its value; the other columns are those of `spec_curve.csv`.

| Option | b | SE | p |
|:--|--:|--:|--:|
| Model 2 of the paper (`race_period`) | −0.686 | 0.138 | < 0.001 |
| Model 1 of the paper (`educ_period`) | −0.129 | 0.180 | 0.475 |
| Total length of the union as a control | −1.104 | 0.166 | < 0.001 |
| No term for time | −0.730 | 0.109 | < 0.001 |
| Linear probability model (difference in the monthly probability) | −0.0137 | 0.0020 | < 0.001 |
| Education as in the SCORE file | −0.724 | 0.112 | < 0.001 |
| Unions of less than a month kept | −0.707 | 0.114 | < 0.001 |
| NSFG 2002 against 2006–2010 | −0.204 | 0.117 | 0.084 |
| NSFG 1995 against 2011–2015 | −0.979 | 0.102 | < 0.001 |
| NSFG 2002 against 2011–2015 | −0.450 | 0.105 | < 0.001 |
| NSFG 2006–2010 against 2011–2015 | −0.231 | 0.125 | 0.067 |

: Options off the curve at the reference settings (display_options.csv).

The comparisons of other surveys take the unions begun in the five years before each interview, and in 2006–2010 only those begun from 2005, as the paper does.

## `display_inference.csv`

The reference specification under the three rules of inference: design-based errors (SE 0.111, t = −6.55 on 241 df), errors clustered by woman (SE 0.108, t = −6.74 on 2,070 df) and conventional errors (SE 0.085, z = −8.56). 3 rows, the columns of `spec_curve.csv` with `errors` naming the rule.

## `display_score_file.csv`

The cohorts of the person-month file that SCORE built and Multi100 gave the analysts, beside the course rebuild with the window of the paper applied to every survey: 10 rows (`source` = `SCORE file` or `rebuilt`, by `cycle`), with `unions`, `marriages`, `separations`, `intact` and `person_months` (the rows of the SCORE file; start to end inclusive for the rebuild).

The SCORE file holds 376 unions of 1995, 369 of them separations, 4 marriages and 3 intact, where the rebuild and the paper hold 929 (340, 347 and 242). Its 2006–2010 cohort (1,150 unions: 195, 509, 446) is close to that of the paper (1,142: 194, 504, 444); the eight extra unions began 61 to 63 months before the interview. For 2002 and the two later surveys the unions of the SCORE file began in 1997 or later, 2008 or later and 2010 or later, which points to windows of calendar years rather than the five-year rule, so its counts differ from those of the rebuild.
