# Codebook: `menu_deviations.csv`

What each option of the five decisions that vary on the curve does to the estimate. For every decision, the first option of the menu is the baseline, and every specification is set against its twin that differs from it on that decision alone; the file summarises those pairs, one row per option, 15 rows. Written by `R/trust/05_menu_deviations.R` from `spec_curve.csv` ([codebook](spec_curve_codebook.md)); the decision map reads it.

## Columns

| Column | Meaning |
|:--|:--|
| `axis` | The decision: `outcome`, `controls`, `weights`, `items` or `fe` |
| `baseline` | The first option of the decision, against which the others are compared: `parliament`, `none`, `none`, `complete`, `country_round` |
| `option` | The option compared with the baseline (the baseline itself has a row with shifts of 0) |
| `n_pairs` | The pairs of twin specifications: 192 divided by the number of options of the decision |
| `shift_mean`, `shift_min`, `shift_max` | The mean, smallest and largest change in `b` (points of distrust per point of concern) from the baseline twin to the option |
| `b_mean` | The mean of `b` over the specifications with the option |
| `n_mean` | The mean number of respondents analysed with the option |
| `n_shift_mean` | The mean change in the number of respondents from the baseline twin |
| `sig_share` | The share of the specifications with the option that are significant at 5% under errors clustered by country |
| `flips` | The pairs in which the option and its baseline twin fall on different sides of p = 0.05 |

## What it shows

The adjustment set is the only decision that moves the estimate far: the demographics leave it where it is (−0.004 on average), outlook and ideology lower it by 0.076, and the full controls of the paper by 0.140, from 0.290 without controls to 0.151 on average. Each step also loses respondents, about 24,000 when household income enters. Among the outcomes, distrust of politicians lies 0.046 below distrust of parliament on average, and the legal system and the mean of the three about 0.025 below. Weights move the estimate by −0.007 on average (design and population weights) and +0.001 (design weights), the rule for unanswered items by −0.0035 while adding about 4,000 respondents, and country and round effects by +0.003 against country-round effects. No option changes the verdict of any specification: every row has `sig_share` = 1 and `flips` = 0.
