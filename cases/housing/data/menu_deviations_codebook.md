# Codebook: `menu_deviations.csv`

What each option of the curve does to the estimate. For each of the five decisions that vary on the curve of the housing-audit case (`spec_curve.csv`, [codebook](spec_curve_codebook.md)), every specification is set beside its twin that differs from it only in that decision, the twin taking the first option of the decision, and the differences are summarised by option: 12 rows. The file is written by `R/housing/04_menu_deviations.R` from `spec_curve.csv`, so it holds no fit of its own. The decision map and the specification menu read it.

## Columns

| Column | Meaning |
|:--|:--|
| `axis` | The decision: `comparison`, `population`, `uncoded`, `model` or `adjust` |
| `baseline` | The first option of the decision, against which the others are compared: `white`, `replies`, `drop`, `lpm` and `none` |
| `option` | The option. Each decision has a row for its baseline, in which every shift is 0 |
| `n_pairs` | The pairs of a specification with the option and its twin with the baseline: 32 for the four decisions with two options, 16 for each option of the adjustment |
| `shift_mean`, `shift_min`, `shift_max` | The mean, smallest and largest difference in the estimate, in percentage points, of the specification with the option minus its twin. The estimates are negative, so a positive shift moves the estimate towards zero |
| `b_mean` | The mean estimate, in points, of the specifications with the option |
| `sig_share` | The share of the specifications with the option whose test gives p < 0.05, as a proportion from 0 to 1 |
| `flips` | The pairs whose two members fall on different sides of p = 0.05 |

## What the file shows

Counting every delivered email instead of the replies moves the estimate by +4.95 points on average (from +3.66 to +6.70), from a mean of −17.7 to −12.7 points. Pooling the Black names with the white ones moves it by +1.27 (from +0.31 to +2.65), counting the uncoded replies as no greeting by +0.34 (from +0.16 to +0.67), and the logit instead of the linear model by −0.01 (from −0.06 to +0.06). Against no covariates, the hard-to-find address moves it by +0.08, the size and poverty of the community by +0.95 and the set with its ethnic composition by +1.06; part of the last two shifts comes from the 14 emails without community thirds, which those sets leave out. Every specification stays significant at 5%, so `sig_share` is 1 and `flips` is 0 on every row.
