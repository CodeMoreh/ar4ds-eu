# Codebook: `menu_deviations.csv`

What each option of the six decisions that vary on the curve does to the estimate. For every decision, the first option of the menu is the baseline, and every specification is set against its twin that differs from it on that decision alone; the file summarises those pairs, one row per option, 14 rows. Written by `R/cohabitation/05_menu_deviations.R` from `spec_curve.csv` ([codebook](spec_curve_codebook.md)); the decision map reads it.

## Columns

| Column | Meaning |
|:--|:--|
| `axis` | The decision: `cohorts`, `horizon`, `adjust`, `weights`, `time` or `model` |
| `baseline` | The first option of the decision, against which the others are compared: `paper`, `five_years`, `none`, `weighted`, `linear`, `mlogit` |
| `option` | The option compared with the baseline (the baseline itself has a row with shifts of 0) |
| `n_pairs` | The pairs of twin specifications: 144 divided by the number of options of the decision |
| `shift_mean`, `shift_min`, `shift_max` | The mean, smallest and largest change in `b` (the coefficient of the later cohort on the log scale) from the baseline twin to the option. A positive shift makes the estimate less negative, so a smaller decline |
| `b_mean` | The mean of `b` over the specifications with the option |
| `n_mean` | The mean number of person-months analysed with the option |
| `n_shift_mean` | The mean change in the number of person-months from the baseline twin |
| `sig_share` | The share of the specifications with the option that are significant at 5% under design-based errors |
| `flips` | The pairs in which the option and its baseline twin differ in significance at 5% |

## What the file shows

No option changes the sign or the significance of any specification (`flips` is 0 on every row). The wider window of the later cohort shifts the estimate by +0.095 on average (+0.084 to +0.110) and adds about 11,000 person-months; leaving out the weights shifts it by −0.049 (−0.080 to −0.024); adjusting for age, race-ethnicity and education by +0.033 (+0.014 to +0.057), and for age alone by +0.006. The three-year horizon (+0.006), the yearly baseline (−0.0004), the logit (−0.0003) and the complementary log-log model (+0.008) leave it almost where it was.
