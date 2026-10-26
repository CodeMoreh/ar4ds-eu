# Codebook: `spec_curve.csv`

The curve of the claim of the cohabitation case, "cohabitors have increasingly become less likely to progress to marriage", in the wording that SCORE and Multi100 took from the paper of the original authors (OAs, p. 932): every specification that the specification menu of the course keeps, 144 rows, fitted once on the union file `cohab_unions.csv` ([codebook](cohab_unions_codebook.md)). The file is written by `R/cohabitation/03_spec_curve.R` with the estimator of `R/cohabitation/_tools.R` (`fit_spec()`), which uses base R, dplyr, tidyr and purrr only, so that the workspace and the browser lab run the identical code; `R/cohabitation/check_parity.R` checks it against survey, svyVGAM and nnet.

## The specifications

Six decisions vary on the curve, and every combination of their options is one row: 2 × 2 × 3 × 2 × 2 × 3 = 144.

| Column | Options | Meaning |
|:--|:--|:--|
| `cohorts` | `paper`, `five_years` | The unions of NSFG 1995 begun in the five years before the interview (1990–1995) against those of NSFG 2006–2010 begun in 2005 or later, as the paper chose them; or every union begun in the five years before the interview in both surveys (2001–2010 in the later one) |
| `horizon` | `five_years`, `three_years` | Every observed month of the unions (at most five years), or the first 36 months only |
| `adjust` | `none`, `age`, `composition` | Nothing besides the time since the start of the union; the age at the start and its square; those with race-ethnicity and education (the controls of the paper without its interactions) |
| `weights` | `weighted`, `unweighted` | The final weights of the two surveys, or every union counted the same |
| `time` | `linear`, `yearly` | The month since the start of the union as a straight line, or a separate baseline for each year of the union |
| `model` | `mlogit`, `logit`, `cloglog` | A multinomial logit of marrying or separating against staying together (the model of the paper); a logit of marrying in the month; a complementary log-log model of marrying in the month |

The other decisions of the menu take one option on every row: the surveys of 1995 and 2006–2010; women aged 15–44 and the first cohabitation that came before any marriage; the marriage of the partners as the outcome; the union histories rebuilt from the dates of the NSFG; education in the four groups of the paper; unions of less than a month left out; person-months as the unit; design-based errors; and the coefficient reported on the log scale. `menu_options.csv` lists every option ([codebook](menu_options_codebook.md)).

The reference specification (`is_reference`) is `paper`, `five_years`, `composition`, `weighted`, `linear`, `mlogit`: the cohorts, the months, the model and the weights of Model 2 of Table 2 of the paper without its interaction of race-ethnicity and period, which the claim does not ask for. It gives −0.730 (SE 0.111, t = −6.55 on 241 df, ratio 0.48) on 33,197 person-months of 2,071 unions. The coefficient that the claim quotes, −0.683 (SE 0.14), is that of Model 2 with the interaction, the change for non-Hispanic white women; the course refit of that model gives −0.686 (SE 0.138), which `reference_marks.csv` holds.

## Columns

| Column | Meaning |
|:--|:--|
| `spec_id` | The row number, 1 to 144, in the order of `expand_grid()` over the six decisions |
| `cohorts`, `horizon`, `adjust`, `weights`, `time`, `model` | The options of the specification, as above. These are the arguments of `fit_spec()` and of `try_spec()` in the exercise |
| `b` | The coefficient of the later cohort in the marriage equation: the log of the ratio of the monthly chances of marrying (the odds for `logit`; the chance of marrying rather than staying together for `mlogit`; the rate for `cloglog`) of cohabitors who began their unions in the later period to that of those who began them in the earlier one, with the other terms held constant. Negative values mean that the later cohort married less, the direction of the claim |
| `se` | The design-based standard error of `b`: the linearisation (sandwich) estimator with the strata and clusters of both surveys, the clusters that hold no analysed union counting with zero scores, as the survey package treats a subset of a design |
| `stat_type` | `t` on every row. readr reads a column that holds only `t` as logical, so give its type when reading the file (`col_types = cols(stat_type = col_character())`) |
| `stat` | `b` divided by `se` |
| `df` | The degrees of freedom of the design-based test, as svyglm counts them: the clusters that hold analysed unions, minus their strata, minus the coefficients of the model, plus one (235 to 264) |
| `p` | The two-sided p-value of `stat` on `df`. This is the test that classes every row |
| `se_model`, `z_model`, `p_model` | The conventional error (the inverse of the information matrix, with the weights scaled to a mean of one), its z and p-value, which treat every person-month as independent |
| `r` | The partial correlation by the rule of Multi100 for a z statistic, tanh(\|z\| × sqrt(1 / (n − 3))) × sign(z), from `z_model` and the number of person-months, as Multi100 converted the result of the paper (z = −4.8786 on 54,291 person-months gives −0.0209) |
| `ratio` | exp(`b`): the ratio of the monthly chances of marrying, later to earlier cohort |
| `n` | The person-months analysed |
| `n_unions` | The unions analysed (2,071 for `paper`, 2,556 for `five_years`) |
| `n_marriages` | The months in which a marriage is recorded (511 to 684) |
| `n_psu`, `n_strata` | The clusters and strata that hold analysed unions |
| `se_woman`, `t_woman`, `df_woman`, `p_woman` | The same test with errors clustered by woman (the months of a union as one cluster, without the strata and clusters of the surveys; t on the number of unions minus one), kept as a check |
| `supports` | `b` < 0 |
| `sig` | `p` < 0.05 |
| `is_reference` | TRUE for the reference specification only |

## What the curve shows

All 144 specifications are negative and significant at the 5% level, and by a wide margin: `b` runs from −0.830 to −0.629 (ratios from 0.44 to 0.53), the design-based t from −9.12 to −6.52, and p never exceeds 0.000000001. Errors clustered by woman give t from −9.53 to −6.61, and the conventional errors z from −9.98 to −8.44. The partial correlations by the Multi100 rule run from −0.052 to −0.042.

Two decisions move the estimate. The wider window of the later cohort (`five_years`) brings in unions begun in 2001–2004 and makes the decline smaller by 0.095 on average (from 0.084 to 0.110), and leaving out the weights makes it larger by 0.049 (from 0.024 to 0.080). Adjusting for age, race-ethnicity and education makes it smaller by 0.033 on average; the other options move it by less than 0.015, as `menu_deviations.csv` records ([codebook](menu_deviations_codebook.md)). In the unions of 1990–1995, 45% had married and 38% had separated by the end of the third year; in those of 2005–2010, 29% and 46% (`display_shares.csv`, [codebook](displays_codebook.md)).
