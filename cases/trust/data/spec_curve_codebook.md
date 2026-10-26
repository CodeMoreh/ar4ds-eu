# Codebook: `spec_curve.csv`

The curve of the claim of the immigration-and-trust case, "... concerns about the effect of immigration on the national community have a(n) [reducing] impact on trust in politics", in the wording that SCORE took from the abstract of the paper of the original author (OA): every specification that the specification menu of the course keeps, 192 rows, fitted once on the course extract `trust_ess.csv` ([codebook](trust_ess_codebook.md)). The file is written by `R/trust/03_spec_curve.R` with the estimator of `R/trust/_tools.R` (`fit_spec()`), which uses base R, dplyr and purrr only, so that the workspace and the browser lab run the identical code; `R/trust/check_parity.R` checks it against fixest and sandwich on every row.

## The specifications

Five decisions vary on the curve, and every combination of their options is one row: 4 × 4 × 3 × 2 × 2 = 192.

| Column | Options | Meaning |
|:--|:--|:--|
| `outcome` | `parliament`, `politicians`, `legal`, `index` | Distrust of the national parliament, of politicians or of the legal system (each the trust item reversed, 0–10), or the mean of the three |
| `controls` | `none`, `demog`, `outlook`, `paper` | No controls; age, sex, education and household income; those with unhappiness, dissatisfaction with life, rarely meeting friends, interpersonal distrust and left–right position; and those with dissatisfaction with the economy, with household income and with health and education services, a vote for a cabinet party and a vote for a far-right party, the individual controls of Table 3 of the paper |
| `weights` | `none`, `design`, `population` | No weights; the design weight (`dweight`); the design weight times the population weight (`dweight` × `pweight`) |
| `items` | `complete`, `available` | A scale (concern, interpersonal distrust, the mean of the trust items) counts only when every item was answered, or is the mean of the answered items |
| `fe` | `country_round`, `country_and_round` | An intercept for every country-round, or an intercept for every country and a shift for every round |

The other decisions of the menu take one option on every row: the respondents are the citizens of the 16 countries who do not belong to an ethnic minority; concern about immigration is the mean of the three immigration items; education enters in its five ISCED levels; the vote controls follow the definitions of the paper; respondents with a missing answer on any variable of the specification are left out; the errors are clustered by country; and the estimate is reported in points of distrust per point of concern. `menu_options.csv` lists every option ([codebook](menu_options_codebook.md)).

Each specification has its own analysed rows, the respondents who answered every variable that it uses. Household income is missing for every respondent in France and Ireland in round 1, and the feeling about household income for every respondent in France in rounds 1 and 2, so the specifications with controls keep 58 country-rounds (`demog`, `outlook`) or 57 (`paper`), and those without controls all 60.

The reference specification (`is_reference`) is `parliament`, `paper`, `none`, `complete`, `country_round`: the outcome that SCORE chose, the controls of the paper, no weights and the complete-answer rule of the SCORE reproduction from source data, with fixed effects in place of the random intercepts of the paper. It gives 0.175 points of distrust per point of concern (SE 0.0097, t = 18.04 on 15 df, n = 70,326 in 57 country-rounds), against 0.17 in Table 3 of the paper.

## Columns

| Column | Meaning |
|:--|:--|
| `spec_id` | The row number, 1 to 192, in the order of `expand_grid()` over the five decisions |
| `outcome`, `controls`, `weights`, `items`, `fe` | The options of the specification, as above. These are the arguments of `fit_spec()` and of `try_spec()` in the exercise |
| `b` | The coefficient of concern about immigration: the change in distrust (0–10) that goes with one more point of concern (0–10), with the other controls held constant and comparisons made within the groups of `fe`. Positive values mean that more concern goes with more distrust, the direction of the claim |
| `se` | The standard error of `b`, clustered by country (CR1: the sandwich estimator with the small-sample factor G / (G − 1) × (n − 1) / (n − K), K counting the slopes and one intercept, as fixest does by default) |
| `stat_type` | `t` on every row. readr reads a column that holds only `t` as logical, so give its type when reading the file (`col_types = cols(stat_type = col_character())`) |
| `stat` | `b` divided by `se` |
| `df` | The degrees of freedom of the clustered test: the number of countries minus one, 15 |
| `p` | The two-sided p-value of `stat` on `df`. This is the test that classes every row |
| `se_conv`, `t_conv`, `df_conv`, `p_conv` | The conventional (model-based) error, its t, its degrees of freedom (respondents minus slopes minus absorbed groups) and its p-value, which treat respondents as independent |
| `r` | The partial correlation by the rule of Multi100, t / sqrt(t^2 + df), from the conventional t and its degrees of freedom; for a linear model this is the partial correlation of concern and distrust given the controls and the fixed effects (weighted when the specification is weighted) |
| `b_std` | The coefficient in standard deviations: `b` × SD(concern) / SD(distrust), over the analysed rows, unweighted |
| `n` | The respondents analysed |
| `n_clusters` | The countries, 16 on every row |
| `n_country_rounds` | The country-rounds that the analysed respondents come from |
| `se_cr`, `t_cr`, `df_cr`, `p_cr` | The same test with errors clustered by country-round (CR1, t on the number of country-rounds minus one), kept as a check |
| `supports` | `b` > 0 |
| `sig` | `p` < 0.05 |
| `is_reference` | TRUE for the reference specification only |

## What the curve shows

All 192 specifications are positive and significant at the 5% level under errors clustered by country, and by a wide margin: `b` runs from 0.117 to 0.326 points of distrust per point of concern, the clustered t from 11.3 to 21.6 on 15 df, and p never exceeds 0.00000001. The country-round clustering gives t from 14.1 to 36.6, and the conventional errors t from 30 to 103. The partial correlations run from 0.111 to 0.305, and the analysed respondents from 69,886 to 109,408.

The adjustment set divides the curve. Without controls the coefficient runs from 0.253 to 0.326 (mean 0.290), with the demographics from 0.259 to 0.315 (0.286), with outlook and ideology from 0.183 to 0.249 (0.214), and with the full controls of the paper from 0.117 to 0.179 (0.151). The outcome moves it by up to 0.055 (politicians lowest, parliament highest), and the weights, the rule for unanswered items and the choice of fixed effects each by less than 0.02, as `menu_deviations.csv` records ([codebook](menu_deviations_codebook.md)).
