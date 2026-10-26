# Codebook: the display files of the immigration-and-trust case

Six small files that the pages of the case use beside the curve. Five are written by `R/trust/06_displays.R` and one, `display_reproduction.csv`, by `R/trust/04_reference_marks.R`, all from the course extract `trust_ess.csv` ([codebook](trust_ess_codebook.md)). Coefficients are on the scale of the curve, points of distrust (0–10) per point of concern about immigration (0–10), and the reference specification is that of `spec_curve.csv` ([codebook](spec_curve_codebook.md)): distrust of parliament, the controls of the paper, no weights, every item answered, fixed effects for country-round and errors clustered by country, 0.175 on 70,326 respondents.

## `display_reproduction.csv`

The file holds the reproduction target of the case, the focal model of the SCORE reproduction from source data (osf.io/jp7tr), a three-level model of Table 3 of the paper of the original author (OA) fitted with lme4, and sets it beside the same model refitted on the course extract and beside the reference specification of the curve. Five rows: the published values (`SCORE_jp7tr`: 0.1780, SE 0.00428, t = 41.583, 69,522 respondents in 56 country-rounds); the refit without the Italian round 2, which the integrated round files that the reproduction used did not hold (`course_repro_59`: 0.1773, SE 0.00428, t = 41.40, the same 69,522 respondents); the refit on all 60 country-rounds (`course_repro_60`: 0.1753, t = 41.21, 70,326 respondents); the reference specification of the curve on the respondents of the reproduction (`course_reference_fe_59`: 0.1772, SE 0.0095, t = 18.67 on 15 df), which the browser lab can fit; and the reference specification on all respondents (`course_reference_fe`). The columns are `id`, `b`, `se`, `statistic_type`, `statistic`, `df` (Satterthwaite for the multilevel models; for the published row the value printed in the report), `n`, `country_rounds`, `countries`, `note`. The results of the multilevel models here and in the other display files are kept to six significant digits, the precision at which lme4 gives the same values from one run to the next.

The course refit without the Italian round 2 analyses exactly the respondents of the reproduction and lands 0.0007 below its coefficient, and the fixed-effects specification on the same respondents 0.0008 below it. A check against the analysis file of the reproduction, respondent by respondent, puts the whole gap down to the two vote controls: the reproduction counts voters of the Dutch liberal party (VVD, the "People's Party for Freedom and Democracy") in rounds 3 and 4 as far right, apparently taking it for the Party for Freedom, and it does not count as cabinet voters the voters of five Belgian and Finnish party codes whose parties sat in the cabinet (PRL-FDF in the Belgian round 1 and the Left Alliance in the Finnish round 1; Open VLD + Vivant, the cdH and part of the joint list of CD&V and N-VA in the Belgian round 4). The series of the country-level variables and the standardisation of income change the coefficient only in the fifth decimal.

## `display_options.csv`

The options off the curve that a linear model can fit, each changed alone from the reference specification, plus the random-intercept model of the paper without its country-level variables. One row per option: `axis` and `option` (the codes of `menu_options.csv`), `b`, `se` (clustered by country, or the model-based error for `random_intercepts`), `stat`, `df`, `p`, `t_conv` (the conventional t), `r` (the Multi100 partial correlation), `b_std`, `n`, `n_country_rounds`, `b_reference` (0.175) and `shift` (`b` − `b_reference`).

| Axis | Option | Coefficient | Shift | Respondents |
|:--|:--|--:|--:|--:|
| sample | all_respondents | 0.175 | −0.0005 | 75,179 |
| concern | economy | 0.121 | −0.055 | 71,396 |
| concern | culture | 0.119 | −0.056 | 71,692 |
| concern | place | 0.129 | −0.046 | 71,687 |
| controls | paper_gov | 0.152 | −0.024 | 69,771 |
| controls | paper_no_unhappy | 0.174 | −0.001 | 70,402 |
| weights | post | 0.177 | +0.001 | 70,326 |
| weights | post_population | 0.163 | −0.012 | 70,326 |
| fe | pooled | 0.184 | +0.008 | 70,326 |
| votes | nonvoters_missing | 0.173 | −0.002 | 58,038 |
| fe | random_intercepts | 0.175 | +0.0001 | 70,326 |

## `display_inference.csv`

The reference specification under five rules of inference: conventional errors (t = 41.2 on 70,253 df), errors clustered by country-round (t = 26.5 on 56 df), errors clustered by country, the rule of the curve (t = 18.0 on 15 df), a wild cluster bootstrap by country (Webb weights, 9,999 draws, null imposed; p below 1/10,000), and the random-intercept model (t = 41.2). The columns are `rule`, `b`, `se`, `stat`, `df`, `p`, `note`. Errors clustered by country are 2.3 times the conventional ones, and the result stays far beyond any threshold under every rule.

## `display_loo.csv`

The reference specification with each of the 16 countries left out in turn: `left_out`, `b`, `se`, `stat`, `df` (14), `p`, `n`, `n_country_rounds`, `b_reference`. The coefficient stays between 0.169 (without Norway) and 0.178 (without Portugal).

## `display_reliability.csv`

The reliability of the two scales of the case among the respondents of the paper who answered all three items, by country and pooled (`cntry` = `all`): `scale` (`concern` for the three immigration items, `trust` for the three trust items), `cntry`, `alpha` (Cronbach's alpha), `mean_r` (the mean correlation between the items), `n`. Pooled, alpha is 0.84 for concern and 0.82 for trust, and by country alpha for concern runs from 0.76 (the Netherlands) to 0.88 (the United Kingdom). The paper reports an average alpha of 0.83 for concern, with a minimum of 0.75 for the Netherlands, and correlations between the items of 0.58 to 0.65.

## `display_country_means.csv`

The design-weighted means of concern about immigration (the index, every item answered) and of distrust of parliament in each of the 60 country-rounds, among the respondents of the paper who answered both: `cntry`, `essround`, `concern`, `distrust_parl`, `n`. These are the between-country differences that fixed effects for country-round set aside. Across the 16 countries, the unweighted means of concern and of distrust of parliament correlate at 0.68 (the paper reports 0.70).
