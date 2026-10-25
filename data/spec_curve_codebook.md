# Codebook: the curve of the claim

Four files describe the specification curve of the claim under test, "poor national economic performances significantly decrease positive EU-framing dimensions" (Teney 2016, *European Sociological Review*), read as a claim about change within countries over time: in the 27 member states of 2004–2013, when the economy of a country deteriorates, do its citizens frame the EU less in positive terms than they did before? The files are built by `R/09_spec_curve.R`, `R/10_curve_forks.R` and `R/11_menu_deviations.R` from the course panel and from the aggregate measurement panels in this folder. They contain only results computed on country-year aggregates.

| File | Rows | What it holds |
|:--|--:|:--|
| `spec_curve.csv` | 864 | The curve of the claim: every specification of the three positive scales that the menu keeps |
| `spec_curve_negative.csv` | 864 | The same specifications of the three negative scales, the other half of the hypothesis |
| `spec_curve_forks.csv` | 20 | How much each decision on each curve moves the result |
| `menu_deviations.csv` | 100 | What each option of the full menu, kept or not, does to the curve, summarised by option |
| `menu_deviations_rows.csv.gz` | 53,280 | The same comparison, one row for each specification and its twin and each rule |

## The specifications

Each curve crosses, for each of its three scales, the options that the audit of the menu keeps: unemployment in four published series that cover all 270 country-years (`wb_nat`, `es_1574_act`, `es_2064_pop`, `es_2564_lfs`) and in two forms (`raw`, `log`), or GDP growth in the World Bank series (`wb_growth`, raw only, since it takes negative values); without or with the other economic measure as an adjustment (`copredictor` = `none` or `other`, the other measure always in its World Bank series); country and year fixed effects (`fe_twoway`) or country fixed effects alone (`fe_country`); respondents who mention none of the 13 items scored 0 (`zero`) or left out (`drop`); all 13 items (`all`) or the items without "unemployment" (`un`); and cells as interviewed (`unweighted`) or weighted by the GESIS weight (`design`). That gives 256 unemployment and 32 growth specifications for each scale, 288 per scale and 864 per curve. The remaining axes are fixed on both curves: all 270 country-years (`sample` = `all`), no regression weight (`weights` = `none`), the mentions of a respondent in the scale divided by the total mentions of that respondent (`construction` = `share`), and a linear model (`family` = `gaussian`).

The two fixed-effects designs are two estimands rather than two ways of estimating one. With year effects the comparison is change within a country relative to the common European path in that year; without them it is all change within a country, including the changes that every member state went through at once.

The reference specification is the reproduced one: `mcosmo` on raw unemployment in the World Bank series, no adjustment, two-way fixed effects, the zero rule, all items and cells as interviewed. It gives b = −0.00347, t = −3.804 on 233 df and r = −0.242, the value that the reproduction task of the course targets.

## Columns of the two curve files

| Column | Meaning |
|:--|:--|
| `outcome` … `family` | The thirteen axis codes that the exercise helpers use, in their order: `outcome`, `predictor`, `copredictor`, `estimator`, `sample`, `weights`, `construction`, `noop`, `item_set`, `cell_weight`, `measure`, `pred_form`, `family`. The fixed axes are included, so that a specification can be looked up with no defaults |
| `b`, `se` | The slope of the scale (a share from 0 to 1) on the predictor, and its conventional standard error. For the logged form the slope is per unit of log unemployment |
| `t`, `df` | The conventional t and the residual degrees of freedom of the fit, as `fixest` reports them (233 for the reference specification). These are the two numbers that a class submission carries |
| `r` | The descriptive partial correlation, r = t / sqrt(t^2 + df) on the conventional t and residual df. It equals the correlation of the outcome and the predictor once the fixed effects and the adjustment have been removed from both, so it describes the association whatever test is applied to it |
| `r_ca` | `r` aligned with the claim: `r` on the positive curve and −`r` on the negative curve, each multiplied by −1 again when growth is the predictor, because the claim expects framing to rise with growth. `r_ca` < 0 is the direction of the claim on both curves |
| `p_conv` | The p-value from the conventional t on the residual df, which treats the 270 country-years as independent |
| `t_cr1`, `p_cr1` | The t with standard errors clustered by country and the default small-sample adjustment of `fixest` (CR1), and its p-value on G − 1 = 26 df |
| `t_cr2`, `df_cr2`, `p_cr2` | The t with the CR2 correction of Bell and McCaffrey, its Satterthwaite degrees of freedom and its p-value (`clubSandwich`). The df say how many countries effectively carry the information, which is far fewer than 27 when a few countries hold most of the variation |
| `p_wcr` | The p-value of the restricted wild cluster bootstrap by country with Rademacher signs: the null of no association is imposed, the residuals of that null model are flipped in sign country by country 9,999 times, and the clustered t of the data is set against the clustered t of each replicate. This is the rule that classes every point |
| `p_wcr_webb` | The same bootstrap with the six-point signs of Webb, kept as a check |
| `supports` | `r_ca` < 0 |
| `sig` | `p_wcr` < 0.05 |
| `n_cells`, `n_countries` | The country-years and countries in the fit: 270 and 27 everywhere on the two curves |
| `is_reference` | TRUE for the reproduced specification only; FALSE throughout the negative curve, which has no reference |

The sign draws of the bootstrap are made once, from a fixed seed, as a matrix with one row per member state, and the same matrix serves every specification. The helper `R/_wild_bootstrap.R` documents the seed and the generator, so that a copy of it fitted on the same cells returns the stored p-value exactly. Two consequences follow from the 9,999 draws. A stored p-value carries Monte Carlo error of about 0.003 near p = 0.1 and about 0.002 near p = 0.05; and since one matrix serves all specifications, the errors of neighbouring specifications are alike rather than independent. At the reference cell the stored value is 0.110, where a run of 199,999 draws of the same procedure gives 0.105 and the `fwildclusterboot` package with 99,999 draws of its own gives 0.106.

## What the curves show

| Curve | Rows | Below zero (%) | Conventional (%) | CR1, G − 1 (%) | CR2 (%) | Bootstrap (%) | Bootstrap, Webb (%) | Median `r_ca` |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Positive scales | 864 | 98.1 | 90.3 | 80.1 | 77.2 | 70.9 | 73.0 | −0.314 |
| Negative scales | 864 | 95.5 | 80.2 | 72.2 | 68.1 | 72.2 | 72.7 | −0.326 |

The percentage columns after the first give the share of rows below zero with p < 0.05 under each kind of inference. Every row has the same partial correlation in every column, so only the class moves.

On the positive curve the estimator divides the result. Below zero with bootstrap p < 0.05 are 92.1% of the country fixed-effects rows and 49.8% of the two-way rows; with unemployment as the predictor the shares are 99.5% and 51.8%, with growth 33.3% under both designs. The other axes move the share by a few points: 74.8% under `drop` against 67.1% under `zero`, 77.8% with all items against 64.1% without "unemployment", 71.8% with weighted cells against 70.1% as interviewed. By scale the shares are 55.9% for `mcosmo`, 77.8% for `mutil` and 79.2% for `mpos`.

The negative curve is not independent of the positive one. Under the share rule the four scales of a respondent sum to one, and under the zero rule the cell means satisfy `mpos` + `mneg` = 1 − (share of respondents with no mention), so the aligned negative composite repeats much of the positive result. Its communitarian scale contains the item "unemployment", which ties the outcome to the predictor by content; that is why the item set moves the negative curve more than the positive one.

## `spec_curve_forks.csv`

One row per curve and axis that varies on it, plus a row `interactions` per curve.

| Column | Meaning |
|:--|:--|
| `curve` | `positive` or `negative` |
| `axis` | The axis code, or `interactions` |
| `share` | The share of the variance of `r_ca` over the 864 rows that the main effect of the axis carries, in per cent: non-partial eta-squared from one sequential fit of all main effects. The predictor enters before the series and the form, which exist only for unemployment; the other axes are balanced against one another, so their shares do not depend on the order. The `interactions` row is the remainder, the part of the variance that only combinations of decisions explain |
| `n_options` | The options of the axis that the pairs compare |
| `n_pairs` | The pairs of specifications that differ only on the axis. Axes with three or more options contribute every pair of their options. The series and the form pair only within unemployment. The predictor pairs raw unemployment in the World Bank series with growth in the World Bank series, the two specifications that differ in nothing but the measure of the economy |
| `median_abs_shift` | The median absolute difference in `r_ca` within a pair |
| `sign_changes` | The percentage of pairs whose two members lie on different sides of zero |
| `crossing_rate` | The percentage of pairs whose two members fall in different classes, the class being "below zero with bootstrap p < 0.05" or not |
| `crossing_rate_cr2`, `crossing_rate_conv` | The same under CR2 and under conventional inference |

On the positive curve the estimator carries 41.5% of the variance and the predictor 16.9%, the outcome 9.0%, the adjustment 4.3% and the item set 3.1%; the no-opinion rule, the series, the form and the cell weighting together carry about 1%, and the interactions 24.1%. Moving the estimator alone changes the class of 42.4% of pairs, moving the predictor 56.3%, the outcome 31.9% and the adjustment 19.7%; the series changes 3.7%, the form 9.1%, the no-opinion rule 8.6%, the item set 13.7% and the cell weighting 2.1%.

## `menu_deviations.csv` and `menu_deviations_rows.csv.gz`

These two files hold the data for a map of where each decision of the full menu is made and what each option does, including the options that the audit moved off the curve. Each option is swapped into every curve specification to which it applies, the changed specification is refitted, and the result is set beside its twin on the curve. The options kept on the curve are compared in the same way, but their pairs lie within the curve and need no refit.

The options compared off the curve are the count of the items of the scale in place of the share (the rate, which divides the count by the number of items, leaves t and r of a linear fit unchanged, and is recorded as identical rather than refitted); the four restricted samples (`y2004_2008`, `y2009_2013`, `no_bailout`, `no_GR_ES`); the three regression weights (`n_cell`, the respondents in the cell; `n_eff`, the effective sample size of the cell; `pop`, the adult population); pooled OLS without fixed effects (`pooled_cl`), random effects (`re`) and the within–between model of Bell and Jones (2015) (`rewb`); the `split` rule and the two imputation engines for respondents who mention nothing (`mi_mice`, `mi_amelia`, available only at the standard measurement settings of the unemployment arm: raw unemployment in the World Bank series, all items, cells as interviewed); the two series that change the sample (`es_1574_tips`, which lacks the United Kingdom and nearly every year before 2009, and `es_growth`, which lacks the United Kingdom); the beta family (`beta`); and the inference rules that the curve does not use (`se` = `conventional`, `cr1_g1`, `cr2`, `wcr_webb`), for which nothing is refitted and only the class is read under another rule.

Each option is tested under the rule that fits it, and the rule is recorded in `rule`:

| `rule` | Used for |
|:--|:--|
| `wcr_rademacher` | Every linear model fitted by least squares, pooled OLS included, and every kept option: the bootstrap of the curve |
| `re_conventional`, `re_cr1_g1` | Random effects: its model-based errors on the residual df, and errors clustered by country (Arellano) with t on G − 1 df |
| `rewb_satterthwaite`, `rewb_cr2` | The within–between model: the within slope with Satterthwaite df, and with CR2 errors and Satterthwaite df. Its year effects follow the twin, so that its within slope equals the two-way slope where the twin has year effects and the country slope where it has not |
| `beta_cr1_g1` | The beta family: errors clustered by country (CR1) with t on G − 1 df, since the bootstrap of the curve is written for linear models |
| `mi_rubin_cr1_g1` | The imputed variants: pooled over ten imputations by Rubin's rules, errors clustered by country, t on 26 df |
| `conventional`, `cr1_g1`, `cr2`, `wcr_webb` | The inference rules themselves, read on the curve without a refit |
| `identical_to_count` | The rate, whose numbers are those of the count |

The partial correlation of a deviation is always the descriptive one of its own conventional fit; for the within–between model it is converted on its Satterthwaite df and for the beta family from its z on the residual df.

On the positive curve, where 70.9% of the specifications lie below zero with bootstrap p < 0.05, the same share is 47.6% when the scale is a count, 37.6% without the eight bailout countries, 54.4% without Greece and Spain, 48.0% under the population weight and 7.4% under pooled OLS. The options that answer the same question in another way stay close to the curve: 75.0% under the split rule, 80.7% for the beta family under its clustered test, and 91.7% for both imputation engines on the 24 specifications that they cover. Every fit succeeded (`n_failed` is 0 throughout).

**`menu_deviations.csv`** has one row per curve, axis, option and rule.

| Column | Meaning |
|:--|:--|
| `curve`, `axis`, `option`, `rule` | What is compared, and under which test |
| `level` | Where the decision sits on the menu: `estimand` (outcome, predictor, copredictor, estimator, sample, weights, construction), `measurement` (noop, item_set, cell_weight, measure), `estimation` (pred_form, family) or `inference` (se) |
| `on_curve` | TRUE for an option that the curve keeps |
| `n_pairs` | The pairs of a specification on the curve and its twin with the option |
| `below_zero` | The percentage of pairs whose deviation has `r_ca` < 0 |
| `below_zero_sig` | The percentage whose deviation has `r_ca` < 0 and p < 0.05 under its rule |
| `median_r_ca` | The median `r_ca` of the deviations |
| `median_shift` | The median of `r_ca` of the deviation minus `r_ca` of its twin |
| `sign_changes` | The percentage of pairs whose two members lie on different sides of zero |
| `class_changes` | The percentage of pairs whose two members fall in different classes, each classed under its own rule |
| `n_failed` | Pairs whose deviation could not be fitted |

Options that the curve fixes (`sample` = `all`, `weights` = `none`, `construction` = `share`, `family` = `gaussian`) and the single growth series (`wb_growth`) have no pairs within the curve. Their rows carry `n_pairs` = 0, shares that describe the curve specifications carrying the option, and empty pair columns.

**`menu_deviations_rows.csv.gz`** has one row per pair and rule: the columns `curve`, `axis`, `option`, `rule`, `level` and `on_curve`; the thirteen axis codes of the twin on the curve; `r_ca_twin`, `p_twin` (its bootstrap p) and `p_twin_cr1` (its CR1 p, for a comparison of like with like where the deviation is tested by CR1); and for the deviation `r_ca_dev`, `p_dev`, `b_dev`, `t_dev`, `df_dev`, `n_cells_dev` and `n_countries_dev`. For the inference rules `t_dev` and `df_dev` are those of the rule (empty for the bootstrap), and for the beta family and the imputed variants they are those of the clustered test.
