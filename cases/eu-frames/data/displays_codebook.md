---
aliases:
  - /data/displays_codebook.html
---

# Codebook: the displays beside the curve

The curve of the claim (`spec_curve.csv`) keeps only the specifications that answer the claim as the course reads it, change within countries over time in all 27 member states of 2004–2013. Several findings of the wider menu stay in the course as displays of their own, because each answers a related but different question. The six files below hold the numbers behind those displays. They are written by `R/eu-frames/12_displays.R` from the course panel and the aggregate measurement panels, and they contain only results computed on country-year aggregates.

Unless a column says otherwise, every fit uses the standard measurement settings: each scale scored as the share of a respondent's mentions, all 13 items, respondents who mention nothing scored 0, cells as interviewed, and the World Bank series of unemployment and growth. Every linear fit carries the inference columns of the curve, which the curve codebook defines: `b`, `se`, `t` and `df` of the conventional fit, the descriptive partial correlation `r`, its claim-aligned version `r_ca`, the conventional p-value `p_conv`, the clustered t on G − 1 df (`t_cr1`, `p_cr1`), CR2 with Satterthwaite df (`t_cr2`, `df_cr2`, `p_cr2`), the wild cluster bootstrap by country (`p_wcr`, Rademacher signs; `p_wcr_webb`, Webb signs), and the counts `n_cells` and `n_countries`. A bootstrap p-value of 0 means that none of the 9,999 replicates reached the t of the data, so p < 0.0001.

## `display_between_within.csv` – within and between countries

Six rows: the three positive scales (`mcosmo`, `mutil`, `mpos`), each with raw unemployment and with growth as the predictor, no adjustment. The display shows that the association within countries and the association between countries are different quantities, and that on these data they run in opposite directions for unemployment.

| Column | Meaning |
|:--|:--|
| `outcome`, `predictor`, `measure` | The scale, the predictor and its series |
| `b_within`, `se_within`, `r_within` | The slope within countries (country fixed effects), its conventional standard error and its partial correlation |
| `p_within_wcr`, `p_within_cr2` | Its bootstrap and CR2 p-values |
| `b_between`, `se_between`, `t_between`, `df_between`, `p_between` | The slope of the 27 country means of the scale on the 27 country means of the predictor, by least squares on 25 df |
| `r_between` | The correlation of the 27 pairs of means |
| `b_rewb_within`, `t_rewb_within`, `df_rewb_within`, `p_rewb_within` | The within slope of the within–between model of Bell and Jones (2015): the deviation of the predictor from its country mean, beside the country mean, with a random intercept for country; Satterthwaite df (`lmerTest`). It equals `b_within` |
| `b_rewb_between`, `t_rewb_between`, `df_rewb_between`, `p_rewb_between` | The between slope of the same model, the coefficient on the country mean |
| `b_difference`, `t_difference`, `df_difference`, `p_difference` | The within slope minus the between slope, with its Satterthwaite test: the test of the assumption of a random-effects model that stable country differences are unrelated to the predictor |
| `n_cells`, `n_countries` | 270 and 27 |

For the cosmopolitan scale the slope within countries is −0.00565 per point of unemployment, and the slope across the country means is +0.00414 (p = 0.31 on 25 df); their difference has p = 0.024. For growth the within slope of the cosmopolitan scale is +0.00068 (bootstrap p = 0.66) and the between slope +0.0206 (p = 0.009).

## `display_country_means.csv` – the 27 country means

One row per member state, for a scatter of country means.

| Column | Meaning |
|:--|:--|
| `cntry`, `country` | The ISO code and the name, as in `EUframes_cy.csv` |
| `n_years` | Years in the panel, 10 for every country |
| `bailout` | 1 for the eight countries that the panel flags as recipients of financial assistance in any year |
| `unemp_mean`, `growth_mean` | The mean of the World Bank unemployment rate (per cent) and of GDP growth (per cent) over 2004–2013 |
| `mcosmo_mean` … `mneg_mean` | The mean of each of the six scales over 2004–2013 |

Mean unemployment runs from 4.9% to 15.8% across the countries, and its correlation with the mean cosmopolitan share is +0.20.

## `display_periods.csv` – before and during the crisis

108 rows: the 36 core specifications (the three positive scales; raw or logged unemployment, or growth; with or without the other measure; two-way or country fixed effects) on the full panel (`sample` = `all`, 270 cells), on 2004–2008 (`y2004_2008`, 135 cells) and on 2009–2013 (`y2009_2013`, 135 cells). The columns are the thirteen axis codes followed by the inference columns, `supports` and `sig`, as in the curve.

The restriction to a period changes the population that the claim is about: the earlier window contains no crisis, and the later one drops the comparison with the years before it. For the reproduced specification (cosmopolitan scale, raw unemployment, two-way fixed effects) the slope is −0.00347 on the full panel (bootstrap p = 0.110), −0.00686 on 2004–2008 (p = 0.0011) and −0.00596 on 2009–2013 (p = 0.018). Across the 24 unemployment specifications, 79.2% are below zero with bootstrap p < 0.05 on the full panel, 66.7% on 2004–2008 and 95.8% on 2009–2013.

## `display_loo.csv` – one country left out at a time

56 rows: the reproduced specification (`estimator` = `fe_twoway`) and its country fixed-effects twin (`fe_country`), each fitted on all 27 countries (`left_out` = `none`) and without each country in turn.

| Column | Meaning |
|:--|:--|
| `estimator`, `left_out` | The design, and the country left out (`none` for the full panel) |
| `outcome`, `predictor` | `mcosmo` and `unemp` |
| `b` … `p_wcr_webb`, `n_cells`, `n_countries` | The fit without that country |
| `variation_share` | The percentage of the variation in unemployment that is left after the fixed effects of the design and that the country holds, computed on the full panel. Empty for the full-panel row |

In the two-way design Greece holds 17.5% of that variation, Spain 15.9%, Poland 10.6% and Ireland 7.5%. Leaving one country out moves the two-way slope between −0.00415 (without Poland) and −0.00246 (without Greece), and none of the 27 fits reaches bootstrap p < 0.05. In the country design every one of the 27 does.

## `display_bailout.csv` – assisted countries and the rest

36 rows: the three positive scales, raw unemployment or growth, two-way or country fixed effects, each in three groups.

| Column | Meaning |
|:--|:--|
| `outcome`, `predictor`, `estimator` | The specification |
| `group` | `bailout`, the eight countries that the panel flags as recipients of financial assistance (Cyprus, Greece, Hungary, Ireland, Latvia, Portugal, Romania, Spain), fitted on their own; `other`, the remaining nineteen, fitted on their own; `difference`, the slope of the bailout countries minus that of the others, estimated in one model on all 270 cells with an interaction and fixed effects shared by all countries |
| `b` … `n_countries` | The inference columns of the curve, without the CR1 pair |

With eight clusters the Rademacher bootstrap can draw only 256 distinct sign patterns, so its p-values for the `bailout` group are coarse; read `p_wcr_webb` there. For the reproduced specification the slope is −0.00504 in the bailout countries (Webb p = 0.042) and +0.00194 in the others (bootstrap p = 0.33); the difference of −0.00674 has bootstrap p = 0.040.

## `display_inference.csv` – one estimate, five tests

Ten rows: the reproduced specification and its country fixed-effects twin, each under five kinds of inference.

| Column | Meaning |
|:--|:--|
| `estimator` | `fe_twoway` or `fe_country` |
| `inference` | `conventional`; `cr1_g1`, errors clustered by country with t on G − 1 = 26 df; `cr2_satterthwaite`, the CR2 correction with Satterthwaite df; `wcr_rademacher` and `wcr_webb`, the wild cluster bootstrap by country |
| `b` | The slope, the same in every row of a design |
| `se`, `t`, `df` | The standard error, t and df of the rule; empty for the bootstrap, which returns a p-value directly |
| `p` | The p-value of the rule |
| `r_descriptive` | The descriptive partial correlation, the same in every row of a design |
| `r_formula` | What the conversion r = t / sqrt(t^2 + df) returns from the t and df of the rule |

For the reproduced specification the p-value runs from 0.00018 (conventional) through 0.029 (CR1) and 0.050 (CR2, on 9.8 df) to 0.110 (bootstrap, Rademacher) and 0.096 (Webb). The partial correlation of the data stays −0.242 throughout, but the conversion formula returns −0.413 from the clustered t on 26 df and −0.581 from the CR2 t on 9.8 df: the weaker the evidence, the larger the converted number, which is why the curve plots the descriptive r and tests it separately.
