# Codebook: `trust_macro.csv`

The country-round and country variables of the model of the original author (OA), one row for each of the 60 country-rounds of the course extract `trust_ess.csv` ([codebook](trust_ess_codebook.md)), to be joined to it by `cntry` and `essround`. Written by `R/trust/02_macro.R` from the web appendix of the paper and from public series of the World Bank and Eurostat, whose snapshots are committed in `data-raw/trust/` with their sources and licences.

These variables describe countries and rounds, not respondents. In a model with fixed effects for country-round, which is how every specification of the curve compares respondents, they are constant within each group and drop out, so the curve never uses them. They enter only the multilevel models that the course fits as checks: the model of the paper and of the SCORE reproductions, with random intercepts for country and country-round.

## Columns

| Column | Meaning |
|:--|:--|
| `cntry`, `essround` | Country (ISO two-letter code) and ESS round |
| `fieldwork_start` | The year in which fieldwork started, as the appendix of the paper gives it; for Ireland in round 4, which the appendix lists as "N/A", 2009, the year of its first interviews |
| `reference_year` | `fieldwork_start` − 1: the paper measures social protection, GDP and unemployment "in the year before" each round, and the course applies the same rule to the governance index |
| `from_appendix` | `TRUE` for the 59 country-rounds of the appendix of the paper, `FALSE` for Ireland in round 4 |
| `far_right_share` | Per cent of the vote for far-right (anti-immigration) parties at the general election before fieldwork, from the appendix of the paper; 0 where it lists none, and 0 for Ireland in round 4, since Ireland had no far-right party in any round of the appendix |
| `social_protection` | Total expenditure on social protection, euro per inhabitant at current prices, in the reference year (Eurostat, ESSPROS) |
| `wgi` | The mean of the six Worldwide Governance Indicators (voice and accountability, political stability, government effectiveness, regulatory quality, rule of law, control of corruption; about −2.5 to 2.5) in the reference year; 2001 has no values, so the country-rounds whose reference year is 2001 take the values of 2000, as the SCORE source-data reproduction did |
| `gdp_index` | GDP per head at purchasing power parity, as an index on which the average of the OECD members is 100, in the reference year (World Bank, current international dollars) |
| `unemployment` | Unemployment as a percentage of the labour force, modelled ILO estimate, in the reference year (World Bank) |
| `long_term_imm` | 1 for the countries with a long post-war history of mass immigration, 0 for Greece, Spain, Portugal, Italy and Ireland, as the paper codes them (its note 27) |

## Differences from the paper

The paper takes GDP per head and unemployment from the OECD and does not name the edition or the base of every series; the course takes both from the World Bank, which covers the 16 countries in every year. The paper treats governance, GDP and unemployment as country-level (level 3) variables that do not vary across rounds, yet measures them in the year before each round; the course, like the SCORE reproductions, gives them one value per country-round. None of this touches the coefficient of concern about immigration, which is estimated from differences between respondents of the same country-round: swapping these series for those of the SCORE reproduction changes it in the fifth decimal.
