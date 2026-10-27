# Codebook: `analyst_KQXUE.csv` and `analyst_018OL.csv`

The two files hold the aggregate data on which the Task 2 results of two of the five Multi100 analysts reproduce exactly: analyst KQXUE (t = −7.169 on 242 df) and analyst 018OL (t = −1.206 on 396.9 df). With the course panel `EUframes_cy.csv`, on which analyst C6HJR reproduces (t = −3.804 on 233 df), they make the three reproductions that Task 3 of the browser lab on the course site and of the downloadable workspace offers. The other two analysts, KEVF1 and PRL47, fitted their models on several hundred thousand respondents, so their results cannot be reproduced from aggregate data; the three-day course builds the respondent file from the GESIS downloads of the participants and reproduces them there.

Both files are written by `R/eu-frames/15_analyst_panels.R` from the sixteen Eurobarometer waves held by GESIS, following the code that each analyst deposited on the OSF (KQXUE: `task2.do`, osf.io/vycgd; 018OL: `multi100_Teney2016_task-2.Rmd`, osf.io/nz7ka) step by step, including the steps that the case study counts as slips. The stage stops unless both recorded statistics come back to the third decimal. They are aggregates, means over 444 or more respondents per cell, which the GESIS terms allow to be published; the respondent files from which they are built are never shared.

## The unemployment series

Both analysts merged a slice of the Eurostat table `une_rt_a_h`, which holds the unemployment rate by age band, sex and unit. KQXUE took the unemployed as a percentage of the whole population aged 20–64 (`Y20-64`, `PC_POP`, total), which is not the unemployment rate as ordinarily understood: across the panel it averages 6.3 per cent, against 8.7 for the rate in the labour force. 018OL took the unemployed as a percentage of the active population aged 15–74 (`Y15-74`, `PC_ACT`, total). Both slices are read from the table as Eurostat published it in December 2023 (`data-raw/eu-frames/eurostat/une_rt_a_h_2023-12-22.tsv.gz`, a copy of the bulk file kept by the Internet Archive on 29 December 2023). Eurostat later revised one value that matters here, Portugal in 2013, from 12.9 to 13.0 on the slice of KQXUE; with the later value the t of KQXUE comes out at −7.172 rather than −7.169. Every other value of the two slices is the same in both vintages.

## `analyst_KQXUE.csv`

One row per country-year: 27 member states, 2004–2013, 270 rows.

| Column | Meaning |
|:--|:--|
| `cntry` | Two-letter ISO country code; the samples of East and West Germany, of Great Britain and Northern Ireland, and of the Republic of Cyprus and the Turkish Cypriot community are each pooled into one country, as `task2.do` pools them |
| `year` | The calendar year of the fieldwork of each wave, as `task2.do` dates it |
| `cosmo` | The mean cosmopolitan share: for each respondent, the number of the five cosmopolitan items mentioned (peace; democracy; freedom to travel, study and work; cultural diversity; a stronger say in the world) divided by the number of the thirteen items mentioned (the euro left out), averaged over the respondents of the country-year without weights. A respondent who mentions none of the thirteen has no share and is left out, as in Stata, where a division by zero gives a missing value. EB 62.0 enters the 2004 means twice, as in the deposited code |
| `n` | The number of respondent rows behind `cosmo`, with the rows of EB 62.0 counted twice |
| `cosmo_once`, `n_once` | The same mean and count with EB 62.0 counted once, for the comparison that shows what the double append does |
| `unemp` | Unemployment as a percentage of the population aged 20–64, Eurostat `une_rt_a_h`, December 2023 vintage |

The recorded result comes from a linear model with a fixed effect for each country and conventional standard errors, which is what the command `xtreg cosmo unemployment, fe` of `task2.do` fits: b = −0.00736, SE = 0.00103, t = −7.169 on 242 degrees of freedom (270 country-years less 27 country effects and the slope). In R, `fixest::feols(cosmo ~ unemp | cntry, data = kqxue, vcov = "iid")` gives the same numbers, and so does `stats::lm(cosmo ~ unemp + factor(cntry))`. The model has no year effects, so the comparison within each country keeps the changes that were common to all of Europe in a year.

The double append moves the result very little: with `cosmo_once` the t is −7.166. It changes the 2004 means of the fifteen countries that EB 61.0 surveyed, where EB 62.0 then carries two thirds of the respondents instead of half; for the twelve newer member states EB 62.0 is the only wave of 2004, and a duplicate leaves their mean unchanged. The total of `n` is 423,328, the number of respondents in the descriptive table of the write-up of KQXUE.

## `analyst_018OL.csv`

One row per country and wave: 27 member states in fifteen waves and the fifteen older member states in EB 61.0, 420 rows.

| Column | Meaning |
|:--|:--|
| `cntry` | Two-letter ISO country code; East and West Germany are pooled, and so is Great Britain under `GB`. Northern Ireland and the Turkish Cypriot community are not pooled into their countries by the deposited code, so they fall out at its filter on the 27 member states |
| `wave` | The Eurobarometer wave, for example `EB 61.0` |
| `year` | The year that 018OL gave the wave, read from the first four characters of the file names of the analyst. It puts EB 79.3, fielded in the spring of 2013, in 2012, so the data have nine years, 2004–2012, and the cells of EB 79.3 take the unemployment of 2012 |
| `year_fieldwork` | The year of the fieldwork, which differs from `year` only for EB 79.3 (2013) |
| `pos` | The mean of the five cosmopolitan items per respondent (each item 1 if mentioned and 0 if not, so a respondent who mentions all five scores 1 and one who mentions none scores 0), averaged over the respondents of the cell without weights. The items are those whose labels in the GESIS files begin with "EU MEANING:" and the words peace, democracy, travel, cultural diversity and stronger say, which is how the deposited code finds them |
| `n` | The number of respondents in the cell |
| `unemp` | The unemployment rate of those aged 15–74 in the active population, Eurostat `une_rt_a_h`, December 2023 vintage, merged on `cntry` and `year` |

The recorded result comes from a linear mixed model with random intercepts for country, year and wave, fitted by `lme4::lmer()`, with the degrees of freedom of the Satterthwaite approximation that `lmerTest` reports: `lmerTest::lmer(pos ~ unemp + (1 | cntry) + (1 | year) + (1 | wave), data = o18ol)` gives b = −0.00072, SE = 0.00060, t = −1.206 on 396.9 degrees of freedom. The deposited code first rescales the cell means to run from 0 to 1, which `datawizard::rescale(pos, to = c(0, 1))` does; that changes the coefficient to −0.00203 and its standard error to 0.00169 but leaves t and the degrees of freedom as they are. The Task 2 report of the analyst says that the outcome was log-transformed, but the code rescales it and takes no logarithm, and the recorded t is the one that the code produces.

The year of EB 79.3 matters. With the year of the fieldwork, so that the cells of EB 79.3 take the unemployment of 2013 and the data have ten years, the same model gives t = −1.606, which is why a rebuild that dates every wave by its fieldwork does not recover the recorded value. The total of `n` is 411,855.

## Licence

The cells are means of the GESIS Eurobarometer files ZA4056, ZA4229, ZA4411, ZA4414, ZA4506, ZA4530, ZA4744, ZA4819, ZA4994, ZA5234, ZA5449, ZA5481, ZA5567, ZA5612, ZA5685 and ZA5689, which participants download themselves from GESIS under its terms of use. The unemployment series are Eurostat data, reusable under the reuse policy of the European Commission (Decision 2011/833/EU) with attribution to Eurostat as the source.
