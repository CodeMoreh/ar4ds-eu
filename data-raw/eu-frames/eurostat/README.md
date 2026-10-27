# Eurostat tables behind the unemployment and growth series

Four of the five Multi100 reanalysts merged an unemployment series from Eurostat; the fifth, analyst C6HJR, used the World Bank national estimate that the course panel carries. This folder holds the Eurostat tables that those four drew on, and the Eurostat growth table, so that `R/eu-frames/02_macro_series.R` can cut each series from the table as published rather than from a substitute.

## Why an age band is part of the predictor

Eurostat does not publish one unemployment rate. It publishes a cube, broken down by age band, by sex and by unit of measurement, and an analyst who merges "the unemployment rate" has to select a cell from it. The selection decides the country-year values of the single predictor whose coefficient Task 2 asked each analyst to report, so two analysts who both write that they used the Eurostat unemployment rate can be testing measurably different variables. The World Bank series has no such dimension, being a single figure per country-year. Over the 270 country-years of the panel, the four slices average between 6.34 and 10.26 per cent.

## Retrieval

The files were retrieved on 4 September 2026 from the SDMX 2.1 dissemination endpoint of Eurostat, `https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/<code>?format=TSV`, and are stored as retrieved, gzipped and otherwise unaltered. The older bulk service (`api/dissemination/files?file=data/<code>.tsv.gz`) is retired and answers 410 Gone for every code used here.

| File | Table | Time span | Rows |
|:--|:--|:--|--:|
| `une_rt_a_h.tsv.gz` | Harmonised unemployment rate, annual | 1992–2020 | 2,268 |
| `une_rt_a.tsv.gz` | Unemployment rate, annual, current table | 2003–2025 | 2,394 |
| `tipsun20.tsv.gz` | Unemployment rate, annual, short table | 2003–2025 | 90 |
| `lfsa_urgan.tsv.gz` | LFS unemployment rate by sex, age and citizenship | 1995–2025 | 26,201 |
| `nama_10_gdp_growth.tsv.gz` | Real GDP growth on the previous year, the `B1GQ` / `CLV_PCH_PRE` slice of `nama_10_gdp` | 1975–2025 | 44 |
| `une_rt_a_h_2023-12-22.tsv.gz` | Harmonised unemployment rate, annual, as published in December 2023 | 1992–2020 | 2,214 |

`une_rt_a_h_2023-12-22.tsv.gz` is an earlier vintage of the same historical table: the bulk file that Eurostat last modified on 22 December 2023, in the copy that the Internet Archive kept on 29 December 2023 (`https://web.archive.org/web/20231229000344id_/https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/une_rt_a_h.tsv.gz`), stored as retrieved on 27 October 2026. It has the older bulk layout, without the `freq` column and with the years in descending order. Between that vintage and the retrieval of September 2026 Eurostat revised 1,232 cells of the table for the years 2004–2013, most of them for Croatia and the aggregates of the European Union, but only one of them in the two slices that analysts KQXUE and 018OL merged for the 27 member states: Portugal in 2013, from 12.9 to 13.0 on the slice of KQXUE (and from 16.4 to 16.5 on the slice of 018OL, which uses no year after 2012). The Task 2 result of KQXUE reproduces exactly on the earlier value, so `R/eu-frames/15_analyst_panels.R` builds the reproduction files of both analysts from this vintage and checks that the two vintages differ on those slices in that one cell. `02_macro_series.R` still reads the retrieval of 2026, so the series `es_2064_pop` on the curve of the claim carries 13.0 for Portugal in 2013.

`une_rt_a` is kept for documentation and is not read by any script. It is the table that a search for annual unemployment reaches today, it is empty before 2009 for every member state except France, and it has no United Kingdom row, which is why the series can appear to be unavailable for 2004–2013. `une_rt_a_h` is the historical companion and covers all 27 member states, the United Kingdom included, for the whole period.

The growth table is a row subset of `nama_10_gdp`, which is 11.7 MB whole and carries some forty units of measurement. Only the annual percentage change in chain-linked volumes of gross domestic product is kept, which is the construct of the World Bank growth series in the panel. Eurostat has withdrawn the national accounts of the United Kingdom, so this table has no United Kingdom row for any year, and choosing the Eurostat growth series costs a country.

## The four slices

| Analyst | Table and slice | How it is established |
|:--|:--|:--|
| 018OL | `une_rt_a_h`, Y15-74, PC_ACT, T | Named in the deposited code, which filters `une_rt_a_h_linear.csv.gz` on exactly these three values and recodes UK to GB and EL to GR |
| KQXUE | `une_rt_a_h`, Y20-64, PC_POP, T | The `eurostatuse une_rt_a_h … keepdim(Y20-64 ; PC_POP ; T)` call in `task2.do` |
| KEVF1 | `tipsun20`, Y15-74, PC_ACT, T | Recovered by matching the merged values in the deposited dataset against every band in the table: 123 of 135 cells agree to within 0.05 points on this slice and none agree on any other |
| PRL47 | `lfsa_urgan`, Y25-64, PC, T, citizen TOTAL | Inferred, not documented. The code merges a local file, `unemp_comp.dta`, that was not deposited, and builds fifteen variables spanning the bands 15-64, 15-74, 20-64, 25-64 and 25-74 by total, male and female; `lfsa_urgan` is the table that carries exactly that set, and an inline comment, `Decision: un_2564_tot`, records which one was carried forward |

PC_ACT expresses unemployment as a percentage of the active population, which is the unemployment rate as ordinarily understood. PC_POP expresses it as a percentage of the total population and is a different quantity: across the panel it averages 6.34 against 8.71 for the labour-force rate, and no cell of the two agrees to within a tenth of a point. The predictor of KQXUE is therefore a distinct measure rather than a rescaling of the others.

`tipsun20` is short in two ways that shaped the sample of KEVF1 without any decision being taken: it carries no United Kingdom row, and it is empty before 2009 for every country except France. Eurostat has since backfilled Sweden for 2005–2008, so the table today has four cells that KEVF1 did not have.

## Licence

Eurostat data are reusable under the reuse policy of the European Commission (Decision 2011/833/EU), with attribution to Eurostat as the source.
