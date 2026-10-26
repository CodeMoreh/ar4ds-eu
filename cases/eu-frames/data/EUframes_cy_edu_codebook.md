---
aliases:
  - /data/EUframes_cy_edu_codebook.html
---

# Codebook: `EUframes_cy_edu.csv`

The course panel split by education band: one row per country, year and band, 810 rows (27 countries × 10 years × 3 bands). It is built from the respondent file by section 6 of `prep/02_scales_and_panel.R`, and it is the aggregate used in the individual-level extension of the course to ask whether national unemployment narrows the gap between the high- and the low-educated in the way they frame the EU. Every row is a mean over at least 43 respondents and most rows over several hundred, so the file is released under CC BY 4.0, like `EUframes_cy.csv`.

## Variables

| Variable | Description |
|---|---|
| `cntry` | ISO 3166 two-letter code of the member state, as in `EUframes_cy.csv` |
| `year` | Fieldwork year, 2004–2013 |
| `edu3` | Education band, from the age at which full-time education finished: `low` (under 16), `mid` (16 to 19), `high` (20 and over); still-studying respondents are placed by their current age and respondents with no full-time education are excluded, as in the published analysis |
| `mcosmo`, `mutil`, `mcomm`, `mlib` | Mean of the respondents' cosmopolitan, utilitarian, communitarian and libertarian shares in the cell, the published scoring rule, respondents who named nothing scored zero, respondents averaged as interviewed |
| `mpos`, `mneg` | The same for the positive (cosmopolitan + utilitarian) and negative (communitarian + libertarian) composites |
| `n` | Respondents in the cell, the count behind each mean and the basis of a precision weight |
| `growth`, `unemp`, `bailout` | The macro variables from `EUframes_cy.csv`, repeated on the row of each band: GDP growth and the unemployment rate from the World Bank, the assistance-programme indicator coded by rule |

## Reading it

The education gap in a country-year is the difference between the `high` and the `low` rows of the same country and year, and the claim that the file is built to test is that the gap narrows as `unemp` rises. Country and year fixed effects remove the average level of each country and the shift that is common to all countries in a year. They do not absorb `unemp`, which moves within each country from year to year, so both its main effect and its interaction with the band can be estimated from these rows; only a separate effect for every country-year would absorb the main effect. The interaction measures how far the gap between the bands in a country-year moves with `unemp`. The three bands of a country share one country effect, so the interaction draws on differences between countries in the size of the gap as well as on change within each country. A separate country effect for each band would confine it to change within countries. The smallest cells are the low band in the Czech Republic and Slovakia and the high band in Malta, at between 43 and 56 respondents; a precision weight by `n` gives them correspondingly less influence.

## Licence

CC BY 4.0, as derived aggregates. The respondent-level records behind the means are licensed by GESIS and are not distributed; the [respondent codebook](eb_respondents_codebook.md) describes them.
