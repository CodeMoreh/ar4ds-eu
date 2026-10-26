# Codebook: `trust_ess.csv`

The analysis extract of the immigration-and-trust case: every respondent of the 16 countries that the original author (OA) studied, in rounds 1–4 of the European Social Survey (ESS), 120,080 rows and 32 columns, one row per respondent. It holds the three trust items, the three immigration items, the individual controls of the model of the paper and the weights, with the derived variables built as the paper describes them in its appendix. It is written by `R/trust/01_extract.R` from the ESS files in the data hub of the course team, and it is about 10 MB.

**Adapted from ESS Rounds 1–4.** Adapted from ESS Round 1, edition 6.7; ESS Round 2, edition 3.6 (with the Italy country file of round 2); ESS Round 3, edition 3.7; and ESS Round 4, edition 4.6. The extract selects respondents and variables, reverses the coding of several items, builds the vote controls from the party questions, and standardises household income. No answer is otherwise changed. The source files:

- European Social Survey European Research Infrastructure (ESS ERIC). (2023). *ESS1 – integrated file, edition 6.7* [Data set]. Sikt – Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess1e06_7
- European Social Survey European Research Infrastructure (ESS ERIC). (2012). *ESS2 – integrated file, edition 3.6 (Italy not included)* [Data set]. Sikt – Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess2e03_6
- European Social Survey European Research Infrastructure (ESS ERIC). (2018). *ESS2 – Italy country file from main questionnaire* [Data set]. Sikt – Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess2it
- European Social Survey European Research Infrastructure (ESS ERIC). (2018). *ESS3 – integrated file, edition 3.7 (Latvia and Romania not included)* [Data set]. Sikt – Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess3e03_7
- European Social Survey European Research Infrastructure (ESS ERIC). (2023). *ESS4 – integrated file, edition 4.6 (Austria and Lithuania not included)* [Data set]. Sikt – Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess4e04_6

The rounds were read from a cumulative file of rounds 1–10 downloaded from the ESS Data Portal (ess.sikt.no) in March 2024, which carries these editions. The Italian file of round 2 is not part of the cumulative file. ESS data are licensed under CC BY-NC-SA 4.0, and so is this extract: it may be shared and adapted for non-commercial purposes, with attribution, under the same licence. ESS ERIC asks that its datasets be linked from the ESS Data Portal rather than hosted elsewhere, and that a copy stored elsewhere state its version. Anyone who wants the full files should download them from the portal.

## Rows

The 16 countries are Austria, Belgium, Switzerland, Germany, Denmark, Spain, Finland, France, the United Kingdom, Greece, Ireland, Italy, the Netherlands, Norway, Portugal and Sweden, with every round in which each took part: 60 country-rounds. Austria has no round 4 in the editions of the paper (its round 4 was released later as a separate file), Greece no round 3 and Italy no rounds 3 and 4. The Italian round 2 was fielded in 2006 and released as a country file of its own. The 120,080 rows are the total that the paper gives in its note 40. The respondents of the paper are the 110,732 rows with `citizen` = 1 and `minority` = 0, the citizens who do not belong to an ethnic minority; their count matches Table 1 of the paper in every country. The rows are sorted by country, round and ESS respondent number, which the file does not carry.

| Country | Round 1 | Round 2 | Round 3 | Round 4 |
|:--|--:|--:|--:|--:|
| AT | 2,257 | 2,256 | 2,405 | – |
| BE | 1,899 | 1,778 | 1,798 | 1,760 |
| CH | 2,040 | 2,141 | 1,804 | 1,819 |
| DE | 2,919 | 2,870 | 2,916 | 2,751 |
| DK | 1,506 | 1,487 | 1,505 | 1,610 |
| ES | 1,729 | 1,663 | 1,876 | 2,576 |
| FI | 2,000 | 2,022 | 1,896 | 2,195 |
| FR | 1,503 | 1,806 | 1,986 | 2,073 |
| GB | 2,052 | 1,897 | 2,394 | 2,352 |
| GR | 2,566 | 2,406 | – | 2,072 |
| IE | 2,046 | 2,286 | 1,800 | 1,764 |
| IT | 1,207 | 1,529 | – | – |
| NL | 2,364 | 1,881 | 1,889 | 1,778 |
| NO | 2,036 | 1,760 | 1,750 | 1,549 |
| PT | 1,511 | 2,052 | 2,222 | 2,367 |
| SE | 1,999 | 1,948 | 1,927 | 1,830 |

## Columns

An empty cell is a missing answer (refusal, don't know, no answer, not applicable). The scales of 0 to 10 are the ESS scales, several of them reversed as the paper reverses them, so that a high value always means more distrust, more concern or more dissatisfaction.

| Column | Meaning | Source variable | Missing |
|:--|:--|:--|--:|
| `cntry` | Country, ISO two-letter code (GB for the United Kingdom, GR for Greece) | `cntry` | 0 |
| `essround` | ESS round, 1 to 4 (fieldwork 2002–2003, 2004–2006, 2006–2007, 2008–2010) | `essround` | 0 |
| `citizen` | 1 = citizen of the country, 0 = not | `ctzcntr` | 66 |
| `minority` | 1 = belongs to an ethnic minority group in the country, 0 = does not | `blgetmg` | 1,406 |
| `dweight` | Design weight (4 decimals) | `dweight` | 0 |
| `pspwght` | Post-stratification weight, including the design weight (4 decimals) | `pspwght` | 0 |
| `pweight` | Population size weight, constant within a country-round (5 significant digits); to be multiplied by `dweight` or `pspwght` | `pweight` | 0 |
| `distrust_parl` | Distrust of the national parliament, 10 − trust (0 = complete trust, 10 = no trust at all) | `trstprl` | 3,380 |
| `distrust_pol` | Distrust of politicians, 10 − trust | `trstplt` | 1,894 |
| `distrust_legal` | Distrust of the legal system, 10 − trust | `trstlgl` | 2,330 |
| `imm_econ` | Immigration is bad for the economy, 10 − answer (0 = good, 10 = bad) | `imbgeco` | 4,529 |
| `imm_culture` | Cultural life is undermined by immigrants, 10 − answer (0 = enriched, 10 = undermined) | `imueclt` | 4,147 |
| `imm_place` | Immigrants make the country a worse place to live, 10 − answer (0 = better, 10 = worse) | `imwbcnt` | 3,627 |
| `unhappy` | Unhappiness, 10 − happiness | `happy` | 456 |
| `life_dissat` | Dissatisfaction with life as a whole, 10 − satisfaction | `stflife` | 497 |
| `meet_rarely` | How rarely the respondent meets friends, relatives or colleagues socially, 8 − answer (1 = every day, 7 = never) | `sclmeet` | 224 |
| `distrust_people` | "You can't be too careful" rather than "most people can be trusted", 10 − answer | `ppltrst` | 358 |
| `unfair_people` | Most people would try to take advantage, 10 − answer | `pplfair` | 672 |
| `unhelpful_people` | People mostly look out for themselves, 10 − answer | `pplhlp` | 525 |
| `econ_dissat` | Dissatisfaction with the present state of the economy, 10 − satisfaction | `stfeco` | 2,697 |
| `income_difficulty` | Feeling about household income nowadays, as asked: 1 = living comfortably, 2 = coping, 3 = finding it difficult, 4 = finding it very difficult | `hincfel` | 4,513 |
| `health_dissat` | The state of health services is bad, 10 − answer | `stfhlth` | 1,212 |
| `educ_dissat` | The state of education is bad, 10 − answer | `stfedu` | 5,241 |
| `gov_dissat` | Dissatisfaction with the way in which the government is doing its job, 10 − satisfaction (not a control of the paper; two Multi100 analysts used it) | `stfgov` | 5,784 |
| `left_right` | Placement on the left–right scale, 0 = left, 10 = right | `lrscale` | 13,058 |
| `hh_income_z` | Household income band, standardised within each country-round over every respondent who answered (mean 0, SD 1; 3 decimals): twelve bands in rounds 1–3, deciles in round 4 | `hinctnt`, `hinctnta` | 28,601 |
| `voted` | 1 = voted at the last national election, 0 = did not vote or was not eligible | `vote` | 1,306 |
| `winner` | 1 = voted for a party that sat in the national cabinet on the day of the interview; 0 = voted for another party, did not vote, or gave no party | `prtv*` questions, cabinets of ParlGov | 0 |
| `far_right_vote` | 1 = voted for a party that the appendix of the paper lists as far right; 0 otherwise, non-voters included | `prtv*` questions, appendix of the paper | 0 |
| `age` | Age in years, as computed by the ESS, from 13 to 123; the value of 123 is implausible, and every value is kept as recorded | `agea` | 565 |
| `female` | 1 = female, 0 = male | `gndr` | 118 |
| `education` | Highest level of education in five harmonised ISCED levels: 1 = less than lower secondary, 2 = lower secondary, 3 = upper secondary, 4 = post-secondary non-tertiary, 5 = tertiary; "other" (55) and "not possible to harmonise" (0) are missing | `edulvla` | 540 |

## How the derived variables are built

- **The scales.** The measure of concern about immigration is the mean of `imm_econ`, `imm_culture` and `imm_place`; interpersonal distrust is the mean of `distrust_people`, `unfair_people` and `unhelpful_people`; and the mean of the three trust items is a scale of distrust in politics. The estimator builds each scale from its items by one of two rules, which are a decision of the specification menu. Under the first, a scale is missing unless every item was answered, and under the second it is the mean of the items that were answered. The file therefore carries the items, not the scales.
- **The vote controls.** The ESS asks each respondent which party they voted for at the last national election, with one list of parties per country and round (and two votes in Germany, for the constituency and for the list; either counts). `winner` follows the definition of the paper, a vote for a party in government at the time of the survey: each party code is linked to its ParlGov key through the Party Facts crosswalk, and the cabinet in office on the day of the interview is read from the ParlGov cabinet table, so that a change of government during fieldwork changes who counts as a winner. `far_right_vote` follows the list of far-right parties in the web appendix of the paper. Where the ESS list has no code of its own for a listed party (the Sweden Democrats; the British National Party in rounds 1, 2 and 4; the Portuguese National Renewal Party in rounds 1–3; One NL and the Party for Freedom in the Dutch round 3; the Norwegian Democrats), its voters count as 0. The lookup tables and their sources are in `data-raw/trust/`.
- **Household income.** The paper standardises income within each survey, because the bands differ between rounds 1–3 and round 4. `hh_income_z` is standardised over every respondent of the country-round who answered, minorities and non-citizens included.
- **Interview date.** The date that decides the cabinet in office is the date of the interview (rounds 1–2) or of its start (rounds 3–4); 111 respondents without a usable date take the median date of their country-round. The date itself is not in the file.

## Not in the file

The respondent number (`idno`), the interview date, the party voted for and the other ESS variables are left out to keep the file small; the respondent number is in the gitignored copy that the pipeline keeps. The education scale of seven categories that the paper describes (0 to 6) is not in the cumulative ESS file. The country-level variables of the model are in `trust_macro.csv` ([codebook](trust_macro_codebook.md)), one row per country-round, to be joined by `cntry` and `essround`.
