# Codebook: `score_person_months.csv`

The person-month file that SCORE built for its replication of the paper of the original authors (OAs) and that Multi100 gave the five analysts of the claim, "cohabitors have increasingly become less likely to progress to marriage": 70,188 rows of 3,963 women from five cycles of the National Survey of Family Growth (NSFG), 18 columns, 3.0 MB. The course ships it unchanged, byte for byte, so that the reproduction of Task 3 and the five Multi100 analyses run offline in the downloadable workspace and in the browser lab. `R/cohabitation/04_reference_marks.R` refits the five analyses on it and on a copy with its 1995 cohort rebuilt (`analysts5.csv`, [codebook](analysts5_codebook.md)), and `R/cohabitation/06_displays.R` counts its cohorts against the course rebuild (`display_score_file.csv`, [codebook](displays_codebook.md)).

## Provenance

| | |
|:--|:--|
| Original name | `Estimation Data - Kuo (JWzJ).csv` |
| Where it is published | osf.io/b6p3j, a file of component jkzdq of the SCORE replication of the paper (osf.io/u8x9s, SCORE replication 6547), within the SCORE dossier on the paper (osf.io/kv38j) |
| Uploaded | 2021-12-05 |
| Size | 3,047,462 bytes |
| MD5 sum | `2b656d3814d273f4a93e29bc4f0139ef` |
| Copies | The copies that analysts 456AP and 5X3KG uploaded to their Multi100 components are identical to it; the `Data.csv` of analyst 73FDT holds the same values without quotation marks around the column names |

The MD5 sum is a short code computed from the contents of a file, and it changes if a single byte changes; the pipeline and the workspace check it before they use the file, so a match means that this is the file that the analysts received. The pipeline fetches the OSF copy only when the committed file is missing.

**Terms.** The report of the replication states: "All materials on this OSF project may be shared publicly." The file is derived from the public-use files of the NSFG, which the National Center for Health Statistics (NCHS) produces and which are in the public domain. Their use is bound by the data use restrictions of NCHS: the data may be used for statistical reporting and analysis only, no attempt may be made to identify a respondent, and the file may not be linked with data that identify individuals. It holds no identifier beyond the case number of the survey.

## The rows

Each woman whose first premarital cohabitation SCORE selected contributes one row for every month of that union, numbered from 1. A union that was still intact at the interview contributes as many rows as its duration in months (`COH1DUR`), all with the outcome 1; a union that ended in a marriage or a separation contributes one row more, and the outcome is recorded in its last row, every earlier row reading 1. `COH1DUR` and `AGE` are the same in every row of a woman.

| Survey | Women | Rows | Intact | Marriages | Separations |
|:--|--:|--:|--:|--:|--:|
| 1995 | 376 | 5,414 | 3 | 4 | 369 |
| 2002 | 1,094 | 21,574 | 366 | 329 | 399 |
| 2006–2010 | 1,150 | 18,535 | 446 | 195 | 509 |
| 2011–2013 | 658 | 11,725 | 274 | 105 | 279 |
| 2013–2015 | 685 | 12,940 | 265 | 132 | 288 |

: Women, rows and the outcome of each union by survey (the outcome in the last row of each woman).

## Columns

| Column | Meaning |
|:--|:--|
| `CASEID` | The case number of the woman in her survey. The ranges of the five surveys do not overlap, so it identifies a woman across the file |
| `MONTHNUM` | The month of the union, from 1 |
| `OUTCOME` | 1 for a month in which the couple were still living together unmarried (intact), 2 for the month of a marriage, 3 for the month of a separation ("break up") |
| `LESSHIGH`, `HIGH`, `SOMECOLLEGE`, `COLLEGEPLUS` | Education in four groups, one of the four equal to 1: less than a high school diploma, a high school diploma or GED, some college, a college degree. The associate degree counts as a college degree, and the 2006–2010 survey uses HIEDUC as released, without the correction that NCHS published for its coding of the GED |
| `P95`, `P02`, `P10`, `P13`, `P15` | The survey, one of the five equal to 1: NSFG 1995, 2002, 2006–2010, 2011–2013 and 2013–2015. `P02` is the last column of the file |
| `NHW`, `AA`, `HSP`, `ONH` | Race and Hispanic origin, one of the four equal to 1: non-Hispanic white, Black ("African American"), Hispanic, non-Hispanic other |
| `COH1DUR` | The duration of the whole union in months, repeated in every row of the woman. It is known only once the union has ended or the woman has been interviewed, so a model that controls for it compares unions by how they turned out |
| `AGE` | Age in years when the union began, rounded up |

The file carries no weights, strata or clusters, so every analysis of it is unweighted and treats the months as independent.

## The 1995 rows

The 1995 cohort of the file is wrong. The paper analyses 929 unions of NSFG 1995 that began in the five years before the interview: 340 became a marriage, 347 dissolved and 242 were still intact. The file holds 376 unions of 1995, and 369 of them end in a separation, 4 in a marriage and 3 are intact. The data dictionary of SCORE (`Data Dictionary.xlsx`, in the same component) builds the duration of a 1995 union as STOPOTH0 − STRTOTH0, the date on which the woman stopped living with her first former partner minus the date on which she began to. That date exists only for a union that dissolved, so the unions that became a marriage or were still intact at the interview almost all drop out. Of the 376 unions, 351 are among the 929 of the paper; the other 25 lie outside its window. In the other four surveys the windows of years differ a little from those of the course rebuild, but every union in them is a union of the rebuild with the same outcome and duration, and the 2006–2010 cohort holds the 1,142 unions of the paper and 8 that began 61 to 63 months before the interview.

A comparison of the share of unions, or of months, that end in a marriage in 1995 and in a later survey therefore finds on this file that cohabitors became more likely to marry, the opposite of the claim; a comparison of marrying against staying together, one row per union, does not, because the intact unions of 1995 are missing as well. Analyst 5X3KG compared the share of rows that record a marriage in 1995 and in 2006–2010 and found a chi-square of 47.474 on one degree of freedom, which the file reproduces exactly. With the 1995 rows rebuilt the same test gives 87.68 in the direction of the claim. Analyst 456AP noticed the problem ("the data for 1995 in the OSF dataset seemed faulty, since there were only 4 marriages on record") and compared 2002 with 2013–2015 instead.

## Reading the file

`read.csv("data/score_person_months.csv")` and `readr::read_csv()` both read every column as a number. The outcome of each union is in its last row, so one row per woman comes from keeping the row with the largest `MONTHNUM`, and a table of the outcomes of the 1995 unions from those rows with `P95 == 1`.
