# Codebook: `cohab_months.csv`

The person-month file of the cohabitation case: one row for every month in which a first premarital cohabitation of the two cohorts of the paper was at risk of ending, 44,878 rows and 13 columns (4.2 MB). A union contributes a row for the month in which the couple moved in (month 1), one for each month after it, and one for the month in which the partners married, separated or, for an intact union, the woman was interviewed; the outcome of the union is recorded in that last row and every earlier row is `intact`. This is the structure of a discrete-time event-history model, which the paper of the original authors (OAs) describes: "the first month indicating the month when cohabitation started and last month the month when cohabitation was ended either by marriage or separation or the month when interview was conducted for those whose first premarital cohabitation remained intact at the time of interview (i.e., censoring)".

The file is written by `R/cohabitation/02_person_months.R` with `person_months()` of `R/cohabitation/_tools.R`, from `cohab_unions.csv` ([codebook](cohab_unions_codebook.md)), whose source and terms of use it shares (NSFG public-use files, public domain, statistical use only). It holds the 2,556 unions of NSFG 1995 and NSFG 2006–2010 that began no more than five years before the interview and lasted at least a month; `from_2005` marks the 1,142 unions of 2006–2010 that the paper keeps. The window of the paper is therefore `cycle == "1995" | from_2005`: 33,197 person-months, 2,071 unions, 534 marriages and 851 separations. The paper reports 54,291 person-months for the same 2,071 unions (Table 2); no construction that the course tried reproduces that count, although the coefficients of Table 2 are reproduced closely.

## Columns

| Column | Meaning |
|:--|:--|
| `cycle` | The survey: `1995` or `2006-10` |
| `caseid` | The case number of the woman in her survey (read as character) |
| `month` | The month of the union: 1 for the month in which the couple moved in, up to 61 |
| `year_of_union` | The year of the union to which the month belongs, 1 (months 1–12) to 5 (months 49–61), for the yearly baseline of the menu |
| `y` | What happened in the month: `marriage`, `separation`, or `intact` (the couple were still living together unmarried at its end, or the woman was interviewed in it) |
| `later` | 1 for the later cohort (NSFG 2006–2010), 0 for the earlier one (NSFG 1995) |
| `from_2005` | TRUE for a union of 2006–2010 that began in 2005 or later; FALSE for every union of 1995 |
| `age_start` | Age in years when the union began, to three decimals |
| `race` | `hispanic`, `white`, `black` or `other` (non-Hispanic) |
| `educ` | `less_hs`, `hs`, `some_college` or `college`, in the coding of the paper |
| `weight` | The final weight of the survey of the woman, the same in every month of her union |
| `stratum`, `psu` | The variance stratum and cluster of the woman, prefixed with the cycle |

## Using the file

The file is for looking at the data: how a union becomes a set of rows, how many months each cohort contributes, and how rare an event is in any one month (684 marriages and 1,083 separations among 44,878 months). The models of the course start from `cohab_unions.csv`, because a specification can change which unions and which months enter (a three-year horizon stops every union at month 36); `spec_frame()` of `R/cohabitation/_tools.R` builds the rows of each specification in the same way as this file.
