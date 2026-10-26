# Codebook: `cohab_unions.csv`

The union file of the cohabitation case: every woman of five cycles of the National Survey of Family Growth (NSFG) whose first cohabitation came before any marriage, one row per woman and so one row per union, 19,183 rows and 23 columns (2.9 MB). It records when the union began, when and how it ended (a marriage of the partners, a separation, or still intact at the interview), the age of the woman when it began, her education and race-ethnicity, and the weight and design variables of her survey. It is written by `R/cohabitation/01_unions.R` from the public-use female respondent files of the NSFG, and every model of the case starts from it: `spec_unions()` in `R/cohabitation/_tools.R` selects the unions of a specification and `person_months()` turns them into person-months.

**Source.** National Center for Health Statistics (NCHS), National Survey of Family Growth, public-use female respondent files of Cycle 5 (1995), Cycle 6 (2002), 2006–2010, 2011–2013 and 2013–2015, downloaded on 2026-10-26 from ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/ (file names in the README of `R/cohabitation/`). The files are in the public domain. Their use is bound by the NCHS data use restrictions: the data may be used for statistical reporting and analysis only, no attempt may be made to identify a respondent, and the files may not be linked with data that identify individuals. The file holds no identifier beyond the case number of the survey.

## The unions

| Cycle | Women interviewed | Rows (first premarital cohabitations) | Window of the paper |
|:--|--:|--:|--:|
| 1995 | 10,847 | 3,816 | 929 |
| 2002 | 7,643 | 3,493 | 966 |
| 2006-10 | 12,279 | 5,988 | 1,142 (1,627 without the restriction to 2005 or later) |
| 2011-13 | 5,601 | 2,934 | 714 |
| 2013-15 | 5,699 | 2,952 | 719 |

: Women and first premarital cohabitations by survey. The window of the paper is `in_window` (and `from_2005` in 2006–2010).

A row is a woman whose first cohabitation came before her first marriage, or who has cohabited and never married (the recode COHSTAT = 2 of every survey). The window of the paper of the original authors (OAs), described in its section "Data and Methods" and in Table 1, keeps unions that began no more than five years before the interview and lasted at least a month, and in 2006–2010 only those that began in 2005 or later. It gives 929 unions in 1995 (340 marriages, 347 separations, 242 intact) and 1,142 in 2006–2010 (194, 504 and 444), the counts of Table 1 of the paper; the shares by race-ethnicity and education reproduce those of Table 1 to two decimals.

**How the end of a union is found.** From 2002 the recodes give the duration of the first cohabitation in months (COH1DUR: to the interview for an intact union, to the marriage for one that became a marriage, to the end of living together for one that dissolved), and the end is the start plus that duration. The 1995 file has no such recode, so the end comes from the source of the start date, as the recode specification of COHAB1 and COHOUT describes it: for a union that became a marriage, the date of the marriage to the husband with whom it began (MARDAT01 for the first husband, and so on); for a union that dissolved, the date on which the woman stopped living with that partner (STOPOTH0 to STOPOTH7, "the last time"); for an intact union, the interview. Fourteen 1995 unions that dissolved have no stop date (not ascertained, refused or not known), and one ends before it begins. Ten of the fourteen began more than five years before the interview; the other four began within the five years, but without a length they cannot be shown to have lasted a month, so `in_window` is empty for them and the window of the paper, which still gives its 929 unions, leaves them out.

## Columns

| Column | Meaning |
|:--|:--|
| `cycle` | The survey: `1995`, `2002`, `2006-10`, `2011-13`, `2013-15` |
| `caseid` | The case number of the respondent in her survey (unique within a cycle only; read as character) |
| `weight` | The final weight of the survey: POST_WT (1995), FINALWGT (2002), WGTQ1Q16 (2006–2010), WGT2011_2013, WGT2013_2015. Each sums to the population of women aged 15–44 of its survey |
| `stratum` | The variance stratum, prefixed with the cycle: COL_STR (1995, the collapsed strata of the NHIS sample from which the 1995 survey was drawn) or SEST (later surveys) |
| `psu` | The variance cluster within the stratum, prefixed with the stratum: PANEL (1995) or SECU (SECU_R in 2002). Every stratum and cluster of each survey holds at least one woman of the file, which `R/cohabitation/01_unions.R` checks, so the file carries the whole design of each survey |
| `cm_interview` | The month of the interview as a century month (months since December 1899: 1 = January 1900, 1261 = January 2005): A_DOI (1995), CMINTVW (later) |
| `cm_start` | The month in which the couple began to live together (COHAB1), century month |
| `cm_end` | The month of the marriage, of the separation, or of the interview for an intact union (see above), century month; empty for the 14 1995 unions without a stop date |
| `year_start` | The calendar year of `cm_start` |
| `since_start` | Months from the start of the union to the interview |
| `duration` | Months from the start to the end of the union (`cm_end` − `cm_start`); 0 for a union that ended in the month in which it began |
| `outcome` | How the union had ended by the interview: `marriage` (the partners married, whether or not the marriage lasted: COHOUT 2 or 3), `separation` (COHOUT 4) or `intact` (COHOUT 1) |
| `age_start` | Age in years when the union began: (`cm_start` − month of birth) / 12, from BDAYCENM (1995) or CMBIRTH (later) |
| `age_interview` | Age at the interview in completed years (AGE in 1995, AGER later) |
| `educ` | Education at the interview in the four groups of the paper: `less_hs` (less than a high school diploma), `hs` (high school diploma or GED), `some_college` (some college, or an associate degree), `college` (a bachelor's degree or more). Built from `hieduc_fixed` |
| `educ_score` | The same four groups as the SCORE file codes them: the associate degree counted as `college`, and HIEDUC as released (without the correction of 2006–2010) |
| `race` | Race and Hispanic origin (HISPRACE, the 1977 standards of the Office of Management and Budget): `hispanic`, `white` (non-Hispanic white), `black` (non-Hispanic Black), `other` (non-Hispanic other) |
| `hieduc` | HIEDUC as released: the highest year of school completed or degree received. Codes below 9 are years of school (from 1 in 1995, from 5 later), 9 a high school diploma or GED, 10 some college without a degree, 11 an associate degree, 12 a bachelor's degree, 13 and above a higher degree |
| `hieduc_fixed` | HIEDUC with the correction that NCHS published for 2006–2010 (`HIEDUC_correction_2006_2010.pdf`): from the second year of interviewing, women with 12 or fewer years of school, a GED and no college degree had been given their years of school instead of 9. According to that note the error affects 410 women of the whole 2006–2010 sample; in the other cycles `hieduc_fixed` equals `hieduc` |
| `hisprace` | HISPRACE as released (1 Hispanic, 2 non-Hispanic white, 3 non-Hispanic Black, 4 non-Hispanic other) |
| `imputed` | TRUE when NCHS imputed the start, the outcome or (from 2002) the duration of the union (the imputation flags COHAB1_I, COHOUT_I and COH1DUR_I); 10 of the 1,142 unions of 2006–2010 in the window of the paper, none in 1995 |
| `in_window` | TRUE for a union that began no more than five years (60 months) before the interview and lasted at least a month: the five-year rule of the paper |
| `from_2005` | TRUE for a union that began in January 2005 or later, the second restriction that the paper applies to the 2006–2010 survey |

## Using the file

`spec_unions(unions, cohorts, short, comparison)` keeps the unions of a specification: the two surveys of the comparison, `in_window` (with the unions of less than a month added back under `short = "keep"`), and `from_2005` in 2006–2010 under `cohorts = "paper"`. `person_months()` turns each union, with `survival::survSplit()`, into one row for every month from the month in which the couple moved in to the month in which the union ended or the woman was interviewed. The weighted shares are shares of the population of each survey; the design-based errors of the case use `stratum` and `psu` of both surveys.
