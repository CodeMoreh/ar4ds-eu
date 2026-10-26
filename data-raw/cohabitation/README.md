# Committed inputs of the cohabitation case

Small inputs that the pipeline in `R/cohabitation/` needs besides the NSFG files, which stay in the data hub (`D:\ACADEMIC\DATA\SURVEYS - national other\NSFG (USA)\`) and are read only by `R/cohabitation/01_unions.R`. Nothing here holds survey responses.

| File | What it holds | Source and licence |
|---|---|---|
| `multi100/multi100_kuo_extract.csv` | The five rows of the Multi100 record for the paper (`Kuo_Demography_2016_JWzJ`), one per analyst, without the columns that carry names, sign-up details and timestamps. See `multi100/README.md`. | Multi100 processed data (github.com/marton-balazs-kovacs/multi100). |

`cache/` (gitignored) holds the person-month file that SCORE built for its replication of the paper and that Multi100 linked as the original materials of the five analysts, `Estimation Data - Kuo (JWzJ).csv` (osf.io/b6p3j, component jkzdq of osf.io/u8x9s; 70,188 rows; MD5 2b656d3814d273f4a93e29bc4f0139ef). It is derived from the public-use NSFG files, and the report of the replication states that "All materials on this OSF project may be shared publicly"; it is kept out of the repository all the same, like the Multi100 file of the immigration-and-trust case, and `R/cohabitation/04_reference_marks.R` fetches it when it is missing. On 2026-10-26 the OSF file server returned "502 Bad Gateway" for the download link, and the cache was filled from the copy taken earlier that day (same MD5 sum).
