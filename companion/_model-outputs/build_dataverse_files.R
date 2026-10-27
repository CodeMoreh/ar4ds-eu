################################################################################
# Title:        build_dataverse_files.R
# Purpose:      The list of files in the Dataverse record of the housing-audit
#               case, as the native API of the Harvard Dataverse returns it, so
#               that the data-sources module can show what a programmatic
#               query of the record gives without a connection at render time.
# Reads:        the metadata of doi:10.7910/DVN/1HOVTU, fetched from the API of
#               dataverse.harvard.edu (public, CC0; nothing licensed involved)
# Writes:       companion/_model-outputs/dataverse_1HOVTU_files.csv
# Author:       Claude for CM
# Last updated: 2026-10-27
################################################################################

record <- "doi:10.7910/DVN/1HOVTU"
meta <- jsonlite::fromJSON(paste0("https://dataverse.harvard.edu/api/datasets/:persistentId/?persistentId=",
                                  record))
latest <- meta$data$latestVersion

# One row per file of the latest version. Dataverse stores a tabular file twice,
# as the original that was deposited (here a Stata file) and as tab-separated
# text; the size and MD5 sum below are those of the deposited original where
# there is one, which is the file that ?format=original returns.
files <- latest$files$dataFile |>
  tibble::as_tibble() |>
  dplyr::transmute(
    label = latest$files$label,
    file_doi = persistentId,
    file_id = id,
    original_format = dplyr::coalesce(originalFileName, filename),
    bytes = dplyr::coalesce(as.numeric(originalFileSize), as.numeric(filesize)),
    md5 = md5,
    version = paste(latest$versionNumber, latest$versionMinorNumber, sep = "."),
    released = substr(latest$releaseTime, 1, 10)
  ) |>
  # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
  dplyr::arrange(label)

readr::write_csv(files, "companion/_model-outputs/dataverse_1HOVTU_files.csv")
