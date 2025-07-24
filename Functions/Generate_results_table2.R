# -------------------------------------------------------------------------------------------
#' Generate Table 2: Crop-Specific Institutional Accession Tables
#'
#' Takes a summary of institutional accession counts (plus precomputed fields)
#' and returns a named list of tibbles, one per Crop_strategy. Each tibble is
#' sorted descending by accession count and includes:
#'   • Institution Code  
#'   • Institution Name  
#'   • Number of accessions  
#'   • Percent of total (two decimals, with “%” suffix)  
#'   • Cumulative percent (two decimals, with “%” suffix)  
#'   • Number of accessions in long term storage (-18–20 C) and source  
#'   • Number of accessions included in MLS (from GLIS)
#'
#' @param institution_accessions_summary Data frame with at least these columns:
#'   - Crop_strategy (chr)  
#'   - INSTCODE (chr)  
#'   - Institute_name (chr)  
#'   - institution_accessions_count (num)  
#'   - total_accessions (num)  
#'   - institution_accessions_perc (num)  
#'   - Number.of.accessions.in.long.term.storage.(-18-20.C).and.source (chr)  
#'   - Number.of.accessions.included.in.MLS.(from.GLIS) (num)
#'
#' @return Named list of tibbles (one per crop) with columns:
#'   Institution Code | Institution Name | Number of accessions |
#'   Percent of total | Cumulative percent |
#'   Number of accessions in long term storage (-18–20 C) and source |
#'   Number of accessions included in MLS (from GLIS)
#'
#' @import dplyr purrr
#' @export
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
  library(dplyr)
  library(purrr)
  
  crop_tables <- institution_accessions_summary %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy)))
  
  crop_tables <- map(crop_tables, function(df) {
    df %>%
      rename(
        storage_source_raw = `Number.of.accessions.in.long.term.storage.(-18-20.C).and.source`,
        mls_glis_raw       = `Number.of.accessions.included.in.MLS.(from.GLIS)`
      ) %>%
      mutate(
        `Institution Code`    = INSTCODE,
        `Institution Name`    = Institute_name,
        `Number of accessions`= institution_accessions_count,
        `Percent of total`    = sprintf("%.2f%%", institution_accessions_perc),
        `Cumulative percent`  = sprintf("%.2f%%", cumsum(institution_accessions_count) / total_accessions * 100),
        `Number of accessions in long term storage (-18-20 C) and source` = storage_source_raw,
        `Number of accessions included in MLS (from GLIS)` = mls_glis_raw
      ) %>%
      select(
        `Institution Code`,
        `Institution Name`,
        `Number of accessions`,
        `Percent of total`,
        `Cumulative percent`,
        `Number of accessions in long term storage (-18-20 C) and source`,
        `Number of accessions included in MLS (from GLIS)`
      ) %>%
      mutate(across(
        c(`Number of accessions`, `Percent of total`, `Cumulative percent`),
        ~ ifelse(.x == "-", "", .x)
      ))
  })
  
  return(crop_tables)
}
