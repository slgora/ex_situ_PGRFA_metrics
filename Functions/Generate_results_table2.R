# -------------------------------------------------------------------------------------------
#' Generate Table 2: Crop-Specific Institutional Accession Tables
#'
#' Takes a summary of institutional accession counts (plus two precomputed
#' fields from your raw summary) and returns a named list of tibbles, one per
#' Crop_strategy. Each tibble is sorted descending by accession count and
#' includes:
#'   • Institution Code  
#'   • Institution Name  
#'   • Number of accessions  
#'   • Number of accessions in long term storage (-18-20 C) and source  
#'   • Cumulative % (two decimals, with “%” suffix)  
#'   • Number of accessions included in MLS (from GLIS)
#'
#' @param institution_accessions_summary Data frame with at least these columns:
#'   - Crop_strategy (chr)  
#'   - INSTCODE (chr)  
#'   - Institute_name (chr)  
#'   - institution_accessions_count (num)  
#'   - total_accessions (num)  
#'   - Number.of.accessions.in.long.term.storage.(-18-20.C).and.source (chr)  
#'   - Number.of.accessions.included.in.MLS.(from.GLIS) (num)
#'
#' @return Named list of tibbles (one per crop) with columns:
#'   Institution Code | Institution Name | Number of accessions |
#'   Number of accessions in long term storage (-18-20 C) and source |
#'   Cumulative % | Number of accessions included in MLS (from GLIS)
#'
#' @import dplyr purrr
#' @export
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
  library(dplyr)
  library(purrr)
  
  # 1) Split by crop strategy, sort descending by accession count
  crop_tables <- institution_accessions_summary %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy)))
  
  # 2) Rename raw cols and build each sheet
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
        `Number of accessions in long term storage (-18-20 C) and source` = storage_source_raw,
        # add "%" suffix and always two decimals
        `Cumulative %` = sprintf(
          "%.2f%%",
          cumsum(institution_accessions_count) / total_accessions * 100
        ),
        `Number of accessions included in MLS (from GLIS)` = mls_glis_raw
      ) %>%
      select(
        `Institution Code`,
        `Institution Name`,
        `Number of accessions`,
        `Number of accessions in long term storage (-18-20 C) and source`,
        `Cumulative %`,
        `Number of accessions included in MLS (from GLIS)`
      ) %>%
      # replace any literal “–” with blank
      mutate(across(
        c(`Number of accessions`, `Cumulative %`),
        ~ ifelse(.x == "-", "", .x)
      ))
  })
  
  return(crop_tables)
}
