# -------------------------------------------------------------------------------------------
# Function: generate_table2
#
# Description:
# Creates crop-specific institutional accession tables modeled on Table 2 layout.
# Each crop strategy returns its own data frame, ready for export or editing.
# Uses existing Institute_name field for institution names.
#
# Input:
# - institution_accessions_summary: Data frame with crop-level institutional accession counts
#
# Output:
# - A named list of formatted data frames, one per crop strategy
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
  library(dplyr)
  library(purrr)
  
  # Split by crop strategy and sort by accession count within each group
  crop_tables <- institution_accessions_summary %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy)))
  
  # Format each sheet
  crop_tables <- purrr::map(crop_tables, function(df) {
    df %>%
      mutate(
        `Institution Code` = INSTCODE,
        `Institution Name` = Institute_name,
        `Number of accessions` = institution_accessions_count,
        `Cumulative %` = round(cumsum(institution_accessions_count) / total_accessions * 100, 2)
      ) %>%
      select(
        `Institution Code`,
        `Institution Name`,
        `Number of accessions`,
        `Cumulative %`
      )
  })
  
  return(crop_tables)
}
