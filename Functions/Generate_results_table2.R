# -------------------------------------------------------------------------------------------
# Function: generate_table2
#
# Description:
# Creates crop-specific institutional accession tables modeled on Table 2 layout.
# Each crop strategy returns its own data frame, ready for manual export or editing.
# Placeholder fields are left blank for manual input (storage type and MLS registration).
#
# Input:
# - institution_accessions_summary: Data frame with crop-level institutional accession counts
#
# Output:
# - A named list of formatted data frames, one per crop strategy
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
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
        `Institution Name` = "",
        `Number of accessions` = institution_accessions_count,
        `Long term storage (-18–20 °C) and source` = "",
        `Cumulative %` = round(cumsum(institution_accessions_count) / total_accessions * 100, 2),
        `Number of accessions included in MLS (from GLIS)` = ""
      ) %>%
      select(
        `Institution Code`,
        `Institution Name`,
        `Number of accessions`,
        `Long term storage (-18–20 °C) and source`,
        `Cumulative %`,
        `Number of accessions included in MLS (from GLIS)`
      )
  })
  
  return(crop_tables)
}
