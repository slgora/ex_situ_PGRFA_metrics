# Load and process FAO WIEWS indicator file
# read in paths to WIEWS indicator 22 file and croplist_PG
process_wiews_indicator_data <- function(
    # update file paths as needed
    wiews_path = "../../Data/FAO_WIEWS/Indicator_22_data/FAO_WIEWS_Indicator22.xlsx",
    # update file paths as needed
    croplist_path = "../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx"
) {
  library(readxl)
  library(dplyr)
  library(stringr)
  
  # Load datasets
  WIEWS_Indicator22 <- read_excel(wiews_path)
  croplist <- read_excel(croplist_path)
  
  # Select relevant columns
  WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
    select(
      "Crop/crop group name",
      "Total number of accessions in the national genebank(s)",
      "Number of accessions regenerated and/or multiplied",
      "Number of accessions in need of regeneration",
      "Number of accessions in need of regeneration without a budget for regeneration"
    )
  
  # Match crop strategy
  find_crop_strategy <- function(common_name, croplist) {
    match_row <- croplist %>%
      filter(
        str_detect(CropStrategy, fixed(common_name, ignore_case = TRUE)) |
          str_detect(CommonName_primary, fixed(common_name, ignore_case = TRUE)) |
          str_detect(CommonName_synonym, fixed(common_name, ignore_case = TRUE)) |
          str_detect(Genera_primary, fixed(common_name, ignore_case = TRUE)) |
          str_detect(Genera_synonyms, fixed(common_name, ignore_case = TRUE)) |
          str_detect(Taxa_main, fixed(common_name, ignore_case = TRUE))
      )
    if (nrow(match_row) > 0) match_row$CropStrategy[1] else NA
  }
  
  # Apply and rename strategy field
  WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
    mutate(Crop_strategy = sapply(`Crop/crop group name`, find_crop_strategy, croplist = croplist)) %>%
    filter(!is.na(Crop_strategy) & Crop_strategy != "")
  
  # Summarize and finalize
  WIEWS_indicator <- WIEWS_Indicator22 %>%
    group_by(Crop_strategy) %>%
    summarise(
      "Crop/crop group name" = paste(unique(na.omit(`Crop/crop group name`)), collapse = "; "),
      across(where(is.numeric), ~ ifelse(all(is.na(.x)), NA, sum(.x, na.rm = TRUE))),
      .groups = "drop"
    ) %>%
    select(-"Crop/crop group name", -"Total number of accessions in the national genebank(s)") %>%
    rename(
      number_of_accessions_regenerated_and_or_multiplied = "Number of accessions regenerated and/or multiplied",
      number_of_accessions_in_need_of_regeneration = "Number of accessions in need of regeneration",
      number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration = "Number of accessions in need of regeneration without a budget for regeneration"
    )
  
  return(WIEWS_indicator)
}
