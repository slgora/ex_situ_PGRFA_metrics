# Load and process FAO WIEWS indicator file
# read in paths to WIEWS indicator file and croplist
process_wiews_indicator_data <- function(
    # update file paths as needed
  wiews_path = "../../GCCSmetricsI/Data/FAO_WIEWS/Indicator_22_data/Wiews_Indicator_1733321439198_regenerations2014-2019.xlsx",
  # update file paths as needed
  croplist_path = "../../GCCSmetricsI/Data_processing/Support_files/GCCS_Selected_crops/CropList_final.xlsx"
) {
  library(readxl)
  library(dplyr)
  library(stringr)
  
  # Load datasets
  WIEWS_Indicator22 <- read_excel(wiews_path)
  croplist <- read_excel(croplist_path)
  
  # Ensure necessary fields exist (Crop_clean and GCCS)
  if (!"Crop_clean" %in% colnames(WIEWS_Indicator22)) {
    stop("Field 'Crop_clean' not found in the WIEWS indicator file.")
  }
  if (!"GCCS" %in% colnames(WIEWS_Indicator22)) {
    stop("Field 'GCCS' not found in the WIEWS indicator file.")
  }
  
  # Select relevant columns
  WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
    select(
      "Crop_clean",
      "GCCS",
      "Total number of accessions in the national genebank(s)",
      "Number of accessions regenerated and/or multiplied",
      "Number of accessions in need of regeneration",
      "Number of accessions in need of regeneration without a budget for regeneration"
    )
  
  # Match crop strategy only when GCCS = "Y"
  find_crop_strategy <- function(crop_clean, gccs_field, croplist) {
    if (is.na(gccs_field)) {
      return(NA)  # Handle NA values in GCCS
    }
    if (gccs_field == "Y") {
      match_row <- croplist %>%
        filter(
          str_detect(CropStrategy, fixed(crop_clean, ignore_case = TRUE))
        )
      if (nrow(match_row) > 0) match_row$CropStrategy[1] else NA
    } else {
      return(NA)
    }
  }
  
  # Apply and rename strategy field
  WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
    mutate(Crop_strategy = mapply(
      find_crop_strategy,
      crop_clean = Crop_clean,
      gccs_field = GCCS,
      MoreArgs = list(croplist = croplist)
    )) %>%
    filter(!is.na(Crop_strategy) & Crop_strategy != "")
  
  # Summarize, round sums, calculate percentages, and finalize
  WIEWS_indicator <- WIEWS_Indicator22 %>%
    group_by(Crop_strategy) %>%
    summarise(
      number_of_accessions_in_national_genebanks = round(
        sum(`Total number of accessions in the national genebank(s)`, na.rm = TRUE), 1
      ),
      number_of_accessions_regenerated_and_or_multiplied = round(
        sum(`Number of accessions regenerated and/or multiplied`, na.rm = TRUE), 1
      ),
      number_of_accessions_in_need_of_regeneration = round(
        sum(`Number of accessions in need of regeneration`, na.rm = TRUE), 1
      ),
      number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration = round(
        sum(`Number of accessions in need of regeneration without a budget for regeneration`, na.rm = TRUE), 1
      ),
      .groups = "drop"
    ) %>%
    mutate(
      percent_of_accessions_regenerated_and_or_multiplied = ifelse(
        number_of_accessions_in_national_genebanks > 0,
        formatC(
          round((number_of_accessions_regenerated_and_or_multiplied / number_of_accessions_in_national_genebanks) * 100, 1),
          format = "f", digits = 1
        ),
        NA
      ),
      percent_of_accessions_in_need_of_regeneration = ifelse(
        number_of_accessions_in_national_genebanks > 0,
        formatC(
          round((number_of_accessions_in_need_of_regeneration / number_of_accessions_in_national_genebanks) * 100, 1),
          format = "f", digits = 1
        ),
        NA
      ),
      percent_of_accessions_in_need_of_regeneration_without_budget_for_regeneration = ifelse(
        number_of_accessions_in_national_genebanks > 0,
        formatC(
          round((number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration / number_of_accessions_in_national_genebanks) * 100, 1),
          format = "f", digits = 1
        ),
        NA
      )
    ) %>%
    select(
      Crop_strategy,
      number_of_accessions_in_national_genebanks,
      number_of_accessions_regenerated_and_or_multiplied,
      percent_of_accessions_regenerated_and_or_multiplied,
      number_of_accessions_in_need_of_regeneration,
      percent_of_accessions_in_need_of_regeneration,
      number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration,
      percent_of_accessions_in_need_of_regeneration_without_budget_for_regeneration
    )
  
  return(WIEWS_indicator)
}
