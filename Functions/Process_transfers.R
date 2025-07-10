# Required Libraries
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(writexl)
library(ineq)

#' Calculate Treaty germplasm distribution metrics for selected crops
#'
#' This function combines ITPGRFA transfer records from 2015–2021 and computes:
#' - Average number of distributed samples per year
#' - Average number of recipient countries per year
#' - Gini index of sample distribution across regions
#'
#' @param croplist_file Path to selected crops Excel file
#' @param out_path Path where the output Excel file should be saved
#' @return A data frame with Treaty germplasm distribution metrics per crop strategy
transfers_metrics <- function(croplist_file, out_path) {
  # Embedded input files
  transfers_2012_2019 <- read_excel("../../Data/Plant_Treaty/Data_store/ITPGRFA_MLSDataStore2022_7_1.xlsx")
  transfers_2019_2021 <- read_excel("../../Data/Plant_Treaty/Data_store/Transfers_ourcrops_2019-2021.xlsx")
  countries_regions <- read_excel("../../Data_processing/Support_files/Geographical/countries_in_regions.xlsx")
  
  # Clean 2015–2018 data
  transfers_2015_2018 <- transfers_2012_2019 %>%
    filter(Year >= 2015 & Year <= 2018 & Dataset != "CGIAR only") %>%
    select(`ISO3...2`, `Provider country`, Crop_cleaned, taxa, Year, Samples, `ISO3...15`, `Recipient country`) %>%
    rename(
      provider_ISO3 = `ISO3...2`,
      provider_country_name = `Provider country`,
      PlantsthatFeedtheWorld_name = Crop_cleaned,
      Taxonomic_name = taxa,
      year = Year,
      number_of_samples = Samples,
      recipient_ISO3 = `ISO3...15`,
      recipient_country_name = `Recipient country`
    )
  
  # Load and filter croplist
  croplist <- read_excel(croplist_file) %>%
    select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>%
    filter(PlantsthatFeedtheWorld_name != "NA") %>%
    distinct(PlantsthatFeedtheWorld_name, .keep_all = TRUE)
  
  # Join croplist to transfers
  transfers_2015_2018 <- left_join(croplist, transfers_2015_2018, by = "PlantsthatFeedtheWorld_name") %>%
    select(CropStrategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
    filter(!is.na(number_of_samples))
  
  # Rename to ensure consistency in output
  transfers_2015_2018 <- transfers_2015_2018 %>%
    rename(Crop_strategy = CropStrategy)
  
  # Clean 2019–2021 transfers
  transfers_2019_2021 <- transfers_2019_2021 %>%
    select(crop_strategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
    rename(Crop_strategy = crop_strategy)
  
  # Combine all transfers
  transfers_all <- bind_rows(transfers_2019_2021, transfers_2015_2018)
  
  # Metric 1: Average samples per year
  avg_samples <- transfers_all %>%
    group_by(Crop_strategy, year) %>%
    summarise(avg_number_of_samples = mean(number_of_samples), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(avg_number_of_samples_per_year = mean(avg_number_of_samples), .groups = "drop")
  
  # Metric 2: Average recipient country count
  avg_recipients <- transfers_all %>%
    group_by(Crop_strategy, year) %>%
    summarise(recipient_count = n_distinct(recipient_ISO3), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(avg_number_of_recipient_countries = mean(recipient_count), .groups = "drop")
  
  # Metric 3: Gini index for regional distribution
  transfers_with_regions <- left_join(transfers_all, countries_regions, by = c("recipient_ISO3" = "Country_code")) %>%
    separate_rows(PlantsThatFeedTheWorld_Region_new, sep = ",") %>%
    mutate(regions = str_trim(PlantsThatFeedTheWorld_Region_new))
  
  gini_results <- transfers_with_regions %>%
    group_by(Crop_strategy, regions) %>%
    summarise(total_samples = sum(number_of_samples, na.rm = TRUE), .groups = "drop") %>%
    group_by(Crop_strategy) %>%
    summarise(gini_index = Gini(total_samples), .groups = "drop")
  
  # Combine all metrics
  treaty_metrics <- avg_samples %>%
    left_join(avg_recipients, by = "Crop_strategy") %>%
    left_join(gini_results, by = "Crop_strategy") %>%
    rename(
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty" = avg_number_of_samples_per_year,
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty" = avg_number_of_recipient_countries,
      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty" = gini_index
    )
  
  write_xlsx(treaty_metrics, out_path)
  return(treaty_metrics)
}
