#' Create Plants That Feed the World indicator file with relevant fields
#'
#' This function reads input data, performs standardization, joins, selects relevant columns,
#' and writes the final output to an Excel file.
#'
#' @param ptftw_csv Path to the Plants That Feed the World indicator average CSV file.
#' @param crop_list_xlsx Path to the crop list Excel file.
#' @param output_xlsx Path for the output Excel file.
#' @return Invisibly, the final processed dataframe.
#' required packages: readr,  readxl, dplyr,  writexl
create_ptftw_indicator_file <- function(
  ptftw_csv = "Data/PlantsThatFeedTheWorld/indicator_average.csv",
  crop_list_xlsx = "Data_processing/Support_files/GCCS_selected_crops/crop_list_PG.xlsx",
  output_xlsx = "PTFTW_indicator_avg.xlsx"
) {
  
  # Read datasets
  PTFTW_indicator_average <- read_csv(ptftw_csv)
  croplist <- read_excel(crop_list_xlsx)
  
  # Standardize crop names
  PTFTW_indicator_average$crop[PTFTW_indicator_average$crop == "Rice (Asian)"] <- "Rice"
  PTFTW_indicator_average$crop[PTFTW_indicator_average$crop == "Chickpeas"] <- "Chickpea"
  
  # Subset relevant columns and drop rows with NA in PlantsthatFeedtheWorld_name
  PlantsthatFeedtheWorld_ourcrops <- croplist %>%
    select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>%
    filter(!is.na(PlantsthatFeedtheWorld_name) & PlantsthatFeedtheWorld_name != "NA")
  
  # Rename crop field in PTFTW dataset
  PTFTW_indicator_average <- PTFTW_indicator_average %>%
    rename(PlantsthatFeedtheWorld_name = crop)
  
  # Join our crops to PTFTW data
  PTFTW_indicator_average <- PlantsthatFeedtheWorld_ourcrops %>%
    left_join(PTFTW_indicator_average, by = "PlantsthatFeedtheWorld_name")
  
  # Select relevant fields for the indicator file
  relevant_fields <- c(
    "PlantsthatFeedtheWorld_name",
    "CropStrategy",
    "Genera_primary",
    "Taxa_main", 
    "crop_use-faostat-food_supply-fat_supply_quantity_g",
    "crop_use-faostat-food_supply-food_supply_kcal",
    "crop_use-faostat-food_supply-food_supply_quantity_g",
    "crop_use-faostat-food_supply-protein_supply_quantity_g",
    "crop_use-faostat-production-area_harvested_ha",
    "crop_use-faostat-production-gross_production_value_us",
    "crop_use-faostat-production-production_quantity_tonnes",
    "crop_use-faostat-trade-export_quantity_tonnes",
    "crop_use-faostat-trade-export_value_tonnes",
    "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
    "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
    "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
    "crop_use-public_interest-wikipedia_pageviews-taxon",
    "crop_use-research_significance-google_scholar-taxon",
    "crop_use-research_significance-pubmed_central-taxon",
    "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
    "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples",
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty",
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty",
    "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon",
    "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein"
  )
  PlantsThatFeedTheWorld_indicator_relevantfields <- PTFTW_indicator_average %>%
    select(all_of(relevant_fields))
  
  # Write to Excel
  write_xlsx(PlantsThatFeedTheWorld_indicator_relevantfields, output_xlsx)
  
  invisible(PlantsThatFeedTheWorld_indicator_relevantfields)
}
