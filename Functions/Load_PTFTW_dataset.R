# Load required libraries
library(dplyr)
library(readr)
library(readxl)
library(writexl)

#' Process PTFTW indicator metrics for our crops
#'
#' Aggregates digital, trade, food supply, and research indicators for selected crops.
#' Metrics include sum, average, and max values, grouped by Crop_strategy.
#'
#' @param indicator_file Path to PTFTW indicator CSV
#' @param croplist Path to croplist Excel file
#' @param out_path Optional path to save the output Excel file
#' @return A data frame of aggregated indicators grouped by Crop_strategy
process_PTFTW_metrics <- function(indicator_file, croplist, out_path = NULL) {
  # Read inputs
  PTFTW_indicator <- read_csv(indicator_file)
  croplist_df <- read_excel(croplist)
  
  PTFTW_indicator <- PTFTW_indicator %>%
    rename(PlantsthatFeedtheWorld_name = crop) %>%
    mutate(PlantsthatFeedtheWorld_name = trimws(PlantsthatFeedtheWorld_name))
  
  # Join and clean
  PTFTW_clean <- croplist_df %>%
    filter(PlantsthatFeedtheWorld_name != "NA") %>%
    left_join(PTFTW_indicator, by = "PlantsthatFeedtheWorld_name") %>%
    rename(cropstrategy = CropStrategy,
           PTFTW_name   = PlantsthatFeedtheWorld_name,
           genus        = Genera_primary,
           fullTaxa     = Taxa_main)
  
  # Define columns
  sum_cols <- c(
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide",
    "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein",
    "supply-research_supply-research_supply_gbif-research_supply_gbif_taxon",
    "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
    "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples",
    "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon",
    "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon",
    "crop_use-faostat-production-area_harvested_ha",
    "crop_use-faostat-production-gross_production_value_us",
    "crop_use-faostat-production-production_quantity_tonnes",
    "crop_use-faostat-trade-export_quantity_tonnes",
    "crop_use-faostat-trade-export_value_tonnes",
    "crop_use-faostat-trade-import_quantity_tonnes",
    "crop_use-faostat-trade-import_value_tonnes",
    "crop_use-faostat-food_supply-fat_supply_quantity_g",
    "crop_use-faostat-food_supply-food_supply_kcal",
    "crop_use-faostat-food_supply-food_supply_quantity_g",
    "crop_use-faostat-food_supply-protein_supply_quantity_g",
    "crop_use-public_interest-wikipedia_pageviews-taxon",
    "crop_use-research_significance-google_scholar-taxon",
    "crop_use-research_significance-pubmed_central-taxon"
  )
  
  avg_cols <- c(
    "crop_use-faostat_equality_of_use-gini_food_supply-food_supply_kcal",
    "crop_use-faostat_equality_of_use-gini_food_supply-protein_supply_quantity_g",
    "crop_use-faostat_equality_of_use-gini_food_supply-fat_supply_quantity_g",
    "crop_use-faostat_equality_of_use-gini_food_supply-food_supply_quantity_g",
    "crop_use-faostat_equality_of_use-gini_production-area_harvested_ha",
    "crop_use-faostat_equality_of_use-gini_production-production_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_production-gross_production_value_us",
    "crop_use-faostat_equality_of_use-gini_trade-export_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-export_value_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-import_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-import_value_tonnes",
    "interdependence-faostat-food_supply-food_supply_kcal",
    "interdependence-faostat-food_supply-protein_supply_quantity_g",
    "interdependence-faostat-food_supply-fat_supply_quantity_g",
    "interdependence-faostat-food_supply-food_supply_quantity_g",
    "interdependence-faostat-production-area_harvested_ha",
    "interdependence-faostat-production-production_quantity_tonnes",
    "interdependence-faostat-production-gross_production_value_us",
    "interdependence-faostat-trade-export_quantity_tonnes",
    "interdependence-faostat-trade-export_value_tonnes",
    "interdependence-faostat-trade-import_quantity_tonnes",
    "interdependence-faostat-trade-import_value_tonnes"
  )
  
  max_cols <- c(
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
    "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
    "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
    "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes"
  )
  
  # Summarise safely and rename final output column
  PTFTW_metrics <- PTFTW_clean %>%
    group_by(cropstrategy) %>%
    summarise(
      across(all_of(sum_cols), ~sum(.x, na.rm = TRUE)),
      across(all_of(avg_cols), ~mean(.x, na.rm = TRUE)),
      across(all_of(max_cols), ~if (all(is.na(.x))) NA else max(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(everything(), ~ifelse(is.infinite(.x), NA, .x))) %>%
    rename(Crop_strategy = cropstrategy)
  
  if (!is.null(out_path)) {
    write_xlsx(PTFTW_metrics, out_path)
  }
  
  return(PTFTW_metrics)
}

