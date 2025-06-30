# note check that name of variable is the correct one 

# --------- USER INPUT: Set file paths as necessary ---------
data_dir <- "your/data/directory/here"  # <-- Update this path as needed
setwd(data_dir)

# --------- PACKAGE SETUP ---------
required_packages <- c("readr", "readxl", "tidyverse", "openxlsx", "stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# --------- HELPER FUNCTIONS ---------
percent_summary <- function(df, group_col, count_expr, total_col, percent_col) {
  df %>%
    group_by({{group_col}}) %>%
    summarise(count = !!rlang::enquo(count_expr), total = n(), .groups = "drop") %>%
    mutate(percent = round((count / total) * 100, 2)) %>%
    rename({{total_col}} := total, {{percent_col}} := percent)
}

# --------- DATA IMPORT ---------
combined_allcrops <- read_csv("combined_allcrops.csv")
#combined_allcrops <- read_csv("C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/combined_df2_PG_30May2025.csv")
SGSV_allcrops <- read_csv("SGSV_allcrops.csv") # Note: Add SGSV_allcrops import if used
BGCI_allcrops <- read_csv("BGCI_allcrops.csv") # Note: Add BGCI_allcrops import if used

# --------- METRICS CALCULATIONS ---------

# 1. Total number of accessions by crop strategy
accession_by_crop_strategy <- combined_allcrops %>% count(cropstrategy, name = "accessions_count")
accession_by_source_crop_strategy <- combined_allcrops %>% count(datasource, cropstrategy, name = "accessions_count")

# 2. Unique institutions per crop strategy
unique_institutions <- combined_allcrops %>%
  group_by(cropstrategy) %>% summarise(unique_instcount = n_distinct(instcode), .groups = "drop")

# 3. Wild, Weedy, Landrace, Breeding, Improved, Other, No SAMPSTAT - % summaries
cwr_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT >= 100 & SAMPSTAT < 200, na.rm = TRUE), cwr_total_records, percent_100SAMPSTAT) %>%
  rename(count_100SAMPSTAT = count)
weedy_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 200, na.rm = TRUE), weedy_total_records, percent_200SAMPSTAT) %>%
  rename(count_200SAMPSTAT = count)
landrace_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 300, na.rm = TRUE), landrace_total_records, percent_300SAMPSTAT)%>%
  rename(count_300SAMPSTAT = count)
breeding_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT >= 400 & SAMPSTAT < 500, na.rm = TRUE), breedingmat_total_records, percent_400SAMPSTAT)%>%
  rename(count_400SAMPSTAT = count)
improved_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 500, na.rm = TRUE), improvedvar_total_records, percent_500SAMPSTAT)%>%
  rename(count_500SAMPSTAT = count)
othervar_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 999, na.rm = TRUE), othervar_total_records, percent_999SAMPSTAT)%>%
  rename(count_999SAMPSTAT = count)
no_SAMPSTAT_metric <- percent_summary(combined_allcrops, cropstrategy, sum(is.na(SAMPSTAT)), noSAMPSTAT_total_records, percent_na_SAMPSTAT)%>%
  rename(count_na_SAMPSTAT = count)

# 4. Unique taxa per crop strategy
unique_taxa <- combined_allcrops %>%
  select(cropstrategy, standardizedtaxa) %>%
  distinct() %>%
  group_by(cropstrategy) %>%
  summarise(
    unique_taxa = list(unique(standardizedtaxa)),
    unique_taxa_count = n_distinct(standardizedtaxa),
    .groups = "drop"
  )

# 5. Number of countries where germplasm collected (excluding certain SAMPSTAT)
country_count <- combined_allcrops %>%
  filter(!(SAMPSTAT %in% c(400:499, 500, 600))) %>%
  group_by(cropstrategy) %>%
  summarise(unique_countrycount = n_distinct(origcty), .groups = "drop")

# 6. Accessions from primary & secondary regions of diversity
primary_region_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  percent_summary(
    cropstrategy,
    sum(fromPrimary_diversity_region, na.rm = TRUE), 
    primaryregions_total_records,
    isinprimaryregion_perc
  )
secondary_region_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  percent_summary(
    cropstrategy,
    sum(fromSecondary_diversity_region, na.rm = TRUE), 
    secondaryregions_total_records,
    isinsecondaryregion_perc
  )

# 7. accessions by org type,  and MLS accessions for organization type
accessions_by_org_type <- combined_allcrops %>%
  group_by(cropstrategy, ORGANIZATIONTYPE) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percent = round(100 * n_records / sum(n_records), 2)
  )

mls_by_orgtype <- combined_allcrops %>%
  group_by(cropstrategy, ORGANIZATIONTYPE) %>% 
  summarise(
    count_includedmls = sum(MLSSTAT, na.rm = TRUE),
    count_notincludedmls = sum(!MLSSTAT, na.rm = TRUE),
    total = n(),
    percent_includedmls = round(100 * count_includedmls / total, 2),
    percent_notincludedmls = round(100 * count_notincludedmls / total, 2),
    .groups = "drop"
  )

# 8. Accessions in Annex I
annex1_count <- combined_allcrops %>%
  group_by(cropstrategy) %>%
  summarise(count_includedannex1 = sum(ANNEX1, na.rm = TRUE), .groups = "drop") 
annex1_perc <- combined_allcrops %>%
  group_by(cropstrategy) %>%
  summarise(
    count_includedannex1 = sum(ANNEX1, na.rm = TRUE),
    annex1_total_records = n(),
    .groups = "drop"
  ) %>%
  mutate(annex1_perc = round((count_includedannex1 / annex1_total_records) * 100, 2))

# 9. Storage type metrics (all, and by term)
storage_types <- list(
  seed      = "10|11|12|13",
  field     = "20",
  invitro   = "30",
  cryo      = "40",
  dna       = "50",
  other     = "99"
)
storage_summary <- combined_allcrops %>%
  group_by(cropstrategy) %>%
  summarise(
    total_records = n(),
    seed_count   = sum(str_detect(STORAGE, storage_types$seed), na.rm = TRUE),
    field_count  = sum(str_detect(STORAGE, storage_types$field), na.rm = TRUE),
    invitro_count = sum(str_detect(STORAGE, storage_types$invitro), na.rm = TRUE),
    cryo_count   = sum(str_detect(STORAGE, storage_types$cryo), na.rm = TRUE),
    dna_count    = sum(str_detect(STORAGE, storage_types$dna), na.rm = TRUE),
    other_count  = sum(str_detect(STORAGE, storage_types$other), na.rm = TRUE),
    nostorage_count = sum(is.na(STORAGE))
  ) %>%
  mutate(
    seed_perc      = round(100 * seed_count / total_records, 2),
    field_perc     = round(100 * field_count / total_records, 2),
    invitro_perc   = round(100 * invitro_count / total_records, 2),
    cryo_perc      = round(100 * cryo_count / total_records, 2),
    dna_perc       = round(100 * dna_count / total_records, 2),
    other_perc     = round(100 * other_count / total_records, 2),
    nostorage_perc = round(100 * nostorage_count / total_records, 2)
  )

storage_term_summary <- combined_allcrops %>%
  group_by(cropstrategy) %>%
  summarise(
    total_records = n(),
    longterm_storage_count  = sum(str_detect(STORAGE, "^13$"), na.rm = TRUE),
    medterm_storage_count   = sum(str_detect(STORAGE, "^12$"), na.rm = TRUE),
    shortterm_storage_count = sum(str_detect(STORAGE, "^11$"), na.rm = TRUE)
  ) %>%
  mutate(
    longterm_storage_perc   = round(100 * longterm_storage_count / total_records, 2),
    medterm_storage_perc    = round(100 * medterm_storage_count / total_records, 2),
    shortterm_storage_perc  = round(100 * shortterm_storage_count / total_records, 2)
  )

# 10. Safety duplication - add your code here once data is available
# SG note for writing script: degree of duplication SG Github script location
# https://github.com/slgora/GCCS-Metrics/blob/main/GCCS-Metrics_PDCI_PGscript.R 
# add data prep to 4_Estimate_metrics.R before metric calc or keep as a separate script in new repo???

# 2 metrics from SD calc: by genus and by instcode (genebank)
# by_genus_genesys
# by_genebank_genesys

# SG: Number of accessions safety duplicated metric (based on combined dataset)
safetydupl_metric <- combined_allcrops %>%
  group_by(cropstrategy) %>%
  summarise(
    safduplsite_count = sum(!is.na(duplsite), na.rm = TRUE), #duplSite
    safduplsite_total_records = n(),
    .groups = "drop" ) %>%
  mutate(safduplsite_perc = round((safduplsite_count / safduplsite_total_records) * 100, 2)
  )

# 11. SGSV duplicates (if applicable)
SGSV_allcrops <- read_csv("sgsv_data_processed.csv") 
SGSV_dupl_count <- SGSV_allcrops %>% group_by(cropstrategy) %>% summarise(sgsvcount = n(), .groups = "drop")

#SG: Percent SGSV duplicates
SGSV_dupl_perc <- SGSV_dupl_metric %>%
  left_join(count(combined_allcrops, cropstrategy, name = "total_count"), 
            by = "cropstrategy") %>%   #Calculate all accessions of crop in combined_allcrops
   mutate(sgsv_dupl_perc = round(sgsv_dupl_count / total_count * 100, 2)) %>%
   select(cropstrategy, sgsv_dupl_count, total_count, sgsv_dupl_perc)

# 12. GLIS: # of accessions with DOIs per crop, use data downloaded from GLIS (GLIS_dataset)
GLIS_dataset <- read_csv("glis_data_processed.csv") # glis_data_processed is the data after adding the cropstrategy variable
GLIS_dois_count <- GLIS_dataset %>% group_by(cropstrategy) %>% summarise(dois = sum(DOI, na.rm = TRUE), .groups = "drop")

# 13. GLIS: # of accessions notified as incuded in MLS (based on GLIS dataset)
GLIS_MLS_count <- GLIS_dataset %>% group_by(cropstrategy) %>% summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")

# 14. SG: Top institutions holding crop germplasm
institution_accessions_summary <- combined_allcrops %>%
   filter(!is.na(INSTCODE)) %>%
   group_by(cropstrategy, INSTCODE, instName) %>%
   summarise(institution_accessions_count = n(), .groups = "drop") %>%
   mutate(total_accessions = sum(accessions, na.rm = TRUE),
          institution_accessions_perc = round((accessions / total_accessions) * 100, 2))

# 15. SG: Number of unique taxa listed in BGCI data metric (BGCI datset)
BGCI_taxa_count <- BGCI_data %>%
   select(cropstrategy, taxa_standardized) %>%
   filter(!is.na(taxa_standardized)) %>%
   distinct() %>%
   group_by(cropstrategy) %>%
   summarise(unique_taxa_count = n_distinct(taxa_standardized), .groups = "drop")

# 16. SG: Number of unique institutions holding crop germplasm (BGCI dataset)
BGCI_inst_count <- BGCI_data %>%
   select(cropstrategy, Ex_situ_Site_GardenSearch_ID) %>%
   filter(!is.na(Ex_situ_Site_GardenSearch_ID)) %>%
   distinct() %>% # Ensure unique institution entries
   group_by(cropstrategy) %>%
   summarise(unique_inst_count = n_distinct(Ex_situ_Site_GardenSearch_ID), .groups = "drop")

# 17. SG: Regeneration metrics (based on WIEWS indicator file)
# SG note for writing script: # data prep of WIEWS indicator 22 file in this script: 
# https://github.com/slgora/GCCS-Metrics/blob/main/GCCS-Metrics_WIEWS_Indicator_Filter-ourCrops.R
# add data prep to 4_Estimate_metrics.R before metric calc or keep as a separate script in new repo???
WIEWS_indicator_ourcrops <- read_excel("C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/WIEWS_indicator_ourcrops_2025-06-16.xlsx")
WIEWS_regeneration_summary <- WIEWS_indicator_ourcrops

# 18. SG: PDCI metric
# SG note for writing script: PDCI calculation in this script:
# https://github.com/slgora/GCCS-Metrics/blob/main/GCCS-Metrics_PDCI_PGscript.R
# add data prep to 4_Estimate_metrics.R before metric calc or keep as a separate script in new repo???

# Metric we want to extract is calculated at end of PDCI_script.R
summary_pdci <- df %>% 
  group_by(cropStrategy) %>%
  summarise(
    median_PDCI = median(PDCI, na.rm = TRUE) #median PDCI
  )

### 19. SG: PTFTW Metrics
# read in Plants that Feed the World indicator file that has been filtered by our crops
PTFTW_indicator_avg_ourCrops <- read_excel("PTFTW_indicator_ourcrops.xlsx")

PTFTW_indicator_avg_ourCrops <- read_excel("C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/PTFTW_indicator_ourcrops_2025-06-23.xlsx")

# Define columns to sum vs. average across genera in crops (if necessary)
sum_cols <- c("supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene", 
  "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
  "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide", 
  "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein", 
  "supply-research_supply-research_supply_gbif-research_supply_gbif_taxon", 
  "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
  "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples", 
  "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty", 
  "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon", 
  "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon", 
  "crop_use-faostat-production-area_harvested_ha", 
  "crop_use-faostat-production-gross_production_value_us", 
  "crop_use-faostat-production-production_quantity_tonnes", 
  "crop_use-faostat-trade-export_quantity_tonnes",  
  "crop_use-faostat-food_supply-fat_supply_quantity_g",
  "crop_use-faostat-trade-export_value_tonnes" )

avg_cols <- c("crop_use-faostat-food_supply-food_supply_kcal",
  "crop_use-faostat-food_supply-food_supply_quantity_g",
  "crop_use-faostat-food_supply-protein_supply_quantity_g",
  #"crop_use-faostat-trade-import_quantity_tonnes",       # need to add these metrics in previous step
  #"crop_use-faostat-trade-import_value_tonnes",       
  "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal", # added
  "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
  "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
  "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
  "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
  "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
  "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
  "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
  "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
  "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
  "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
  "crop_use-public_interest-wikipedia_pageviews-taxon",
  "crop_use-research_significance-google_scholar-taxon",
  "crop_use-research_significance-pubmed_central-taxon",
  "crop_use-faostat_equality_of_use-gini_food_supply-food_supply_kcal",        # added  gini index metrics, confirm avg and not recalc gini index
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
  "interdependence-faostat-food_supply-food_supply_kcal",          # added interdependence metrics
  "interdependence-faostat-food_supply-protein_supply_quantity_g",
  "interdependence-faostat-food_supply-fat_supply_quantity_g",
  "interdependence-faostat-food_supply-food_supply_quantity_g",
  "interdependence-faostat-production-area_harvested_ha",
  "interdependence-faostat-production-production_quantity_tonnes",
  "interdependence-faostat-production-gross_production_value_us",
  "interdependence-faostat-trade-export_quantity_tonnes",
  "interdependence-faostat-trade-export_value_tonnes",
  "interdependence-faostat-trade-import_quantity_tonnes",
  "interdependence-faostat-trade-import_value_tonnes" )                        

PTFTW_metrics <- PTFTW_indicator_avg_ourCrops %>%
  rename(
    cropstrategy = CropStrategy,
    PTFTW_name   = PlantsthatFeedtheWorld_name,
    genus        = Genera_primary,
    fullTaxa     = Taxa_main) %>%
  group_by(cropstrategy) %>%
  summarise( across(all_of(sum_cols), sum, na.rm = TRUE),
             across(all_of(avg_cols), mean, na.rm = TRUE),
             .groups = "drop" )
# save 
write_xlsx(PTFTW_metrics,"PTFTW_metrics2025_06_27.xlsx")  #add processing date


## 20. gini metric calculations (3 metrics)
# import in the gini metrics, completed in metrics_transfer.R file
transfers_metrics <- read_excel("transfers_metrics_2015_2021_06-25-2025.xlsx")


# 21. Count of records in GBIF
source('Functions/Call_gbif_API.R')   # Import function get_gbif_count
croplist <- read_csv("croplist.csv")  # Load croplist for genera list

# Run GBIF count of occurrences for each genus and synonyms
results <- croplist %>%                 
  rowwise() %>%
  mutate(
    count_primary = get_gbif_count(Genera_primary),
    count_synonym = get_gbif_count(Genera_synonyms),
    GBIF_count_total = sum(count_primary, count_synonym, na.rm = TRUE)
  ) %>%
  ungroup()

# Summarize by CropStrategy
summary_gbif_count <- results %>%
  group_by(CropStrategy) %>%
  summarise(total_GBIF_count = sum(GBIF_count_total, na.rm = TRUE), .groups = "drop")




### 22. Characterization and Evaluation datasets
# example count for Lentil


##3 delete didnt work
## and delete the lentil datasets metadata and the lentil datasets folder
library(httr)

# URL for metadata export of Lentil datasets (search results)
csv_url <- "https://www.genesys-pgr.org/datasets?q=Lentil&format=csv"

# Destination file
dest_file <- "lentil_datasets_metadata.csv"

# Download the CSV
download.file(csv_url, destfile = dest_file, mode = "wb")

cat("Metadata downloaded to:", dest_file, "\n")



# --------- END OF SCRIPT ---------


