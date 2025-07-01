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

# 6.a and 6.b Accessions from primary & secondary regions of diversity
primary_region_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  percent_summary(
    cropstrategy,
    sum(fromPrimary_diversity_region, na.rm = TRUE), 
    isinprimaryregions_count,
    isinprimaryregion_perc
  )

# 6.c and 6.d Diversity_regions_metric (includes accessions from primary and secondary regions of diversity)
diversity_regions_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  group_by(cropstrategy) %>%
  summarise(
    isindiversityregions_count = sum(fromPrimary_diversity_region, na.rm = TRUE) + 
                             sum(fromSecondary_diversity_region, na.rm = TRUE),
    total_accessions = n(),
    isindiversityregions_perc = round(100 * isindiversityregions_count / total_accessions, 2),
    .groups = "drop"
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

# 10. Safety duplication

# source functions 
source(Functions/SD_duplicates_out_country.R)    # sg note: source individually or document functions all-together in one file?
source(Functions/SD_duplicates_in_country.R)
source(Functions/SD_duplicates_SDGV.R)
source(Functions/SD_duplicates_only_in_country.R)
source(Functions/SD_safety_duplication_complete.R)

### note, before running the sd analysis you need to drop genebanks that only old safety duplicates 
### such as NOR051 and BRA003 from the datasets

#### SG note, need to confirm that SD analysis is run on Genesys dataset not on the gen_wiews_df

# Read dataset 
gen<- read.csv("Genesys_allcrops.csv", header = TRUE )        # update path to drive location
# keep fields needed and change names according to functions used 
gen <- subset(gen, select = c(INSTCODE, ACCENUMB, GENUS, SPECIES, DUPLSITE))
colnames(gen) <- c("instCode","acceNumb","genus","species","duplSite")

# Prepare dataset 
gen$instCode <- as.character(gen$instCode)
gen$acceNumb <- as.character(gen$acceNumb)
gen$ID <- paste0(gen$instCode, gen$acceNumb)
gen$duplSite <- as.character(gen$duplSite) # added while debugging
gen  <- gen  %>% mutate(duplSite = str_replace_all(duplSite, " ", "")) # added while debugging because found some duplsite entries with whitespaces
gen2 <- gen %>% distinct(ID, .keep_all = TRUE) # maybe whitespaces should be eliminated from ID before this
gen2 <- gen2 %>% mutate(holding_country = substr(instCode, 1, 3))

# Use value in duplSite to create a list with each institute code as item
gen2 <- gen2 %>% mutate(duplSite_LIST = strsplit(ifelse(is.na(duplSite), '', duplSite), ';'))

# Run function to calculate metric, SD by genus
sd_by_genus_genesys <- safety_duplication_complete(gen2, groupby = 'genus')

# Run function to calculate metric, SD by instCode (i.e. genebank)
sd_by_genebank_genesys <- safety_duplication_complete(gen2, groupby = 'instCode')

# Save results
sd_by_genus_genesys <- apply(sd_by_genus_genesys,2,as.character)  
write.csv(sd_by_genus_genesys, 'genesys_sd_results_by_crop.csv', row.names = FALSE) # write ouput to Drive folder
sd_by_genebank_genesys <- apply(sd_by_genebank_genesys,2,as.character)
write.csv(sd_by_genebank_genesys, 'genesys_sd_results_by_genebanks.csv', row.names = FALSE) # write ouput to Drive folder




# Number of accessions safety duplicated metric (based on combined dataset)
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
# Data read in: Wiews indicator 22 file and croplist
WIEWS_Indicator22 <- "Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_Indicator22_regeneration_allcrops.csv"
croplist <- read_csv("Data_processing/Support_files/GCCS_Selected_Crops/croplist_PG.csv")  # Load croplist for genera list

# Select relevant columns 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
  select("Crop/crop group name",
         # "Country (ISO3)",  # use this for calc later
         # "Stakeholder (Instcode)", # use this for calc later
         "Total number of accessions in the national genebank(s)",
         "Number of accessions regenerated and/or multiplied",
         "Number of accessions in need of regeneration",
         "Number of accessions in need of regeneration without a budget for regeneration")

# Function to check for crop matches across multiple columns 
find_crop_strategy <- function(common_name, croplist) { match_row <- croplist %>% # source function later if we use this function
  filter(str_detect(CropStrategy, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(CommonName_primary, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(CommonName_synonym, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Genera_primary, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Genera_synonyms, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Taxa_main, fixed(common_name, ignore_case = TRUE))) 
if (nrow(match_row) > 0) { 
  return(match_row$CropStrategy[1]) 
} else { 
  return(NA) 
} 
}    

# Use function to check for crop and add CropStrategy to WIEWS_Indicator22 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>% 
  mutate(cropStrategy = sapply(`Crop/crop group name`, find_crop_strategy, croplist = croplist))

# Keep rows where CropStrategy is filled out, ~ 313 rows 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>% filter(!is.na(cropStrategy) & cropStrategy != "")
# sum data by crop (some rows have multiple entries per crop)
# combine name field as a list and separate by a semicolon: "Crop/crop group name"
# sum the numbers across crops
# removed irrelevant columns and renamed relevant columns to standardize
WIEWS_Indicator22_ourCrops <- WIEWS_Indicator22 %>% 
  group_by(cropStrategy) %>% 
  summarise("Crop/crop group name" = paste(unique(na.omit(`Crop/crop group name`)), collapse = "; "), 
            across(where(is.numeric), ~ ifelse(all(is.na(.x)), NA, sum(.x, na.rm = TRUE)))) %>%
  select(-"Crop/crop group name") %>%  #drop WIEWS crop name column
  select (-"Total number of accessions in the national genebank(s)") %>%  #drop column, have more up to date data on this
  rename( number_of_accessions_regenerated_and_or_multiplied = "Number of accessions regenerated and/or multiplied",
          number_of_accessions_in_need_of_regeneration= "Number of accessions in need of regeneration",
          number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration= "Number of accessions in need of regeneration without a budget for regeneration") 

# save file
write_xlsx(WIEWS_Indicator22_ourCrops, "Data_processing/3_Post_taxa_standardization/Resulting_datasets/WIEWS_indicator_ourcrops2025_06_16.xlsx")
# extract metrics
WIEWS_regeneration_summary <- WIEWS_Indicator22_ourCrops

# 18. PDCI metric
# load data
df <- combined_allcrops   # update name combined_allcrops to gen_wiews_df
df$sampStat <- as.numeric(as.character(df$sampStat)) #convert sampStat to numeric

# Source and Run function to calculate PDCI
source(Functions/Get_PDCI.R)
df <- get_PDCI(df)

# Extract median PDCI
summary_pdci <- df %>% 
  group_by(cropStrategy) %>%
  summarise(
    median_PDCI = median(PDCI, na.rm = TRUE) #median PDCI
  )

### 19. SG: PTFTW Metrics
## Data read in: Plants That Feed the World dataset
PTFTW_indicator_average <- "Data/Plants_that_feed_the_world/Indicators/indicator_average.csv"

# replace "Rice (Asian)" in crop field to be "Rice"
PTFTW_indicator_average['crop'][PTFTW_indicator_average['crop'] == "Rice (Asian)"] <- "Rice"
# replace "Chickpeas" in crop field to be "Chickpea"
PTFTW_indicator_average['crop'][PTFTW_indicator_average['crop'] == "Chickpeas"] <- "Chickpea"

# filter by our crops, Subset relevant columns and drop rows with NA in PlantsthatFeedtheWorld_name 
PlantsthatFeedtheWorld_ourcrops <- croplist %>% 
  select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>% 
  filter(PlantsthatFeedtheWorld_name != "NA")

# rename crop field in Plants That Feed the World dataset to PlantsthatFeedtheWorld_name
PTFTW_indicator_average <- PTFTW_indicator_average %>% 
  rename(PlantsthatFeedtheWorld_name = crop)

## join PlantsthatFeedtheWorld_ourcrops to PlantsThatFeedTheWorld_rawdata
PTFTW_indicator_average <- PlantsthatFeedtheWorld_ourcrops %>% 
  left_join(PTFTW_indicator_average, by = "PlantsthatFeedtheWorld_name")

## select relevant fields to keep for the indicator file for our summaries and metrics
PTFTW_indicator_ourCrops <- subset(PTFTW_indicator_average, 
                                       select = c( "PlantsthatFeedtheWorld_name",
                                                   "CropStrategy",
                                                   "Genera_primary",
                                                   "Taxa_main", 
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
                                                   "crop_use-faostat-food_supply-food_supply_fat_g",
                                                   "crop_use-faostat-food_supply-food_supply_kcal",
                                                   "crop_use-faostat-food_supply-food_supply_quantity_g",
                                                   "crop_use-faostat-food_supply-protein_supply_quantity_g",
                                                   "crop_use-public_interest-wikipedia_pageviews-taxon",
                                                   "crop_use-research_significance-google_scholar-taxon",
                                                   "crop_use-research_significance-pubmed_central-taxon",
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
                                                   "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
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
                                                          ))

# save file as Indicator file
write_xlsx(PTFTW_indicator_ourCrops, "C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/PTFTW_indicator_ourcrops2025_06_23.xlsx")

# Define processing method across genera in crops (if necessary)
sum_cols <- c("supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene",
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
              "crop_use-faostat-food_supply-food_supply_fat_g",
              "crop_use-faostat-food_supply-food_supply_kcal",
              "crop_use-faostat-food_supply-food_supply_quantity_g",
              "crop_use-faostat-food_supply-protein_supply_quantity_g",
              "crop_use-public_interest-wikipedia_pageviews-taxon",
              "crop_use-research_significance-google_scholar-taxon",
              "crop_use-research_significance-pubmed_central-taxon" )

avg_cols <- c("crop_use-faostat_equality_of_use-gini_food_supply-food_supply_kcal",
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
              "interdependence-faostat-trade-import_value_tonnes" )

max_cols <- c("crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
              "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
              "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
              "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
              "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
              "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
              "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
              "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
              "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
              "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
              "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes")

PTFTW_metrics <- PTFTW_indicator_ourCrops %>%
  rename(
    cropstrategy = CropStrategy,
    PTFTW_name   = PlantsthatFeedtheWorld_name,
    genus        = Genera_primary,
    fullTaxa     = Taxa_main) %>%
  group_by(cropstrategy) %>%
  summarise( across(all_of(sum_cols), sum, na.rm = TRUE),
             across(all_of(avg_cols), mean, na.rm = TRUE),
             across(all_of(max_cols), max, na.rm = TRUE)
             .groups = "drop" )
# save 
write_xlsx(PTFTW_metrics,"4_Estimate_metrics/Priority_metrics_results/PTFTW_metrics2025_06_27.xlsx")  #add processing date


## 20. gini metric calculations (3 metrics)
# ---- Read and Clean Data ----
transfers_2012_2019 <- "Data/Plant_Treaty/Data_store/ITPGRFA_MLSDataStore2022_7_1.xlsx"
transfers_2019_2021 <- "Data/Plant_Treaty/Data_store/Transfers_ourcrops_2019-2021.xlsx"
countries_regions <- "Data_proccessing/Support_files/Geographical/countries_in_regions.xlsx"

# Filter 2015-2018 data
transfers_2015_2018 <- read_csv(transfers_2012_2019) %>%
  filter(Year >= 2015 & Year <= 2018 & Dataset != "CGIAR only") %>%
  select(provider_ISO3, Provider_country, Crop_cleaned, Taxonomic_name, Year, Samples, ISO3, Recipient_country) %>%
  rename(
    provider_country_name = Provider_country,
    recipient_ISO3 = ISO3,
    year = Year,
    recipient_country_name = Recipient_country,
    number_of_samples = Samples,
    PlantsthatFeedtheWorld_name = Crop_cleaned
  )

# Filter crops from Croplist in transfers_2015_2018
croplist <- read_excel(croplist_file) %>%
  select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>%
  filter(PlantsthatFeedtheWorld_name != "NA") %>%
  distinct(PlantsthatFeedtheWorld_name, .keep_all = TRUE)

# 2015-2018 data, select relevant data
transfers_2015_2018 <- left_join(croplist, transfers_2015_2018, by = "PlantsthatFeedtheWorld_name") %>%
  select(CropStrategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
  rename(crop_strategy = CropStrategy) %>%
  filter(!is.na(number_of_samples))

# 2019-2021 data, select relevant data
transfers_2019_2021 <- transfers_2019_2021 %>% 
  select(crop_strategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples)

# ---- Combine Data ----
transfers_2025_2021 <- bind_rows(transfers_2019_2021, transfers_2015_2018)

# ---- Metrics ----
# Calculate Average number of samples per year by crop
df_avg_final <- transfers_2025_2021 %>%
  group_by(crop_strategy, year) %>%
  summarize(avg_number_of_samples = mean(number_of_samples), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_samples_per_year = mean(avg_number_of_samples), .groups = "drop")

# Calculate Average number of recipient countries per year by crop
df_avg_recipient_counts <- transfers_2025_2021 %>%
  group_by(crop_strategy, year) %>%
  summarize(number_of_recipient_countries = n_distinct(recipient_ISO3), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_recipient_countries = mean(number_of_recipient_countries), .groups = "drop")

# Calculate Gini index for recipient regions by crop
countries_in_regions <- read_excel(countries_regions_file) %>%
  select(recipient_ISO3 = Country_code, regions = PlantsThatFeedTheWorld_Region_new)

transfers_regions <- left_join(transfers, countries_in_regions, by = "recipient_ISO3") %>%
  separate_rows(regions, sep = ",") %>%
  mutate(regions = trimws(regions))

gini_results <- transfers_regions %>%
  group_by(crop_strategy, regions) %>%
  summarize(total_samples = sum(number_of_samples, na.rm = TRUE), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(gini_index = ineq::Gini(total_samples), .groups = "drop")

# ---- Combine All Metrics ----
transfers_metrics_2015_2021 <- df_avg_final %>%
  left_join(df_avg_recipient_counts, by = "crop_strategy") %>%
  left_join(gini_results, by = "crop_strategy") %>%
  rename(
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty" = avg_number_of_samples_per_year,
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty" = avg_number_of_recipient_countries,
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty" = gini_index
  )

# save
write_xlsx(transfers_metrics_2015_2021, transfers_2015_2021_metrics2025_06_25.xlsx) 


# 21. Count of records in GBIF
source('Functions/Call_gbif_API.R')   # Import function get_gbif_count

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
# SG didnt add yet


# --------- END OF SCRIPT ---------


