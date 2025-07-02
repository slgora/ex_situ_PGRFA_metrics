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
combined_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/combined_df_2025_07_02.csv") 
SGSV_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/SGSV_processed.csv")
BGCI_allcrops <- read_csv("../../Data_processing//1_merge_data/2025_07_02//BGCI_processed.csv") # Note: Add BGCI_allcrops import if used, check if need to add Crop_strategy here
GLIS_dataset  <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/GLIS_processed.csv") # glis_data_processed is the data after adding the cropstrategy variable

# --------- METRICS CALCULATIONS ---------

# 1. Total number of accessions by crop strategy
accession_by_crop_strategy <- combined_allcrops %>% count(Crop_strategy, name = "accessions_count")
accession_by_source_crop_strategy <- combined_allcrops %>% count(data_source, Crop_strategy, name = "accessions_count")

# 2. Unique institutions per crop strategy
unique_institutions <- combined_allcrops %>%
  group_by(Crop_strategy) %>% summarise(unique_instcount = n_distinct(INSTCODE), .groups = "drop")

# 3. Wild, Weedy, Landrace, Breeding, Improved, Other, No SAMPSTAT - % summaries
cwr_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT >= 100 & SAMPSTAT < 200, na.rm = TRUE), cwr_total_records, percent_100SAMPSTAT)
weedy_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 200, na.rm = TRUE), weedy_total_records, percent_200SAMPSTAT)
landrace_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 300, na.rm = TRUE), landrace_total_records, percent_300SAMPSTAT)
breeding_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT >= 400 & SAMPSTAT < 500, na.rm = TRUE), breedingmat_total_records, percent_400SAMPSTAT)
improved_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 500, na.rm = TRUE), improvedvar_total_records, percent_500SAMPSTAT)
othervar_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 999, na.rm = TRUE), othervar_total_records, percent_999SAMPSTAT)
no_SAMPSTAT_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(is.na(SAMPSTAT)), noSAMPSTAT_total_records, percent_na_SAMPSTAT)

# 4. Unique taxa per crop
unique_taxa <- combined_allcrops %>%
  select(Crop_strategy, Standardized_taxa) %>%
  distinct() %>%
  group_by(Crop_strategy) %>%
  summarise(
    unique_taxa = list(unique(Standardized_taxa)),
    unique_taxa_count = n_distinct(Standardized_taxa),
    .groups = "drop"
  )

# 5. Number of countries where germplasm collected (excluding certain SAMPSTAT)
country_count <- combined_allcrops %>%
  filter(!(SAMPSTAT %in% c(400:499, 500, 600))) %>%
  group_by(Crop_strategy) %>%
  summarise(unique_countrycount = n_distinct(ORIGCTY), .groups = "drop")

# 6.a and 6.b Accessions from primary & secondary regions of diversity
primary_region_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  percent_summary(
    Crop_strategy,
    sum(fromPrimary_diversity_region, na.rm = TRUE),
    primaryregions_total_records,
    isinprimaryregion_perc
  )

# 6.c and 6.d Diversity_regions_metric (includes accessions from primary and secondary regions of diversity)
diversity_regions_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  group_by(Crop_strategy) %>%
  summarise(
    isindiversityregions_count = sum(fromPrimary_diversity_region, na.rm = TRUE) + 
                             sum(fromSecondary_diversity_region, na.rm = TRUE),
    total_accessions = n(),
    isindiversityregions_perc = round(100 * isindiversityregions_count / total_accessions, 2),
    .groups = "drop"
  )

# 7. accessions by org type,  and MLS accessions for organization type
accessions_by_org_type <- combined_allcrops %>%
  group_by(Crop_strategy, ORGANIZATIONTYPE) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percent = round(100 * n_records / sum(n_records), 2)
  )

mls_by_orgtype <- combined_allcrops %>%
  group_by(Crop_strategy, ORGANIZATIONTYPE) %>%
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
  group_by(Crop_strategy) %>%
  summarise(count_includedannex1 = sum(Annex1, na.rm = TRUE), .groups = "drop")
annex1_perc <- combined_allcrops %>%
  group_by(Crop_strategy) %>%
  summarise(
    count_includedannex1 = sum(Annex1, na.rm = TRUE),
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
  group_by(Crop_strategy) %>%
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
    seed_pct      = round(100 * seed_count / total_records, 2),
    field_pct     = round(100 * field_count / total_records, 2),
    invitro_pct   = round(100 * invitro_count / total_records, 2),
    cryo_pct      = round(100 * cryo_count / total_records, 2),
    dna_pct       = round(100 * dna_count / total_records, 2),
    other_pct     = round(100 * other_count / total_records, 2),
    nostorage_pct = round(100 * nostorage_count / total_records, 2)
  )

storage_term_summary <- combined_allcrops %>%
  group_by(Crop_strategy) %>%
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

# 10. Safety duplication >>> SG still working
## percentage of accessions duplicated out of the country in other genebanks (excluding SGSV) calculated only using Genesys data. 

### note, before running the sd analysis you need to drop genebanks that only old safety duplicates 
### such as NOR051 and BRA003 from the datasets

source(Functions/SD_duplicates_out_country.R) # source function

# Read dataset 
# what file to use? use combined_allcrops and filter out Genesys?
gen <- read.csv("Genesys_allcrops.csv", header = TRUE )

# Run function to calculate metric, SD by instCode (i.e. genebank)
sd_by_genebank_genesys <- SD_duplicates_out_country(gen2, groupby = 'instCode')

# Save output to Drive folder
sd_by_genebank_genesys <- apply(sd_by_genebank_genesys,2,as.character)
write.csv(sd_by_genebank_genesys, '../../Data_processing/4_Estimate_metrics/Safety_duplication/genesys_sd_results_by_genebanks.csv', row.names = FALSE)


# 11. SGSV duplicates

# SG note: add step to drop potential double counts using accession number+instcode first?

SGSV_dupl_count <- SGSV_allcrops %>% group_by(Crop_strategy) %>% summarise(sgsvcount = n(), .groups = "drop")

# 12. GLIS: # of accessions with DOIs per crop, use data downloaded from GLIS (GLIS_dataset)
GLIS_dois_count <- GLIS_dataset %>%
  group_by(Crop_strategy) %>%
  summarise(dois = sum(!is.na(DOI) & DOI != ""), .groups = "drop")

# 13: GLIS: # of accessions notified as include in MLS (based on GLIS dataset)
GLIS_MLS_count <- GLIS_dataset %>% group_by(Crop_strategy) %>% summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")

# 14. Top institutions holding crop germplasm
institution_accessions_summary <- combined_allcrops %>%    #note: tested and corrected
  filter(!is.na(INSTCODE)) %>%
  group_by(Crop_strategy, INSTCODE) %>%
  summarise(institution_accessions_count = n(), .groups = "drop_last") %>%
  group_by(Crop_strategy) %>%
  mutate(
    total_accessions = sum(institution_accessions_count, na.rm = TRUE),
    institution_accessions_perc = round((institution_accessions_count / total_accessions) * 100, 2)
  ) %>%
  ungroup()

# 15. Number of unique taxa listed in BGCI data metric (BGCI dataset)
BGCI_taxa_count <- BGCI_allcrops %>%               
  select(Crop_strategy, Standardized_taxa) %>%    # need to add crop strategy to BGCI_processed
  filter(!is.na(Standardized_taxa)) %>%
  distinct() %>%
  group_by(Crop_strategy) %>%
  summarise(unique_taxa_count = n_distinct(Standardized_taxa), .groups = "drop")

# 16. Number of unique institutions holding crop germplasm (BGCI dataset)
BGCI_inst_count <- BGCI_allcrops_SG %>%
  select(cropstrategy, ex_situ_site_gardenSearch_ID) %>%  # need to add crop strategy to BGCI_processed
  filter(!is.na(ex_situ_site_gardenSearch_ID)) %>%
  distinct() %>% # unique institution entries
  group_by(cropstrategy) %>%
  summarise(unique_inst_count = n_distinct(ex_situ_site_gardenSearch_ID), .groups = "drop")

# 17. SG: Regeneration metrics (based on WIEWS indicator file)
# Data read in: Wiews indicator 22 file and croplist


############ works until here, the rest needs to be corrected #########


