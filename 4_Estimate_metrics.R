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
combined_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_08/combined_df.csv") 
SGSV_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/SGSV_processed.csv")
BGCI_allcrops <- read_csv("../../Data_processing//3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/BGCI_processed.csv") # Note: Add BGCI_allcrops import if used, check if need to add Crop_strategy here
GLIS_dataset  <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/GLIS_processed.csv") # glis_data_processed is the data after adding the cropstrategy variable
croplist <- read_excel("../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx")
institute_names_no_syn <- read_excel("../../Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")

# --------- METRICS CALCULATIONS ---------

# 1. Total number of accessions by crop strategy
accession_by_crop_strategy <- combined_allcrops %>% count(Crop_strategy, name = "accessions_count")
accession_by_source_crop_strategy <- combined_allcrops %>% count(data_source, Crop_strategy, name = "accessions_count")
# save results
write.csv(accession_by_crop_strategy, '../../Data_processing/4_Estimate_metrics/2025_07_14/accession_by_crop_strategy.csv', row.names = FALSE)
write.csv(accession_by_source_crop_strategy, '../../Data_processing/4_Estimate_metrics/2025_07_14/accession_by_source_crop_strategy.csv', row.names = FALSE)

# 2. Unique institutions per crop strategy
unique_institutions <- combined_allcrops %>%
  group_by(Crop_strategy) %>% summarise(unique_instcount = n_distinct(INSTCODE), .groups = "drop")
# save results
write.csv(unique_institutions, '../../Data_processing/4_Estimate_metrics/2025_07_14/unique_institutions.csv', row.names = FALSE)

# 3. Wild, Weedy, Landrace, Breeding, Improved, Other, No SAMPSTAT - % summaries
cwr_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT >= 100 & SAMPSTAT < 200, na.rm = TRUE), cwr_total_records, SAMPSTAT100_perc) %>%
  rename(SAMPSTAT100_count = count)
weedy_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 200, na.rm = TRUE), weedy_total_records, SAMPSTAT200_perc) %>%
  rename(SAMPSTAT200_count = count)
landrace_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 300, na.rm = TRUE), landrace_total_records, SAMPSTAT300_perc)%>%
  rename(SAMPSTAT300_count = count)
breeding_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT >= 400 & SAMPSTAT < 500, na.rm = TRUE), breedingmat_total_records, SAMPSTAT400s_perc)%>%
  rename(SAMPSTAT400s_count = count)
improved_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 500, na.rm = TRUE), improvedvar_total_records, SAMPSTAT500_perc)%>%
  rename(SAMPSTAT500_count = count)
othervar_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(SAMPSTAT == 999, na.rm = TRUE), othervar_total_records, SAMPSTAT999_perc)%>%
  rename(SAMPSTAT999_count = count)
no_SAMPSTAT_metric <- percent_summary(combined_allcrops, Crop_strategy, sum(is.na(SAMPSTAT)), noSAMPSTAT_total_records, SAMPSTATna_perc)%>%
  rename(SAMPSTATna_count = count)
# save results
write.csv(cwr_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/cwr_metric.csv", row.names = FALSE)
write.csv(weedy_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/weedy_metric.csv", row.names = FALSE)
write.csv(landrace_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/landrace_metric.csv", row.names = FALSE)
write.csv(breeding_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/breeding_metric.csv", row.names = FALSE)
write.csv(improved_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/improved_metric.csv", row.names = FALSE)
write.csv(othervar_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/othervar_metric.csv", row.names = FALSE)
write.csv(no_SAMPSTAT_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/no_SAMPSTAT_metric.csv", row.names = FALSE)

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
# save results
unique_taxa <- unique_taxa %>%  # Convert taxa column to semicolon-separated string
  mutate(unique_taxa = sapply(unique_taxa, function(x) paste(x, collapse = "; ")))
write.csv(unique_taxa, "../../Data_processing/4_Estimate_metrics/2025_07_14/unique_taxa.csv", row.names = FALSE)

# 5. Number of countries where germplasm collected (excluding certain SAMPSTAT)
country_count <- combined_allcrops %>%
  filter(!(SAMPSTAT %in% c(400:499, 500, 600))) %>%
  group_by(Crop_strategy) %>%
  summarise(unique_countrycount = n_distinct(ORIGCTY), .groups = "drop")
# save results
write.csv(country_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/country_count.csv", row.names = FALSE)

# 6.a and 6.b Accessions from primary & secondary regions of diversity
primary_region_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | SAMPSTAT == 999 | is.na(SAMPSTAT)) %>%
  percent_summary(
    Crop_strategy,
    sum(fromPrimary_diversity_region, na.rm = TRUE),
    primaryregions_total_records,
    isinprimaryregion_perc
  )
#save results
write.csv(primary_region_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/primary_region_metric.csv", row.names = FALSE)

# 6.c and 6.d Diversity_regions_metric (primary + secondary regions)
diversity_regions_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | SAMPSTAT == 999 | is.na(SAMPSTAT)) %>%
  group_by(Crop_strategy) %>%
  summarise(
    isindiversityregions_count = sum(fromPrimary_diversity_region, na.rm = TRUE) +
                                 sum(fromSecondary_diversity_region, na.rm = TRUE),
    total_accessions = n(),
    isindiversityregions_perc = round(100 * isindiversityregions_count / total_accessions, 2),
    .groups = "drop"
  )
# save results
write.csv(diversity_regions_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/diversity_regions_metric.csv", row.names = FALSE)

# 7. accessions by org type,  and MLS accessions for organization type (A15 collection versus non-A15)
combined_allcrops <- combined_allcrops %>% 
  mutate( A15_collection = ifelse(ORGANIZATIONTYPE %in% c("CGIAR", "International"), TRUE, FALSE) )

accessions_by_org_type <- combined_allcrops %>%
  group_by(Crop_strategy, A15_collection) %>%
  summarise(
    n_records = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percent = round(100 * n_records / sum(n_records), 2)
  )

mls_by_orgtype <- combined_allcrops %>%
  group_by(Crop_strategy, A15_collection) %>%
  summarise(
    count_includedmls = sum(MLSSTAT, na.rm = TRUE),
    count_notincludedmls = sum(!MLSSTAT, na.rm = TRUE),
    total = n(),
    percent_includedmls = round(100 * count_includedmls / total, 2),
    percent_notincludedmls = round(100 * count_notincludedmls / total, 2),
    .groups = "drop"
  )
#save results
write.csv(accessions_by_org_type, "../../Data_processing/4_Estimate_metrics/2025_07_14/accessions_by_org_type.csv", row.names = FALSE)
write.csv(mls_by_orgtype, "../../Data_processing/4_Estimate_metrics/2025_07_14/mls_by_orgtype.csv", row.names = FALSE)

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
# save results
write.csv(annex1_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/annex1_count.csv", row.names = FALSE)
write.csv(annex1_perc, "../../Data_processing/4_Estimate_metrics/2025_07_14/annex1_perc.csv", row.names = FALSE)

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
    seed_perc      = round(100 * seed_count / total_records, 2),
    field_perc     = round(100 * field_count / total_records, 2),
    invitro_perc   = round(100 * invitro_count / total_records, 2),
    cryo_perc    = round(100 * cryo_count / total_records, 2),
    dna_perc       = round(100 * dna_count / total_records, 2),
    other_perc     = round(100 * other_count / total_records, 2),
    nostorage_perc = round(100 * nostorage_count / total_records, 2)
  )

storage_term_summary <- combined_allcrops %>%
  group_by(Crop_strategy) %>%
  summarise(
    total_records = n(),
    not_specified_seed_storage_count = sum(str_detect(STORAGE, "^10$"), na.rm = TRUE),
    longterm_storage_count  = sum(str_detect(STORAGE, "^13$"), na.rm = TRUE),
    medterm_storage_count   = sum(str_detect(STORAGE, "^12$"), na.rm = TRUE),
    shortterm_storage_count = sum(str_detect(STORAGE, "^11$"), na.rm = TRUE)
  ) %>%
  mutate(
    not_specified_seed_storage_perc =  round(100 * not_specified_seed_storage_count / total_records, 2),
    longterm_storage_perc   = round(100 * longterm_storage_count / total_records, 2),
    medterm_storage_perc    = round(100 * medterm_storage_count / total_records, 2),
    shortterm_storage_perc  = round(100 * shortterm_storage_count / total_records, 2)
  )
# save results
write.csv(storage_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/storage_summary.csv", row.names = FALSE)
write.csv(storage_term_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/storage_term_summary.csv", row.names = FALSE)

# 10. Safety duplication percentage of accessions duplicated out of the country in other genebanks (excluding SGSV) calculated only using Genesys data. 
source("Functions/SD_duplicates_out_country.R") # source function

genesys <- combined_allcrops %>%
  filter(data_source == "Genesys") %>%
  mutate(
    holding_country = substr(INSTCODE, 1, 3),
    sd_out_country = mapply(duplicates_out_country, DUPLSITE, holding_country)
  )

# Run summary of metric calculated, SD out of country by genebank
sd_outcountry_metric <- genesys %>%
  group_by(INSTCODE) %>%
  summarise(
    sd_out_country_count = sum(sd_out_country, na.rm = TRUE),
    accessions_total = n()
  ) %>%
  arrange(desc(sd_out_country_count)) %>%
  mutate(sd_out_country_perc = 100 * sd_out_country_count / accessions_total)

# Save output to Drive folder, please use subfolder with date of running the script 
sd_outcountry_metric <- apply(sd_outcountry_metric,2,as.character)
write.csv(sd_outcountry_metric, '../../Data_processing/4_Estimate_metrics/2025_07_14/sd_outcountry_metric.csv', row.names = FALSE)

# 11. SGSV duplicates
SGSV_dupl_metric <- SGSV_allcrops %>%
  count(Crop_strategy, name = "sgsv_dupl_count") %>%
  left_join(
    combined_allcrops %>% count(Crop_strategy, name = "total_count"),
    by = "Crop_strategy"
  ) %>%
  mutate(sgsv_dupl_perc = round((sgsv_dupl_count / total_count) * 100, 2)) %>%
  select(Crop_strategy, sgsv_dupl_count, total_count, sgsv_dupl_perc)
# save results
write.csv(SGSV_dupl_metric, "../../Data_processing/4_Estimate_metrics/2025_07_14/SGSV_dupl_metric.csv", row.names = FALSE)

# 12. GLIS: # of accessions with DOIs per crop, use data downloaded from GLIS (GLIS_dataset)
GLIS_dois_count <- GLIS_dataset %>%
  group_by(Crop_strategy) %>%
  summarise(dois = sum(!is.na(DOI) & DOI != ""), .groups = "drop")
# save results
write.csv(GLIS_dois_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/GLIS_dois_count.csv", row.names = FALSE)

# 13: GLIS: # of accessions notified as include in MLS (based on GLIS dataset)
GLIS_MLS_count <- GLIS_dataset %>% 
  group_by(Crop_strategy) %>% 
  summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")
# save results
write.csv(GLIS_MLS_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/GLIS_MLS_count.csv", row.names = FALSE)

# 14. Top institutions holding crop germplasm
institution_accessions_summary <- combined_allcrops %>%
  filter(!is.na(INSTCODE)) %>%
  group_by(Crop_strategy, INSTCODE) %>%
  summarise(institution_accessions_count = n(), .groups = "drop_last") %>%
  group_by(Crop_strategy) %>%
  mutate(
    total_accessions = sum(institution_accessions_count, na.rm = TRUE),
    institution_accessions_perc = round((institution_accessions_count / total_accessions) * 100, 2)
  ) %>%
  ungroup()

# add organization name to metric 14 table
table_INSTCODE_to_name <- setNames(
  institute_names_no_syn[["Name of organization"]],
  institute_names_no_syn[["WIEWS instcode"]])

institution_accessions_summary$Institute_name = NA
institution_accessions_summary <- institution_accessions_summary %>%
  mutate(
    Institute_name = ifelse(
      is.na(Institute_name),
      table_INSTCODE_to_name[INSTCODE],
      Institute_name))
# save results
write.csv(institution_accessions_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/institution_accessions_summary.csv", row.names = FALSE)

# 15. Number of unique taxa listed in BGCI data metric (BGCI dataset)
BGCI_taxa_count <- BGCI_allcrops %>%
  filter(!is.na(Standardized_taxa)) %>%
  distinct(Crop_strategy, Standardized_taxa) %>%
  count(Crop_strategy, name = "bgci_unique_taxa_count")
# save results
write.csv(BGCI_taxa_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/BGCI_taxa_count.csv", row.names = FALSE)

# 16. Number of unique institutions holding crop germplasm (BGCI dataset)
BGCI_inst_count <- BGCI_allcrops %>%
  filter(!is.na(ex_situ_site_gardenSearch_ID)) %>%
  distinct(Crop_strategy, ex_situ_site_gardenSearch_ID) %>%
  count(Crop_strategy, name = "unique_inst_count")
# save results
write.csv(BGCI_inst_count, "../../Data_processing/4_Estimate_metrics/2025_07_14/BGCI_inst_count.csv", row.names = FALSE)

# 17. Regeneration metrics (based on WIEWS indicator file)
# read in processed WIEWS indicator file, metrics already extracted
WIEWS_regeneration_summary <- read_csv("../../Data_processing/1_merge_data/2025_07_08/WIEWS_indicator_processed.csv")
# save results to Drive location
write.csv(WIEWS_regeneration_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/WIEWS_regeneration_summary.csv", row.names = FALSE)

# 18. PDCI metric
# Source and Run function to calculate PDCI
source("Functions/Get_PDCI.R")
df <- combined_allcrops %>% 
  filter(data_source == "Genesys") # use genesys processed data
df <- get_PDCI(df)

# Extract median PDCI
pdci_summary <- df %>% 
  group_by(Crop_strategy) %>%
  summarise(
    median_PDCI = median(PDCI, na.rm = TRUE) #median PDCI
  )
# save results
write.csv(pdci_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/pdci_summary.csv", row.names = FALSE)

# 19.- 20. PTFTW Metrics; Note implemented in individual script 5_PTFTW_processing_and_metrics.R

# 21. Count of records in GBIF
source("Functions/Call_gbif_API.R")  # Import function get_gbif_count
gbif_count_summary <- croplist %>%
  rename(Crop_strategy = CropStrategy) %>%
  rowwise() %>%
  mutate(
    count_primary = get_gbif_count(Genera_primary),
    count_synonym = get_gbif_count(Genera_synonyms),
    GBIF_count_total = sum(count_primary, count_synonym, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Crop_strategy) %>%
  summarise(
    total_GBIF_count = sum(GBIF_count_total, na.rm = TRUE),
    .groups = "drop"
  )
# save results
write.csv(gbif_count_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/gbif_count_summary.csv", row.names = FALSE)

# 22. Characterization and Evaluation datasets
source("Functions/Process_char_eval.R")
# call location of folders with char and eval datasets extracted from Genesys
char_eval_summary <- summarize_char_eval('../../Data/Genesys/Characterization_and_evaluation_datasets')
# save results
write.csv(char_eval_summary, "../../Data_processing/4_Estimate_metrics/2025_07_14/char_eval_summary.csv", row.names = FALSE)

############ End of Script ############
