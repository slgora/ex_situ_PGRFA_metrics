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
combined_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_18/combined_df.csv") 
SGSV_allcrops <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/SGSV_processed.csv")
BGCI_allcrops <- read_csv("../../Data_processing//3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/BGCI_processed.csv") # Note: Add BGCI_allcrops import if used, check if need to add Crop_strategy here
GLIS_dataset  <- read_csv("../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/GLIS_processed.csv") # glis_data_processed is the data after adding the cropstrategy variable
croplist <- read_excel("../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx")
institute_names_no_syn <- read_excel("../../Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")

# --------- METRICS CALCULATIONS ---------

# 1. Total number of accessions by crop strategy
accession_by_crop_strategy <- combined_allcrops %>% count(Crop_strategy, name = "accessions_count")
accession_by_source <- combined_allcrops %>% count(data_source, Crop_strategy, name = "accessions_count")

# 2. Unique institutions per crop strategy
unique_institutions <- combined_allcrops %>%
  group_by(Crop_strategy) %>% summarise(unique_instcount = n_distinct(INSTCODE), .groups = "drop")

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
  filter(SAMPSTAT <= 399 | SAMPSTAT == 999 | is.na(SAMPSTAT)) %>%
  percent_summary(
    Crop_strategy,
    sum(fromPrimary_diversity_region, na.rm = TRUE),
    primaryregions_total_records,
    isinprimaryregion_perc
  )

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

# 7. accessions by org type,  and MLS accessions for organization type (A15 collection versus non-A15)
combined_allcrops <- combined_allcrops %>% 
  mutate( A15_collection = ifelse(ORGANIZATIONTYPE %in% c("CGIAR", "International"), TRUE, FALSE) )

accessions_by_org_type <- combined_allcrops %>%
  group_by(Crop_strategy, A15_collection) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  group_by(Crop_strategy) %>%
  mutate(percent = round(100 * n_records / sum(n_records), 2)) %>%
  ungroup()

mls_by_orgtype <- combined_allcrops %>%
  mutate(A15_collection = ORGANIZATIONTYPE %in% c("CGIAR", "International")) %>%
  group_by(Crop_strategy) %>%
  mutate(total_crop_records = n()) %>%
  group_by(Crop_strategy, A15_collection, total_crop_records) %>%
  summarise(
    count_includedmls = sum(MLSSTAT == TRUE, na.rm = TRUE),
    count_notincludedmls = sum(MLSSTAT == FALSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_includedmls = round(100 * count_includedmls / total_crop_records, 2),
    percent_notincludedmls = round(100 * count_notincludedmls / total_crop_records, 2)
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
accession_base <- combined_allcrops %>%
  mutate(Accession_ID = row_number()) %>%
  select(Accession_ID, Crop_strategy)

storage_types <- list(
  seed      = c("10", "11", "12", "13"),
  field     = "20",
  invitro   = "30",
  cryo      = "40",
  dna       = "50",
  other     = "99"
)

storage_long <- combined_allcrops %>%
  mutate(
    STORAGE = as.character(STORAGE),
    Accession_ID = row_number()
  ) %>%
  filter(!is.na(STORAGE)) %>%
  separate_rows(STORAGE, sep = ";") %>%
  mutate(STORAGE = str_trim(STORAGE))

nostorage_counts <- combined_allcrops %>%
  mutate(Accession_ID = row_number()) %>%
  filter(is.na(STORAGE)) %>%
  count(Crop_strategy, name = "nostorage_count")

# Storage type summary
storage_term_summary <- storage_long %>%
  filter(STORAGE %in% storage_types$seed) %>%
  group_by(Accession_ID, Crop_strategy) %>%
  summarise(
    has_10 = any(STORAGE == "10"),
    has_11 = any(STORAGE == "11"),
    has_12 = any(STORAGE == "12"),
    has_13 = any(STORAGE == "13"),
    .groups = "drop"
  ) %>%
  mutate(
    seed_storage_category = case_when(
      has_13 ~ "long_term",
      has_12 ~ "medium_term",
      has_11 ~ "short_term",
      has_10 & !has_11 & !has_12 & !has_13 ~ "not_specified",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(seed_storage_category)) %>%  # Remove NA categories
  left_join(accession_base, by = c("Accession_ID", "Crop_strategy")) %>%
  group_by(Crop_strategy) %>%
  summarise(
    total_records = n(),
    seed_count = sum(!is.na(seed_storage_category)),
    longterm_storage_count = sum(seed_storage_category == "long_term", na.rm = TRUE),
    medterm_storage_count  = sum(seed_storage_category == "medium_term", na.rm = TRUE),
    shortterm_storage_count = sum(seed_storage_category == "short_term", na.rm = TRUE),
    not_specified_seed_storage_count = sum(seed_storage_category == "not_specified", na.rm = TRUE)
  ) %>%
  mutate(
    seed_perc = round(100 * seed_count / total_records, 2),
    longterm_storage_perc = round(100 * longterm_storage_count / total_records, 2),
    medterm_storage_perc = round(100 * medterm_storage_count / total_records, 2),
    shortterm_storage_perc = round(100 * shortterm_storage_count / total_records, 2),
    not_specified_seed_storage_perc = round(100 * not_specified_seed_storage_count / total_records, 2)
  )

# Seed storage term summary
storage_term_summary <- storage_long %>%
  filter(STORAGE %in% storage_types$seed) %>%
  group_by(Accession_ID, Crop_strategy) %>%
  summarise(
    has_10 = any(STORAGE == "10"),
    has_11 = any(STORAGE == "11"),
    has_12 = any(STORAGE == "12"),
    has_13 = any(STORAGE == "13"),
    .groups = "drop"
  ) %>%
  mutate(
    seed_storage_category = case_when(
      has_13 ~ "long_term",
      has_12 ~ "medium_term",
      has_11 ~ "short_term",
      has_10 & !has_11 & !has_12 & !has_13 ~ "not_specified",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(accession_base, by = c("Accession_ID", "Crop_strategy")) %>%
  group_by(Crop_strategy) %>%
  summarise(
    total_records = n(),
    seed_count = sum(!is.na(seed_storage_category)),
    longterm_storage_count = sum(seed_storage_category == "long_term", na.rm = TRUE),
    medterm_storage_count  = sum(seed_storage_category == "medium_term", na.rm = TRUE),
    shortterm_storage_count = sum(seed_storage_category == "short_term", na.rm = TRUE),
    not_specified_seed_storage_count = sum(seed_storage_category == "not_specified", na.rm = TRUE)
  ) %>%
  mutate(
    seed_perc = round(100 * seed_count / total_records, 2),
    longterm_storage_perc = round(100 * longterm_storage_count / total_records, 2),
    medterm_storage_perc = round(100 * medterm_storage_count / total_records, 2),
    shortterm_storage_perc = round(100 * shortterm_storage_count / total_records, 2),
    not_specified_seed_storage_perc = round(100 * not_specified_seed_storage_count / total_records, 2)
  )

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
sd_outcountry_metric <- apply(sd_outcountry_metric,2,as.character)

sd_outcountry_metric_by_crop <- genesys %>%
  group_by(Crop_strategy) %>%
  summarise(
    sd_out_country_count = sum(sd_out_country, na.rm = TRUE),
    accessions_total = n()
  ) %>%
  arrange(desc(sd_out_country_count)) %>%
  mutate(sd_out_country_perc = 100 * sd_out_country_count / accessions_total)
sd_outcountry_metric_by_crop <- apply(sd_outcountry_metric_by_crop,2,as.character)

# 11. SGSV duplicates
SGSV_dupl_metric <- SGSV_allcrops %>%
  count(Crop_strategy, name = "sgsv_dupl_count") %>%
  left_join(
    combined_allcrops %>% count(Crop_strategy, name = "total_count"),
    by = "Crop_strategy"
  ) %>%
  mutate(sgsv_dupl_perc = round((sgsv_dupl_count / total_count) * 100, 2)) %>%
  select(Crop_strategy, sgsv_dupl_count, total_count, sgsv_dupl_perc)

# 12. GLIS: # of accessions with DOIs per crop, use data downloaded from GLIS (GLIS_dataset)
GLIS_dois_count <- GLIS_dataset %>%
  group_by(Crop_strategy) %>%
  summarise(dois = sum(!is.na(DOI) & DOI != ""), .groups = "drop")

# 13: GLIS: # of accessions notified as include in MLS (based on GLIS dataset)
GLIS_MLS_count <- GLIS_dataset %>% 
  group_by(Crop_strategy) %>% 
  summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")

# 14. Top institutions holding crop germplasm a. count and b. percent, c. # of accessions in long term storage, and d. # of accessions included in MLS (from GLIS)
# a. Count of top institutions holding crop germplasm and b. percent
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

# c. Number of accessions in long term storage (-18-20 C)
long_term_storage <- combined_allcrops %>%
  filter(str_detect(as.character(STORAGE), "\\b13\\b")) %>%
  group_by(Crop_strategy, INSTCODE) %>%           #by crop & institute
  summarise(long_term_storage_count = n(), .groups = "drop")

# d. Number of accessions included in MLS (from GLIS)
GLIS_MLS_count_by_inst <- GLIS_dataset %>%
  group_by(Crop_strategy, INSTCODE) %>%          #by crop & institute
  summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")

# add metrics c & d to metric 14 table
institution_accessions_summary <- institution_accessions_summary %>%
  left_join(long_term_storage,        by = c("Crop_strategy","INSTCODE")) %>%
  left_join(GLIS_MLS_count_by_inst,   by = c("Crop_strategy","INSTCODE")) %>%
  mutate(
    # fill NAs
    long_term_storage_count = replace_na(long_term_storage_count, 0),
    MLS_notified             = replace_na(MLS_notified, 0),
    # create the two new display columns
    `Number of accessions in long term storage (-18-20 C) and source` =
      if_else(
        long_term_storage_count > 0,
        paste0(long_term_storage_count, " (storage=13)"),
        "" ),
    `Number of accessions included in MLS (from GLIS)` =
      MLS_notified
  )

# 15. Number of unique taxa listed in BGCI data metric (BGCI dataset)
BGCI_taxa_count <- BGCI_allcrops %>%
  filter(!is.na(Standardized_taxa)) %>%
  distinct(Crop_strategy, Standardized_taxa) %>%
  count(Crop_strategy, name = "bgci_unique_taxa_count")

# 16. Number of unique institutions holding crop germplasm (BGCI dataset)
BGCI_inst_count <- BGCI_allcrops %>%
  filter(!is.na(ex_situ_site_gardenSearch_ID)) %>%
  distinct(Crop_strategy, ex_situ_site_gardenSearch_ID) %>%
  count(Crop_strategy, name = "unique_inst_count")

# 17. Regeneration metrics (based on WIEWS indicator file)
# read in processed WIEWS indicator file, metrics already extracted
WIEWS_regeneration_summary <- read_csv("../../Data_processing/1_merge_data/2025_07_08/WIEWS_indicator_processed.csv")

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

# 22. Characterization and Evaluation datasets
source("Functions/Process_char_eval.R")
# call location of folders with char and eval datasets extracted from Genesys
char_eval_summary <- summarize_char_eval('../../Data/Genesys/Characterization_and_evaluation_datasets')


# --------- COMPILE ALL METRICS INTO ONE LIST ---------
library(openxlsx)

# Collect all metrics into a list for tables
metrics_list <- list(
  accession_by_crop_strategy       = accession_by_crop_strategy,
  accession_by_source              = accession_by_source,
  unique_institutions              = unique_institutions,
  cwr_metric                       = cwr_metric,
  weedy_metric                     = weedy_metric,
  landrace_metric                  = landrace_metric,
  breeding_metric                  = breeding_metric,
  improved_metric                  = improved_metric,
  othervar_metric                  = othervar_metric,
  no_SAMPSTAT_metric               = no_SAMPSTAT_metric,
  unique_taxa                      = unique_taxa,
  country_count                    = country_count,
  primary_region_metric            = primary_region_metric,
  diversity_regions_metric         = diversity_regions_metric,
  accessions_by_org_type           = accessions_by_org_type,
  mls_by_orgtype                   = mls_by_orgtype,
  annex1_count                     = annex1_count,
  annex1_perc                      = annex1_perc,
  storage_summary                  = storage_summary,
  storage_term_summary             = storage_term_summary,
  sd_outcountry_metric_by_crop     = sd_outcountry_metric_by_crop,
  SGSV_dupl_metric                 = SGSV_dupl_metric,
  GLIS_dois_count                  = GLIS_dois_count,
  GLIS_MLS_count                   = GLIS_MLS_count,
  institution_accessions_summary  = institution_accessions_summary,
  BGCI_taxa_count                  = BGCI_taxa_count,
  BGCI_inst_count                  = BGCI_inst_count,
  WIEWS_regeneration_summary       = WIEWS_regeneration_summary,
  pdci_summary                     = pdci_summary,
  gbif_count_summary               = gbif_count_summary,
  char_eval_summary                = char_eval_summary
)
# Save all metrics to excel file with each sheet as different metric
write.xlsx(metrics_list, file = "../../Data_processing/4_Estimate_metrics/2025_07_25/all_metrics_summary.xlsx")


# --------- Export Taxa  ---------
######## for each crop produce a table with number of accessions for each taxa ###############
# Get taxa counts per crop
taxa_counts <- combined_allcrops %>%
  group_by(Crop_strategy, Standardized_taxa) %>%
  summarise(accession_count = n(), .groups = "drop") %>%
  arrange(Crop_strategy, desc(accession_count))

# Split by crop for Excel export
taxa_by_crop <- split(taxa_counts, taxa_counts$Crop_strategy)
names(taxa_by_crop) <- substr(names(taxa_by_crop), 1, 31)

# Write to Excel
write.xlsx(taxa_by_crop, "../../Data_processing/4_estimate_metrics/2025_07_17/accessions_per_taxa.xlsx")

############ End of Script ############
