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
# Note: Add SGSV_allcrops import if used

# --------- METRICS CALCULATIONS ---------

# 1. Total number of accessions by crop strategy
accession_by_crop_strategy <- combined_allcrops %>% count(cropstrategy, name = "accessions_count")
accession_by_source_crop_strategy <- combined_allcrops %>% count(datasource, cropstrategy, name = "accessions_count")

# 2. Unique institutions per crop strategy
unique_institutions <- combined_allcrops %>%
  group_by(cropstrategy) %>% summarise(unique_instcount = n_distinct(instcode), .groups = "drop")

# 3. Wild, Weedy, Landrace, Breeding, Improved, Other, No SAMPSTAT - % summaries
cwr_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT >= 100 & SAMPSTAT < 200, na.rm = TRUE), cwr_total_records, percent_100SAMPSTAT)
weedy_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 200, na.rm = TRUE), weedy_total_records, percent_200SAMPSTAT)
landrace_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 300, na.rm = TRUE), landrace_total_records, percent_300SAMPSTAT)
breeding_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT >= 400 & SAMPSTAT < 500, na.rm = TRUE), breedingmat_total_records, percent_400SAMPSTAT)
improved_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 500, na.rm = TRUE), improvedvar_total_records, percent_500SAMPSTAT)
othervar_metric <- percent_summary(combined_allcrops, cropstrategy, sum(SAMPSTAT == 999, na.rm = TRUE), othervar_total_records, percent_999SAMPSTAT)
no_SAMPSTAT_metric <- percent_summary(combined_allcrops, cropstrategy, sum(is.na(SAMPSTAT)), noSAMPSTAT_total_records, percent_na_SAMPSTAT)

# 4. Unique taxa per crop
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
    primaryregions_total_records,
    isinprimaryregion_perc
  )

# 6.c and 6.d Diversity_regions_metric (includes accessions from primary and secondary regions of diversity)
diversity_regions_metric <- combined_allcrops %>%
  filter(SAMPSTAT <= 399 | is.na(SAMPSTAT)) %>%
  group_by(cropstrategy) %>%
  summarise(
    total_diversity_regions = sum(fromPrimary_diversity_region, na.rm = TRUE) + 
                             sum(fromSecondary_diversity_region, na.rm = TRUE),
    total_accessions = n(),
    percent_diversity_regions = round(100 * total_diversity_regions / total_accessions, 2),
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
    seed_pct      = round(100 * seed_count / total_records, 2),
    field_pct     = round(100 * field_count / total_records, 2),
    invitro_pct   = round(100 * invitro_count / total_records, 2),
    cryo_pct      = round(100 * cryo_count / total_records, 2),
    dna_pct       = round(100 * dna_count / total_records, 2),
    other_pct     = round(100 * other_count / total_records, 2),
    nostorage_pct = round(100 * nostorage_count / total_records, 2)
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

# 11. SGSV duplicates (if applicable)
SGSV_allcrops <- read_csv("sgsv_data_processed.csv") 
SGSV_dupl_count <- SGSV_allcrops %>% group_by(cropstrategy) %>% summarise(sgsvcount = n(), .groups = "drop")

# 12. GLIS: # of accessions with DOIs per crop, use data downloaded from GLIS (GLIS_dataset)
GLIS_dataset <- read_csv("glis_data_processed.csv") # glis_data_processed is the data after adding the cropstrategy variable
GLIS_dois_count <- GLIS_dataset %>% group_by(cropstrategy) %>% summarise(dois = sum(DOI, na.rm = TRUE), .groups = "drop")

# 13: GLIS: # of accessions notified as incuded in MLS (based on GLIS dataset)
GLIS_MLS_count <- GLIS_dataset %>% group_by(cropstrategy) %>% summarise(MLS_notified = sum(MLSSTAT, na.rm = TRUE), .groups = "drop")


# --------- END OF SCRIPT ---------


