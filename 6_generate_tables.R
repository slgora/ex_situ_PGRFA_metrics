# testing script for building out results tables 

library(readr)
library(readr)

# Read in Metrics_and_data_descriptions_table as guide file for table set up
metrics_guide <- read_excel("../../Data_processing/Metrics_and_data_descriptions_table.xlsx")

# Read in metrics calculated:
# note: SG create a function to assist with this data read in from folder
PTFTW_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/PTFTW_metrics.xlsx")
#transfer_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/transfers_metrics_2015_2021.xlsx")
#storage_term_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/storage_term_summary.csv")
#WIEWS_regeneration_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/WIEWS_regeneration_summary.csv")
#char_eval_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/char_eval_summary.csv")
#gbif_count_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/gbif_count_summary.csv")
#pdci_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/pdci_summary.csv")
#BGCI_inst_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/BGCI_inst_count.csv")
#BGCI_taxa_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/BGCI_taxa_count.csv")
#institution_accessions_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/institution_accessions_summary.csv")
#GLIS_MLS_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/GLIS_MLS_count.csv")
#GLIS_dois_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/GLIS_dois_count.csv")
#SGSV_dupl_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/SGSV_dupl_metric.csv")
#### sd_outcountry_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/sd_outcountry_metric.csv")
#storage_summary <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/storage_summary.csv")
#annex1_perc <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/annex1_perc.csv")
#annex1_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/annex1_count.csv")
#mls_by_orgtype <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/mls_by_orgtype.csv")
#accessions_by_org_type <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/accessions_by_org_type.csv")
#diversity_regions_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/diversity_regions_metric.csv")
#primary_region_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/primary_region_metric.csv")
#country_count <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/country_count.csv")
#unique_taxa <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/unique_taxa.csv")
#no_SAMPSTAT_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/no_SAMPSTAT_metric.csv")
#othervar_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/othervar_metric.csv")
#improved_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/improved_metric.csv")
#breeding_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/breeding_metric.csv")
#landrace_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/landrace_metric.csv")
#weedy_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/weedy_metric.csv")
#cwr_metric <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/cwr_metric.csv")
#unique_institutions <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/unique_institutions.csv")
#accession_by_source_crop_strategy <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/accession_by_source_crop_strategy.csv")
#accession_by_crop_strategy <- read_csv("../../Data_processing/4_estimate_metrics/2025_07_14/accession_by_crop_strategy.csv")

# Source function(s)
source("Functions/Generate_results_table1.R")
# source("Functions/Generate_results_table2.R") #didnt push to github yet
# source("Functions/Generate_results_table3.R") #didnt push to github yet
# source("Functions/Generate_results_table4.R") #didnt push to github yet
# source("Functions/Generate_results_table5.R") #didnt push to github yet
# source("Functions/Generate_results_table6.R") #didnt push to github yet
# source("Functions/Generate_results_table7.R") #didnt push to github yet

# note: Create a folder for date of run to export results tables 

# ---------- Table 1 ------------
# Filter guide file for Table 1 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 1)
# Fun function to generate table 1
table1_by_crop <- generate_table1(1, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table1_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_14/Table1_all_crops.xlsx")


# note: script for Tables 2- Table 7 not pushed to Github yet
