library(readr)

# Read in Metrics_and_data_descriptions_table as guide file for table set up
metrics_guide <- read_excel("../../Data_processing/Metrics_and_data_descriptions_table.xlsx")
# Read in PTFTW and transfer metrics calculated
PTFTW_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/PTFTW_metrics.xlsx")
transfer_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/transfers_metrics_2015_2021.xlsx")
# Read in all other metrics file
all_metrics <- "../../Data_processing/4_estimate_metrics/2025_07_18/all_metrics_summary.xlsx"
sheet_names <- getSheetNames(all_metrics)
all_metrics <- setNames(      #Read all sheets into a named list so can call to tables
  lapply(sheet_names, function(s) read.xlsx(all_metrics, sheet = s)),
  sheet_names)

# Source function(s)
source("Functions/Generate_results_table1.R")
source("Functions/Generate_results_table2.R")
source("Functions/Generate_results_table3.R")
source("Functions/Generate_results_table4.R")
source("Functions/Generate_results_table5.R")
source("Functions/Generate_results_table6.R")
source("Functions/Generate_results_table7.R")

# note: Create a folder for date of run to export results tables 

# ---------- Table 1 ------------
# Filter guide file for Table 1 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 1)
# Fun function to generate table 1
table1_by_crop <- generate_table1(1, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table1_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_17/Table1_all_crops.xlsx")

# ---------- Table 2 ------------
# Extract metric needed for Table 2
institution_accessions_summary <- all_metrics$institution_accessions_summary
# Run function to generate table 2
table2_by_crop <- generate_table2(institution_accessions_summary)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table2_by_crop, path = "../../GCCS metrics project shared folder/Data_processing/6_generate_tables/2025_07_18/Table2_all_crops.xlsx")

# ---------- Table 3 ------------
# Run function to generate table 3
table3_by_crop <- generate_table3(tbl_number = 3, metrics_guide = metrics_guide, all_metrics_df = all_metrics_df)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table3_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_17/Table3_all_crops.xlsx")

# ---------- Table 4 ------------
# Create list of metrics for table 4
metric_dfs <- list(
  accessions_by_org_type = all_metrics$accessions_by_org_type,
  annex1_count           = all_metrics$annex1_count,
  annex1_perc            = all_metrics$annex1_perc,
  GLIS_dois_count        = all_metrics$GLIS_dois_count,
  GLIS_MLS_count         = all_metrics$GLIS_MLS_count,
  mls_by_orgtype         = all_metrics$mls_by_orgtype)
# Run function to generate table 4
table4_by_crop <- generate_table4(metrics_guide, metric_dfs)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table4_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_17/Table4_all_crops.xlsx")

# ---------- Table 5 ------------
# Create list of metrics for table 5
metric_dfs <- list(
  storage_summary            = all_metrics$storage_summary,
  storage_term_summary       = all_metrics$storage_term_summary,
  WIEWS_regeneration_summary = all_metrics$WIEWS_regeneration_summary,
  # sd_outcountry_metric     = sd_outcountry_metric,              #see issue
  SGSV_dupl_metric           = all_metrics$SGSV_dupl_metric)
# Run function to generate table 5
table5_by_crop <- generate_table5(metrics_guide, metric_dfs)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table5_by_crop, path = "../../GCCS metrics project shared folder/Data_processing/6_generate_tables/2025_07_17/Table5_all_crops.xlsx")

# ---------- Table 6 ------------
# Create list of metrics for table 6
metric_dfs_table6 <- list(
  pdci_summary        = all_metrics$pdci_summary,
  gbif_count_summary  = all_metrics$gbif_count_summary,
  PTFTW_metrics       = PTFTW_metrics)
# Run function to generate table 6
table6_by_crop <- generate_table6(metrics_guide, metric_dfs_table6)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table6_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_17/Table6_all_crops.xlsx")

# ---------- Table 7 ------------
# Create list of metrics for table 7
metric_dfs <- list(
  PTFTW_metrics      = PTFTW_metrics,
  transfer_metrics  = transfer_metrics)
# Run function to generate table 7
table7_by_crop <- generate_table7(metrics_guide, metric_dfs)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table7_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_17/Table7_all_crops.xlsx")
