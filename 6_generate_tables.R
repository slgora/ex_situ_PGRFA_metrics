# testing script for building out results tables 
library(readr)

# Read in Metrics_and_data_descriptions_table as guide file for table set up
metrics_guide <- read_excel("../../Data_processing/Metrics_and_data_descriptions_table.xlsx")

# Read in metrics calculated
PTFTW_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/PTFTW_metrics.xlsx")
transfer_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/transfers_metrics_2015_2021.xlsx")
all_metrics <- read_excel("../../Data_processing/4_estimate_metrics/2025_07_15/all_metrics_summary.xlsx")

# Source function(s)
source("Functions/Generate_results_table1.R")
# source("Functions/Generate_results_table2.R") #didnt push to github yet
# source("Functions/Generate_results_table3.R") #didnt push to github yet
# source("Functions/Generate_results_table4.R") #didnt push to github yet
# source("Functions/Generate_results_table5.R") #didnt push to github yet
# source("Functions/Generate_results_table6.R") #didnt push to github yet
source("Functions/Generate_results_table7.R")

# note: Create a folder for date of run to export results tables 

# ---------- Table 1 ------------
# Filter guide file for Table 1 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 1)
# Fun function to generate table 1
table1_by_crop <- generate_table1(1, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table1_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_15/Table1_all_crops.xlsx")


# note: script for Tables 2-6 not pushed to Github yet


# ---------- Table 7 ------------
# Create list of metrics for table 7
metric_dfs <- list(
  PTFTW_metrics      = PTFTW_metrics,
  transfer_metrics  = transfer_metrics
)
# Run function to generate table 7
table7_by_crop <- generate_table7(metrics_guide, metric_dfs)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table7_by_crop, path = "../../Data_processing/6_generate_tables/2025_07_15/Table7_all_crops.xlsx")
