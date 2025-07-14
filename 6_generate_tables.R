####  Script for building out results tables
####  working directory: working directory should be Code/R_code , following shared folder structure ####

# Read in Metrics and data descriptions table as guide file for table set up
metrics_guide <- read_excel("../../Data_processing/Metrics_and_data_descriptions_table.xlsx")
PTFTW_metrics <- read_excel("../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/PTFTW_metrics.xlsx")

source("Functions/Generate_results_table1.R")
# source("Functions/Generate_results_table2.R") PG: this function is not yet in the Function folder


# ---------- Table 1 ------------
# Filter guide file for Table 1 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 1)
# Function to generate table 1
table1_by_crop <- generate_table1(1, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table1_by_crop, path = "../../Data_processing/6_generate_tables/DATE/Table1_all_crops2025_06_27.xlsx") # add date table generated

# ---------- Table 2 ------------
# Filter guide file for Table 2 metric names and set up info
filtered_guide_table_2 <- metrics_guide %>% filter(`Pertains to Table` == 2)
#Function to generate table 2
#table2_by_crop <- generate_table2(2, PTFTW_metrics, filtered_guide_table_2)
# Export all crop tables into one Excel file with each crop as a tab
#write_xlsx(table2_by_crop, path = "../Data/Data_processing/6_generate_tables/DATE/Table2_all_crops.xlsx")
