# testing script for building out results tables 


# Read in Metrics and data descriptions table as guide file for table set up
metrics_guide <- read_excel("Metrics_and_data_descriptions_table2025_06_27.xlsx")

# SG note: Read in all metrics in a separate file here 
#          or include table generation at end of 4_Estimate_metrics ?

# Source function(s)
source("Functions/Generate_results_table1.R")
# source("Functions/Generate_results_table2.R")


# ---------- Table 1 ------------
# Filter guide file for Table 1 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 1)
# Fun function to generate table 1
table1_by_crop <- generate_table1(1, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
write_xlsx(table1_by_crop, path = "Table1_all_crops.xlsx")


# ---------- Table 2 ------------
# Filter guide file for Table 2 metric names and set up info
filtered_guide <- metrics_guide %>% filter(`Pertains to Table` == 2)
# Fun function to generate table 2
table2_by_crop <- generate_table2(2, PTFTW_metrics, filtered_guide)
# Export all crop tables into one Excel file with each crop as a tab
# write_xlsx(table2_by_crop, path = "Table2_all_crops.xlsx")
