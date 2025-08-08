# -------------------------------------------------------------------------------------------
# PTFTW and Treaty Transfer Indicators
# 
# This script processes and summarizes two key metric blocks:
# 1. Plants That Feed the World (PTFTW) indicator metrics — combining digital, trade, research,
#    and use indicators for selected crops.
# 2. Germplasm transfer metrics under the ITPGRFA Treaty — calculating distribution statistics
#    and equity indicators (sample averages, country counts, and Gini inequality).
# Outputs: Two Excel files with crop strategy-level metrics, ready for reporting.
####  working directory: working directory should be Code/R_code , following shared folder structure ####
# -------------------------------------------------------------------------------------------

# load libraries
install.packages("writexl")
library(writexl)


### 19. PTFTW metrics (56 metrics)
# source function to process PTFTW dataset and metrics
# note: will need to update local paths within function to work properly
source("Functions/Load_PTFTW_dataset.R")

## Data read in: Plants That Feed the World dataset and croplist
indicator_file <- read_csv("../../Data/Plants_that_feed_the_world/Indicators/indicator_average.csv")
croplist <- read_excel("../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx")

# Run function, specify indicator file to process and croplist, output file is saved
# add folder with date of ouput run
PTFTW_metrics_processed <- process_PTFTW_metrics(
  indicator_file = "../../Data/Plants_that_feed_the_world/Indicators/indicator_average.csv",
  croplist = "../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx",
  out_path = "../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_24/PTFTW_metrics.xlsx"
)


## 20. Transfer metrics calculations (3 metrics)
# source function to process transfers datasets
# note: will need to update local paths within function to work properly
source("Functions/Process_transfers.R")

# Data read in, transfers (2012-2019), transfers (2019-2021), country regions, croplist already read in above
# note: function filters the older transfers dataset to include only records from 2015–2018, 
#       excluding earlier years and post-2019 entries to avoid duplication with newer 2019–2021 dataset
transfers_2012_2019 <- read_excel("../../Data/Plant_Treaty/Data_store/ITPGRFA_MLSDataStore2022_7_1.xlsx")
transfers_2019_2021 <- read_excel("../../Data/Plant_Treaty/Data_store/Transfers_ourcrops_2019-2021.xlsx")
countries_regions <- read_excel("../../Data_processing/Support_files/Geographical/countries_in_regions.xlsx")

# Run function, specify croplist and output file is saved
# add folder with date of ouput run
transfer_metrics <- transfers_metrics(
  croplist_file = "../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx",
  out_path = "../../Data_processing/5_PTFTW_processing_and_metrics/2025_07_14/transfers_metrics_2015_2021.xlsx"
)

