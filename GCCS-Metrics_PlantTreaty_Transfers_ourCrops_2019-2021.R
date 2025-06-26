### Project: Global Crop Conservation Strategies Metrics ###
### Data sources cleaning individually and join
### GCCS-Metrics_PlantTreaty_Transfers_ourCrops_2019-2021
### by Sarah Gora
### Date created: 2024_12_16
### Date updated: 2025_06_25

#### Read in packages ####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)


## data read in from PTFTW data 2012-2019
ITPGRFA_Data_Store_2012_2019 <- read_csv("C:/Users/sarah/Desktop/GCCS-Metrics/Data/Transfers/Copy of ITPGRFA_MLS-Data-Store_2022_7_1.xlsx - smta.csv")

# Filter out rows where Year is less than 2018 but greater than 2015 and Dataset is equal to "CGIAR only"
# use data 2015-2018 and "all transfers" data
ITPGRFA_Data_Store_2015_2018 <- ITPGRFA_Data_Store_2012_2019 %>%
  filter(Year >= 2015 & Year <= 2018 & Dataset != "CGIAR only")

# subset relevant columns and rename 
ITPGRFA_Data_Store_2015_2018 <- ITPGRFA_Data_Store_2015_2018 %>%
  select("provider_ISO3", "Provider country", "Crop_cleaned", 
         "Taxonomic name", "Year","Samples","ISO3","Recipient country") %>%
  rename("provider_country_name"="Provider country",
          "recipient_ISO3" = "ISO3",
         "year"="Year",
         "recipient_country_name" = "Recipient country",
         "number_of_samples"="Samples")

#### Read in croplist to fill out fields in ITPGRFA_Data_Store_2015_2018
croplist <- read_excel("C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/croplist_PG.xlsx")

# Subset relevant columns and drop rows with NA in PlantsthatFeedtheWorld_name 
PlantsthatFeedtheWorld_ourcrops <- croplist %>% 
  select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>% 
  filter(PlantsthatFeedtheWorld_name != "NA")

# rename crop field in ITPGRFA_Data_Store_2015_2018 dataset to PlantsthatFeedtheWorld_name
ITPGRFA_Data_Store_2015_2018 <- ITPGRFA_Data_Store_2015_2018 %>% 
  rename(PlantsthatFeedtheWorld_name = Crop_cleaned)


#### code below to remove error in joining

# Remove duplicates from PlantsthatFeedtheWorld_ourcrops based on PlantsthatFeedtheWorld_name
PlantsthatFeedtheWorld_ourcrops_unique <- PlantsthatFeedtheWorld_ourcrops %>%
  distinct(PlantsthatFeedtheWorld_name, .keep_all = TRUE)

# Perform the left join with unique values
ITPGRFA_Data_Store_2015_2018 <- PlantsthatFeedtheWorld_ourcrops_unique %>%
  left_join(ITPGRFA_Data_Store_2015_2018, by = "PlantsthatFeedtheWorld_name")

# Keep relevant columns and rename to join with Plant Treaty Transfers 2019-2021
ITPGRFA_Data_Store_2015_2018 <- ITPGRFA_Data_Store_2015_2018 %>%
  select(CropStrategy, year, provider_ISO3, provider_country_name, 
         recipient_ISO3, recipient_country_name, number_of_samples) %>%
  rename(crop_strategy = CropStrategy)

# drop all rows with number of samples = NA
# PTFTW crop name with no data for this dataset
ITPGRFA_Data_Store_2015_2018 <- ITPGRFA_Data_Store_2015_2018 %>% 
   filter(!is.na(number_of_samples))



## Data read in: 2019-2021 transfer data from Plant Treaty
# read in Transfer file from Plant Treaty, filtered by our crops and date range 2019-2021
Transfers_ourcrops_2019_2021 <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/Transfers/Transfers_ourcrops_2019-2021.xlsx")

# subset relevant columns
Transfers_ourcrops_2019_2021 <- Transfers_ourcrops_2019_2021 %>% 
  select(crop_strategy, year, provider_ISO3, provider_country_name, 
         recipient_ISO3, recipient_country_name, number_of_samples)

###### INTEGRATE 2019-2021 (newest Transfers data) called Transfers_ourcrops_2019_2021
#    with 2015-2018 data called ITPGRFA_Data_Store_2015_2018
transfers_2015_2021 <- bind_rows(Transfers_ourcrops_2019_2021, ITPGRFA_Data_Store_2015_2018)

# Fill in crop_strategy with data from CropStrategy if crop_strategy is not filled out
transfers_2015_2021 <- transfers_2015_2021 %>%
  mutate(crop_strategy = ifelse(is.na(crop_strategy), CropStrategy, crop_strategy))

## make new column, avg_number_of_samples_per_year
## this is the PTFTW column:
## demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty
## average per year by crop 

# subset relevant columns for this metric
transfers_2015_2021 <- transfers_2015_2021 %>% 
  select(crop_strategy, year, number_of_samples)

# Calculate the average number of samples per year grouped by crop_strategy
df_avg <- transfers_2015_2021 %>%
  group_by(crop_strategy, year) %>%
  summarize(avg_number_of_samples = mean(number_of_samples)) %>%
  ungroup()

# Calculate the average number of samples per year for each crop_strategy
df_avg_final <- df_avg %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_samples_per_year = mean(avg_number_of_samples))

# Merge back to the original dataframe if you want to keep all columns
# Transfers_ourcrops_2019_2021_new <- merge(Transfers_ourcrops_2019_2021, df_avg_final, by = "crop_strategy", all.x = TRUE)





## make new column, number_of_recipient_countries 
## this is the PTFTW column: 
## demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty
## Count number of unique countries, and average across years by crop

# subset relevant columns for this metric
transfers_2015_2021 <- bind_rows(Transfers_ourcrops_2019_2021, ITPGRFA_Data_Store_2015_2018)
transfers_2015_2021 <- transfers_2015_2021 %>%
  mutate(crop_strategy = ifelse(is.na(crop_strategy), CropStrategy, crop_strategy))

transfers_2015_2021 <- transfers_2015_2021 %>% 
  select(crop_strategy, year, recipient_ISO3)

# Count the number of unique recipient countries by crop_strategy and year
df_recipient_counts <- transfers_2015_2021 %>%
  group_by(crop_strategy, year) %>%
  summarize(number_of_recipient_countries = n_distinct(recipient_ISO3)) %>%
  ungroup()

# Calculate the average number of recipient countries per year by crop_strategy
df_avg_recipient_counts <- df_recipient_counts %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_recipient_countries = mean(number_of_recipient_countries))


# Merge back to the original dataframe if you want to keep all columns
# Transfers_ourcrops_2019_2021 <- merge(Transfers_ourcrops_2019_2021, df_avg_recipient_counts, by = "crop_strategy", all.x = TRUE)



## make new column, gini_index_recipient_regions
## this is the PTFTW column:
## demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty
## gini index per year by crop 


# subset relevant columns for this metric
# subset relevant columns for this metric
transfers_2015_2021 <- bind_rows(Transfers_ourcrops_2019_2021, ITPGRFA_Data_Store_2015_2018)
transfers_2015_2021 <- transfers_2015_2021 %>%
  mutate(crop_strategy = ifelse(is.na(crop_strategy), CropStrategy, crop_strategy))

transfers_2015_2021 <- transfers_2015_2021 %>% 
  select(crop_strategy, recipient_ISO3, number_of_samples)

## assign regions to recipient countries in Transfers data
## read in guidefile for what countries are in what regions 
countries_in_regions <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/CropRegions_PlantsThatFeedtheWorld/countries_in_regions.xlsx")

# subset relevant columns to join and renames columns 
countries_in_regions <- countries_in_regions %>% 
  select(Country_code, PlantsThatFeedTheWorld_Region_new) %>%
  rename(
    recipient_ISO3 = Country_code,
    regions = PlantsThatFeedTheWorld_Region_new)

# Left join to assign regions to recipient countries 
transfers_2015_2021_regions <- transfers_2015_2021 %>%
  left_join(countries_in_regions, by = "recipient_ISO3")




####### Gini index, TRY 2
install.packages("ineq")
library(ineq)

#### solution to problem with multiple regions for one country

# Read in dataset: Transfers_ourcrops_2019_2021_regions 

# Separate multiple regions into different rows
df_separated <- transfers_2015_2021_regions %>%
  separate_rows(regions, sep = ",")

# Trim whitespace from the regions column
df_separated$regions <- trimws(df_separated$regions)

# Group by crop_strategy and regions, and sum the number of samples
df_grouped <- df_separated %>%
  group_by(crop_strategy, regions) %>%
  summarize(total_samples = sum(number_of_samples)) %>%
  ungroup()

# Calculate the Gini index for each crop_strategy
gini_results <- df_grouped %>%
  group_by(crop_strategy) %>%
  summarize(gini_index = ineq::Gini(total_samples))



### join all 3 metrics together 
transfers_metrics_2015_2021 <- df_avg_final%>% 
  left_join(df_avg_recipient_counts, by = "crop_strategy") %>%
  left_join(gini_results , by = "crop_strategy")


## rename columns to be PTFTW data columns 
transfers_metrics_2015_2021 <-transfers_metrics_2015_2021 %>%
  rename(
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty" = "avg_number_of_samples_per_year",
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty" = "avg_number_of_recipient_countries",
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty" = "gini_index"
 )

# save file
write_xlsx(transfers_metrics_2015_2021, "transfers_metrics_2015_2021_06-25-2025.xlsx")

