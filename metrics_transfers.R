
# ---- Libraries ----
library(tidyverse)
library(readxl)
library(ineq)
library(writexl)

# ---- File paths (edit as needed) ----
ptftw_file <- "ITPGRFA_MLS-Data-Store_2022_7_1.xlsx"    #SG fix these files paths with correct names in Drive
croplist_file <- "croplist.xlsx"
transfers_2019_2021_file <- "Transfers/Transfers_ourcrops_2019-2021.xlsx"
countries_regions_file <- "Data/CropRegions_PlantsThatFeedtheWorld/countries_in_regions.xlsx"

# ---- Read and Clean Data ----

# 2015-2018 data
itpgrfa <- read_csv(ptftw_file) %>%
  filter(Year >= 2015 & Year <= 2018 & Dataset != "CGIAR only") %>%
  select(provider_ISO3, Provider_country, Crop_cleaned, Taxonomic_name, Year, Samples, ISO3, Recipient_country) %>%
  rename(
    provider_country_name = Provider_country,
    recipient_ISO3 = ISO3,
    year = Year,
    recipient_country_name = Recipient_country,
    number_of_samples = Samples,
    PlantsthatFeedtheWorld_name = Crop_cleaned
  )

# Croplist join
croplist <- read_excel(croplist_file) %>%
  select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>%
  filter(PlantsthatFeedtheWorld_name != "NA") %>%
  distinct(PlantsthatFeedtheWorld_name, .keep_all = TRUE)

itpgrfa <- left_join(croplist, itpgrfa, by = "PlantsthatFeedtheWorld_name") %>%
  select(CropStrategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples) %>%
  rename(crop_strategy = CropStrategy) %>%
  filter(!is.na(number_of_samples))

# 2019-2021 data
transfers_2019_2021 <- read_excel(transfers_2019_2021_file) %>%
  select(crop_strategy, year, provider_ISO3, provider_country_name, recipient_ISO3, recipient_country_name, number_of_samples)

# ---- Combine Data ----
transfers <- bind_rows(transfers_2019_2021, itpgrfa)

# ---- Metrics ----

# 1. Average number of samples per year by crop
df_avg_final <- transfers %>%
  group_by(crop_strategy, year) %>%
  summarize(avg_number_of_samples = mean(number_of_samples), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_samples_per_year = mean(avg_number_of_samples), .groups = "drop")

# 2. Average number of recipient countries per year by crop
df_avg_recipient_counts <- transfers %>%
  group_by(crop_strategy, year) %>%
  summarize(number_of_recipient_countries = n_distinct(recipient_ISO3), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(avg_number_of_recipient_countries = mean(number_of_recipient_countries), .groups = "drop")

# 3. Gini index for recipient regions by crop
countries_in_regions <- read_excel(countries_regions_file) %>%
  select(recipient_ISO3 = Country_code, regions = PlantsThatFeedTheWorld_Region_new)

transfers_regions <- left_join(transfers, countries_in_regions, by = "recipient_ISO3") %>%
  separate_rows(regions, sep = ",") %>%
  mutate(regions = trimws(regions))

gini_results <- transfers_regions %>%
  group_by(crop_strategy, regions) %>%
  summarize(total_samples = sum(number_of_samples, na.rm = TRUE), .groups = "drop") %>%
  group_by(crop_strategy) %>%
  summarize(gini_index = ineq::Gini(total_samples), .groups = "drop")

# ---- Combine All Metrics ----
transfers_metrics_2015_2021 <- df_avg_final %>%
  left_join(df_avg_recipient_counts, by = "crop_strategy") %>%
  left_join(gini_results, by = "crop_strategy") %>%
  rename(
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty" = avg_number_of_samples_per_year,
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty" = avg_number_of_recipient_countries,
    "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty" = gini_index
  )

# ---- Save to File ----
write_xlsx(transfers_metrics_2015_2021, transfers_metrics2025_06_27)  # add date output file created at end, YYYY_MM_DD
