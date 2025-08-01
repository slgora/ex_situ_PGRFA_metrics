######## use conversion table with validated taxa for taxa standardisation, 
## do the processing that comes after standardization,as these depend on the correct genus and species #########

###### 1. Load datasets with taxa to be standardized using conversion table  #######################
crops <- read_excel("../../Data_processing/Support_files/GCCS_selected_crops/croplist_PG.xlsx")
combined_df <- read_csv("../../Data_processing/1_merge_data/2025_07_18/gen_wiews_df.csv")
SGSV_allcrops <- read_csv("../../Data_processing/1_merge_data/2025_07_07/SGSV_processed.csv") 
GLIS_processed <- read_csv('../../Data_processing/1_merge_data/2025_07_07/GLIS_processed.csv')
BGCI_processed <- read_csv('../../Data_processing/1_merge_data/2025_07_07/BGCI_processed.csv')

# create a copy of fullTaxa without some characters that gave problems when using the conversion table
combined_df$fullTaxa2 <- trimws(
  gsub("\\s+", " ",
       gsub("\\?", "",
            gsub("\\+", " ",
                      na.omit(combined_df$fullTaxa)
                 )
            )
       )
  )

#### add standardized_taxa column here when the manual vetting is complete and standardization_table becomes available, code to be tested#######
#load standardization table
table_standardization <- read_excel("../../Data_processing/Support_files/Taxa_standardization/SG_standardization_table_WFO_GRIN_2025_07_01.xlsx")

# Replace tabs with space in input_name
table_standardization$input_name <- gsub("\t", " ", table_standardization$input_name)

#Replace multiple spaces with one space 
table_standardization$input_name <- gsub("\\s+", " ", table_standardization$input_name)

#Add a space after period, "." between words in PG_recommendation
table_standardization$PG_recommendation <- gsub("(?<=[a-zA-Z])\\.(?=[a-zA-Z])", ". ", table_standardization$PG_recommendation, perl = TRUE)

# Create named vector: input_name -> PG_recommendation
standardization_table <- setNames(table_standardization$PG_recommendation, table_standardization$input_name)

combined_df$Standardized_taxa = NA
combined_df <- combined_df %>%
    mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa2], Standardized_taxa))

BGCI_processed$Standardized_taxa = NA
BGCI_processed <- BGCI_processed %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa), standardization_table[fullTaxa], Standardized_taxa))

# NOTE in SGSV dataset and GLIS dataset taxa were not yet standardized

###### 2. Assign crop strategy categorical variable #####################
# use Assign_crop_strategy.R function in Functions folder
source("Functions/Assign_crop_strategy.R")

combined_df <- combined_df %>%
  mutate(GENUS_standardized = word(!!sym("Standardized_taxa"), 1)) %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS_standardized") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

SGSV_allcrops <- SGSV_allcrops %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

GLIS_processed <- GLIS_processed %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

BGCI_processed <- BGCI_processed %>%
  mutate(GENUS_standardized = word(!!sym("Standardized_taxa"), 1)) %>%
  assign_crop_strategy(crops = crops, col_name = "GENUS_standardized") %>%
  filter(!is.na(Crop_strategy))  # remove records where Crop_strategy is NA

###### 3. Assign diversity region               ######################
# It requires crops dataframe (croplist_PG.xlsx) and countries in regions (countries_in_regions.xlsx)
source("Functions/Assign_diversity_regions.R")
countries_in_regions <- read_excel("../../Data_processing/Support_files/Geographical/countries_in_regions.xlsx")
combined_df = assign_diversity_regions(combined_df, crops = crops, countries_in_regions = countries_in_regions)

###### 4. Assign Annex 1 status                 ######################
# in the function assign_annex1status one needs to change the path for the file containing the list of Petota and Melongena species
# function taking a dataframe including a column taxa names and returning TRUE/FALSE , 
source("Functions/Assign_annex1_status.R")
combined_df = assign_annex1status(combined_df, standardize_taxa = 'Standardized_taxa')  # assumed the column with standardized taxa is named Standardized_taxa


##### save resulting datasets in folder: ../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/
write.csv(combined_df, '../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_18/combined_df.csv', row.names = FALSE)
write.csv(SGSV_allcrops, '../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/SGSV_processed.csv', row.names = FALSE)
write.csv(GLIS_processed, '../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/GLIS_processed.csv', row.names = FALSE)
write.csv(BGCI_processed, '../../Data_processing/3_post_taxa_standardization_processing/Resulting_datasets/2025_07_07/BGCI_processed.csv', row.names = FALSE)
