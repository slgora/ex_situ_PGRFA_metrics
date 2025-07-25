### Project: Global Crop Conservation Strategies Metrics ###
### Data sources, clean individually and Join when it is possible (Genesys and WIEWS)
#### Set working directory: working directory is assumed to be Code/R_code , following shared folder structure ####

#### Install packages ####
# tidyverse already include tidyr , dplyr, readr, magrittr, stringr, readxl
install.packages("tidyverse")
library(tidyverse)
library(readxl)
install.packages("writexl")
library(writexl)

####################################################################################################
########### Read in all database data for all crops ################################################
BGCI_allcrops <- read_excel("../../Data/BGCIPlantSearch_data/FullReport_counts_and_origin/BGCI_allcrops_unformatted.xlsx")
WIEWS_allcrops <- read_csv("../../Data/FAO_WIEWS/Passport_data/SDGBrequestExp.csv")
Genesys_allcrops <- read_csv("../../Data/Genesys/Data_aggregated_all_selected_GCCS/Genesys_allcrops_unformatted.csv") # Read in as a csv, not excel, helped eliminate data loss
GBIF_allcrops <- read_csv("../../Data/GBIF/Living_records_all_genus_aggregated/GBIF_allcrops_unformatted.csv")

##### read file with country codes, I added na.strings to resolve the problem with NA for Namibia becoming a NaN value
geo_names <- read_csv("../../Data_processing/Support_files/Geographical/geo_names.csv" , na = c("", "-"))
## subset only the relevant column to join- 2 letter country code and the 3 letter abbreviation
geo_names <- subset(geo_names, select = c(country2, country3))
#####  file with institute names and FAO INSTCODE, some synonyms were added to the list 
institute_names <- read_excel("../../Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG_with_synonyms.xlsx")
names(institute_names)[names(institute_names) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names)[names(institute_names) == 'Name of organization'] <- 'Name_of_organization'
institute_names_full <- subset(institute_names, select = c(`INSTCODE`, `Name_of_organization`))  %>% drop_na()
institute_names_no_syn <- read_excel("../../Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")
names(institute_names_no_syn)[names(institute_names_no_syn) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names_no_syn)[names(institute_names_no_syn) == 'Organization authority status'] <- 'ORGANIZATIONTYPE'
institute_names_no_syn <- subset(institute_names_no_syn, select = c(`INSTCODE`, `ORGANIZATIONTYPE`))  %>% drop_na()
WIEWS_institute_IDs <- read_excel("../../Data_processing/Support_files/FAO_WIEWS/WIEWS_instIDs.xlsx")
WIEWS_institute_IDs = subset(WIEWS_institute_IDs, select = c('ID' , 'WIEWS_INSTCODE'))
####################################################################################################
########## Change field names to follow MCPD standard see https://www.fao.org/plant-treaty/tools/toolbox-for-sustainable-use/details/en/c/1367915/ ############################################


############### BGCI Plant Search: Data Read in and Cleaning ####################
# PG notes BGCI country code is country of Botanical Garden not origin country of the plant (it may be the same ? but is it a. valid assumption)
# only useful column seems to be institute name, taxa name in plant search (standardized), and type of germplasm

names(BGCI_allcrops)[names(BGCI_allcrops) == 'Name (in PlantSearch)'] <- 'fullTaxa'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Submitted Name'] <- 'SubmittedName'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Ex Situ Site GardenSearch ID'] <- 'ex_situ_site_gardenSearch_ID' #added
BGCI_allcrops <- cbind(BGCI_allcrops, data_source = "BGCI") # Add field: data source

# Separate fields: fullSciName, still have fullTaxa (which is the fullSciName standardized by BGCI )
# Split the "fullSciName" column into "genus" and "species" without removing "fullSciName"
BGCI_allcrops <- BGCI_allcrops %>%
  mutate(GENUS   = word(fullTaxa, 1),  # Extract the first word (genus)
         SPECIES = word(fullTaxa, 2))  # Extract the second word (species)

# when  fullTaxa is empty fill it with SubmittedName
BGCI_allcrops <- BGCI_allcrops %>%
  mutate(fullTaxa = ifelse(is.na(fullTaxa) | fullTaxa == "", SubmittedName, fullTaxa))

### Encode all fields relevant to storage fields
BGCI_allcrops['Germplasm, seed'][BGCI_allcrops['Germplasm, seed'] == 1] <- 10
BGCI_allcrops['Germplasm, seed'][BGCI_allcrops['Germplasm, seed'] == 0] <- NA
BGCI_allcrops['Germplasm, plant'][BGCI_allcrops['Germplasm, plant'] == 1] <- 20
BGCI_allcrops['Germplasm, plant'][BGCI_allcrops['Germplasm, plant'] == 0] <- NA
BGCI_allcrops['Germplasm, pollen'][BGCI_allcrops['Germplasm, pollen'] == 1] <- 99
BGCI_allcrops['Germplasm, pollen'][BGCI_allcrops['Germplasm, pollen'] == 0] <- NA
BGCI_allcrops['Germplasm, explant'][BGCI_allcrops['Germplasm, explant'] == 1] <- 30
BGCI_allcrops['Germplasm, explant'][BGCI_allcrops['Germplasm, explant'] == 0] <- NA

# Combine all 4 fields into one storage field
BGCI_allcrops$STORAGE <- apply(BGCI_allcrops[, c("Germplasm, seed", "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant")], 1, function(x) paste(na.omit(x), collapse = "; "))

# Drop the specified columns from the BGCI_allcrops data frame
BGCI_allcrops <- select(BGCI_allcrops, -c('Germplasm, seed', "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant"))

# Fields we want to keep
BGCI_allcrops <- subset(BGCI_allcrops, select = c(data_source, fullTaxa, ex_situ_site_gardenSearch_ID, GENUS, SPECIES, STORAGE ))
# Note: you need to create folder DATE_OF_RUN before running the following line of code
write.csv(BGCI_allcrops, '../../Data_processing/1_merge_data/2025_07_07/BGCI_processed.csv', row.names = FALSE)
############### WIEWS: Data Cleaning ####################
#rename all columns according to MCPD naming style, and select columns that are needed
WIEWS_allcrops <- WIEWS_allcrops %>%
  rename_with(~ c("holdingCty", "INSTCODE", "ACCENUMB", "fullTaxa", "GENUS", 
                  "SPECIES", "acceptedGenus", "acceptedSpecies", "CROPNAME", 
                  "ACQDATE", "ORIGCTY", "SAMPSTAT", "DUPLSITE", "DUPLINSTNAME",
                  "DECLATITUDE", "DECLONGITUDE", "COLLSRC", "STORAGE", 
                  "MLSSTAT", "DOI")) %>%
  select(holdingCty, INSTCODE, ACCENUMB, fullTaxa, GENUS, 
         SPECIES, CROPNAME, ORIGCTY, 
         SAMPSTAT, DUPLSITE, DUPLINSTNAME, DECLATITUDE, DECLONGITUDE, 
         STORAGE, MLSSTAT, 
         ACQDATE, COLLSRC)

# Add field: data source
WIEWS_allcrops <- cbind(WIEWS_allcrops, data_source = "WIEWS")

## Standardize ACCENUMB field: remove blank/space between institute abbreviation and number
WIEWS_allcrops  <- WIEWS_allcrops  %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))

# Split the DUPLSITE column into separate rows, trim spaces, join with WIEWS_institute_IDs conversion table
WIEWS_allcrops <- WIEWS_allcrops %>%
  separate_rows(DUPLSITE, sep = ";") %>%
  mutate(DUPLSITE = str_trim(DUPLSITE)) %>%
  mutate(DUPLSITE = as.integer(DUPLSITE)) %>%
  left_join(WIEWS_institute_IDs, by = c("DUPLSITE" = "ID"), relationship = "many-to-one") 

# Combine rows back into a single row per original entry, with DUPLSITE and WIEWS_INSTCODE values separated by ";"
WIEWS_allcrops <- WIEWS_allcrops %>%
  group_by(across(-c(DUPLSITE, WIEWS_INSTCODE))) %>%
  summarize(
    DUPLSITE = paste(unique(DUPLSITE), collapse = ";"),
    WIEWS_INSTCODE = paste(unique(WIEWS_INSTCODE), collapse = ";"),
    .groups = 'drop'
  )

WIEWS_allcrops <- WIEWS_allcrops %>%
  select(-DUPLSITE) %>%                 #drop DUPLSITE column with wiews IDs
  rename(DUPLSITE = WIEWS_INSTCODE)  # Rename WIEWS_INSTCODE to DUPLSITE

# recode MLSSTAT as TRUE and FALSE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "I"] <-  TRUE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "N"] <-  FALSE
WIEWS_allcrops <- WIEWS_allcrops %>% mutate(MLSSTAT = as.logical(MLSSTAT))

############### Genesys PGR: Data Read in and Cleaning ####################
# select columns to keep
Genesys_allcrops <- subset(Genesys_allcrops, select = c(INSTCODE, ACCENUMB, 
                                                        GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR, 
                                                        GRIN_NAME, CROPNAME, ACQDATE, ACCENAME, SAMPSTAT, 
                                                        DONORCODE, DONORNAME, OTHERNUMB, DONORNUMB, # added for PDCI calc
                                                        ORIGCTY, DECLATITUDE,DECLONGITUDE, ELEVATION,
                                                        BREDCODE, ANCEST, DUPLSITE, STORAGE, 
                                                        COLLDATE, COLLSITE, COLLSRC, COLLNUMB, COLLCODE,
                                                        MLSSTAT, ACCEURL))

# Add field: data source 
Genesys_allcrops <- cbind(Genesys_allcrops, data_source = "Genesys")

##### correcting manually these species names as they occur in a lot of accessions
Genesys_allcrops$SPECIES <- gsub('z.mays', 'mays', Genesys_allcrops$SPECIES)
Genesys_allcrops$SPECIES <- gsub('o.sativa', 'sativa', Genesys_allcrops$SPECIES)
######

# Replace NA values with empty strings for 'subTaxa' and 'spAuthor'
Genesys_allcrops$SUBTAXA <- ifelse(is.na(Genesys_allcrops$SUBTAXA), "", Genesys_allcrops$SUBTAXA)
Genesys_allcrops$SPAUTHOR <- ifelse(is.na(Genesys_allcrops$SPAUTHOR), "", Genesys_allcrops$SPAUTHOR)

# Concatenate Genus, species, 'subTaxa', and 'spAuthor' with spaces in between
Genesys_allcrops$fullTaxa <- trimws(paste(Genesys_allcrops$GENUS, Genesys_allcrops$SPECIES, Genesys_allcrops$SUBTAXA, Genesys_allcrops$SPAUTHOR))

## Standardize ACCENUMB field. Remove blank/space between institute abbreviation and number
Genesys_allcrops <- Genesys_allcrops %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))

####################################################################################################
## Combine Genesys and WIEWS data and Remove duplicates between Genesys and WIEWS, keep Genesys ##################################################
gen_wiews_df <- bind_rows(Genesys_allcrops, WIEWS_allcrops)
gen_wiews_df$ACCENUMB <- trimws(gen_wiews_df$ACCENUMB)
gen_wiews_df$INSTCODE <- trimws(gen_wiews_df$INSTCODE)
gen_wiews_df$ID <- paste0(gen_wiews_df$ACCENUMB, gen_wiews_df$INSTCODE)
gen_wiews_df <- gen_wiews_df[!duplicated(gen_wiews_df$ID), ]  # drop duplicates but keep the first occurrence, in this case Genesys

####### correct country codes iso-codes
source("Functions/Correct_country_codes.R")
gen_wiews_df = correct_country_codes(gen_wiews_df, col = 'ORIGCTY')

####### assign organization type ############
source("Functions/Assign_organization_status.R")
gen_wiews_df = assign_org_type(gen_wiews_df, institute_names_no_syn)

# added to drop all Pisum accessions
gen_wiews_df <-gen_wiews_df %>% filter(!grepl("Pisum", fullTaxa))                               

# save results
gen_wiews_df$STORAGE <- as.character(gen_wiews_df$STORAGE)
write.csv(gen_wiews_df, '../../Data_processing/1_merge_data/2025_07_18/gen_wiews_df.csv', row.names = FALSE)
################## GLIS data ########################################################################
##### read all JSON files downloaded from GLIS and extract data 
# create a list of file paths (each one is a Json file downloaded from GLIS)
install.packages("jsonlite")
library("jsonlite")
source("Functions/Extract_results_GLIS_API.R") # added 30May 2025 corrected                              
filenames <- list.files("../../Data/Plant_Treaty/GLIS/", pattern="*.json", full.names=TRUE)

# Read all the downloaded GLIS json file and merge in one single dataframe
li = list()
for (i in filenames) {
  json_data <- read_json(i)
  r = extract_result_GLIS(json_data)
  li[[i]] = r
}
#merge all the extracted dataframes in one single dataframe
all_glis_data <- do.call("rbind", li)

#generate MLSSTAT variable, used later to compute metrics on # of accessions included in the MLS
all_glis_data$MLSSTAT = NA
all_glis_data$MLSSTAT <- ifelse(all_glis_data$MLS %in% c(1, 11, 12, 13, 14, 15), TRUE, all_glis_data$MLSSTAT)
all_glis_data$MLSSTAT <- ifelse(all_glis_data$MLS %in% c(0), FALSE, all_glis_data$MLSSTAT)
# save results
write.csv(all_glis_data, '../../Data_processing/1_merge_data/2025_07_07/GLIS_processed.csv', row.names = FALSE)

################# SGSV data ########################################################################## 
source("Functions/Load_SGSV_data.R")
sgsv = load_SGSV_data('../../Data/SGSV/Deposits_all_genera_aggregated/SGSV_allcrops_unformatted.xlsx')

# create uniqueID and drop duplicates                               
sgsv$ACCENUMB <- trimws(sgsv$ACCENUMB)
sgsv$INSTCODE <- trimws(sgsv$INSTCODE)
sgsv$ID <- paste0(sgsv$ACCENUMB, sgsv$INSTCODE)
sgsv <- sgsv[!duplicated(sgsv$ID), ]  
# save results
write.csv(sgsv, '../../Data_processing/1_merge_data/2025_07_07/SGSV_processed.csv', row.names = FALSE)


################# FAO WIEWS Indicator data ########################################################################## 
# read in FAO WIEWS indicator file and croplist_PG within function 
source("Functions/Load_WIEWS_indicator_data.R") # source function
WIEWS_indicator_proccessed <- process_wiews_indicator_data(
  wiews_path = "../../Data/FAO_WIEWS/Indicator_22_data/FAO_WIEWS_Indicator22.xlsx",
  croplist_path = "../../Data_processing/Support_files/GCCS_Selected_crops/croplist_PG.xlsx"
)
# save results
write.csv(WIEWS_indicator_proccessed, '../../Data_processing/1_merge_data/2025_07_08/WIEWS_indicator_processed.csv', row.names = FALSE)


