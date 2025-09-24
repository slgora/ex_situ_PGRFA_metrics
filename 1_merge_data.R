### Project: Global Crop Conservation Strategies Metrics ###
### Part II: New 15 Crops
### Data sources, clean individually and Join when it is possible (Genesys and WIEWS)
#### Set working directory: working directory is assumed to be Code/R_code , following shared folder structure ####

#### Install packages ####
# tidyverse already include tidyr , dplyr, readr, magrittr, stringr, readxl
install.packages("tidyverse")
library(tidyverse)
library(readxl)
install.packages("writexl")
library(writexl)
library(dplyr)

####################################################################################################
########### Read in all database data for all crops ################################################
BGCI_new15crops <- read_csv("../../GCCSmetricsII/Data/BGCIPlantSearch_data/BGCI_new15crops_unformatted.csv")
WIEWS_new15crops <- read_csv("../../GCCS metrics project shared folder/GCCSmetricsII/Data/FAO_WIEWS/Passport_data/all_crops_aggregated/WIEWS_new15crops.csv")
Genesys_new15crops <- read_csv("../../GCCS metrics project shared folder/GCCSmetricsII/Data/Genesys/Data_aggregated_all_selected_GCCS/Genesys_new15crops_unformatted.csv")

##### read file with country codes, I added na.strings to resolve the problem with NA for Namibia becoming a NaN value
geo_names <- read_csv("../../GCCSmetricsII/Data_processing/Support_files/Geographical/geo_names.csv", na = c("", "-"))
## subset only the relevant column to join- 2 letter country code and the 3 letter abbreviation
geo_names <- subset(geo_names, select = c(country2, country3))
#####  file with institute names and FAO INSTCODE, some synonyms were added to the list
institute_names <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG_with_synonyms.xlsx")
names(institute_names)[names(institute_names) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names)[names(institute_names) == 'Name of organization'] <- 'Name_of_organization'
institute_names_full <- subset(institute_names, select = c(`INSTCODE`, `Name_of_organization`))  %>% drop_na()
institute_names_no_syn <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")
names(institute_names_no_syn)[names(institute_names_no_syn) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names_no_syn)[names(institute_names_no_syn) == 'Organization authority status'] <- 'ORGANIZATIONTYPE'
institute_names_no_syn <- subset(institute_names_no_syn, select = c(`INSTCODE`, `ORGANIZATIONTYPE`))  %>% drop_na()
WIEWS_institute_IDs <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/FAO_WIEWS/WIEWS_instIDs.xlsx")
WIEWS_institute_IDs = subset(WIEWS_institute_IDs, select = c('ID' , 'WIEWS_INSTCODE'))
#read file to select institution and data source
data_source <- read_excel("../../GCCSmetricsII/Data_processing/Support_files/Source_selection/selection_data_sources.xlsx")

####################################################################################################
########## Change field names to follow MCPD standard see https://www.fao.org/plant-treaty/tools/toolbox-for-sustainable-use/details/en/c/1367915/ ############################################


############### BGCI Plant Search: Data Read in and Cleaning ####################
# PG notes BGCI country code is country of Botanical Garden not origin country of the plant (it may be the same ? but is it a. valid assumption)
# only useful column seems to be institute name, taxa name in plant search (standardized), and type of germplasm
names(BGCI_new15crops)[names(BGCI_new15crops) == 'Name (in PlantSearch)'] <- 'fullTaxa'
names(BGCI_new15crops)[names(BGCI_new15crops) == 'Submitted Name'] <- 'SubmittedName'
names(BGCI_new15crops)[names(BGCI_new15crops) == 'Ex Situ Site GardenSearch ID'] <- 'ex_situ_site_gardenSearch_ID' #added
BGCI_new15crops <- cbind(BGCI_new15crops, data_source = "BGCI") # Add field: data source

# Separate fields: fullSciName, still have fullTaxa (which is the fullSciName standardized by BGCI )
# Split the "fullSciName" column into "genus" and "species" without removing "fullSciName"
BGCI_new15crops <- BGCI_new15crops %>%
  mutate(GENUS   = word(fullTaxa, 1),  # Extract the first word (genus)
         SPECIES = word(fullTaxa, 2))  # Extract the second word (species)

# when  fullTaxa is empty fill it with SubmittedName
BGCI_new15crops <- BGCI_new15crops %>%
  mutate(fullTaxa = ifelse(is.na(fullTaxa) | fullTaxa == "", SubmittedName, fullTaxa))

### Encode all fields relevant to storage fields
BGCI_new15crops['Germplasm, seed'][BGCI_new15crops['Germplasm, seed'] == 1] <- 10
BGCI_new15crops['Germplasm, seed'][BGCI_new15crops['Germplasm, seed'] == 0] <- NA
BGCI_new15crops['Germplasm, plant'][BGCI_new15crops['Germplasm, plant'] == 1] <- 20
BGCI_new15crops['Germplasm, plant'][BGCI_new15crops['Germplasm, plant'] == 0] <- NA
BGCI_new15crops['Germplasm, pollen'][BGCI_new15crops['Germplasm, pollen'] == 1] <- 99
BGCI_new15crops['Germplasm, pollen'][BGCI_new15crops['Germplasm, pollen'] == 0] <- NA
BGCI_new15crops['Germplasm, explant'][BGCI_new15crops['Germplasm, explant'] == 1] <- 30
BGCI_new15crops['Germplasm, explant'][BGCI_new15crops['Germplasm, explant'] == 0] <- NA

# Combine all 4 fields into one storage field
BGCI_new15crops$STORAGE <- apply(BGCI_new15crops[, c("Germplasm, seed", "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant")], 1, function(x) paste(na.omit(x), collapse = "; "))

# Drop the specified columns from the BGCI_new15crops data frame
BGCI_new15crops <- select(BGCI_new15crops, -c('Germplasm, seed', "Germplasm, plant", "Germplasm, pollen", "Germplasm, explant"))

# Fields we want to keep
BGCI_new15crops <- subset(BGCI_new15crops, select = c(data_source, fullTaxa, ex_situ_site_gardenSearch_ID, GENUS, SPECIES, STORAGE ))
# Note: you need to create folder DATE_OF_RUN before running the following line of code
write.csv(BGCI_new15crops, "../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_24/BGCI_new15crops_processed.csv", row.names = FALSE)

############### WIEWS: Data Cleaning ####################
#rename all columns according to MCPD naming style, and select columns that are needed
WIEWS_new15crops <- WIEWS_new15crops %>%
  rename(
    holdingCty     = `Country name`,
    INSTCODE       = `Holding institute code`,
    ACCENUMB       = `Accession number`,
    fullTaxa       = `Taxon`,
    GENUS          = `Genus`,
    SPECIES        = `Species`,
    acceptedGenus  = `Accepted Genus`,
    acceptedSpecies= `Accepted Species`,
    CROPNAME       = `Crop name`,
    ACQDATE        = `Acquisition date (YYYY/MM)`,
    ORIGCTY        = `Country of origin`,
    SAMPSTAT       = `Biological status`,
    DUPLSITE       = `Genebank(s) holding safety duplications - code`,
    DUPLINSTNAME   = `Genebank(s) holding safety duplications`,
    DECLATITUDE    = `Latitude of collecting site (decimal degrees format)`,
    DECLONGITUDE   = `Longitude of collecting site (decimal degrees format)`,
    COLLSRC        = `Collecting/acquisition source`,
    STORAGE        = `Type of germplasm storage`,
    MLSSTAT        = `Status under the Multilateral System`,
    DOI            = `DOI`
  ) %>%
  select(
    holdingCty, INSTCODE, ACCENUMB, fullTaxa, GENUS, SPECIES,
    CROPNAME, ORIGCTY, SAMPSTAT, DUPLSITE, DUPLINSTNAME,
    DECLATITUDE, DECLONGITUDE, STORAGE, MLSSTAT, ACQDATE,
    COLLSRC, DOI
  )

# Add field: data source
WIEWS_new15crops <- cbind(WIEWS_new15crops, data_source = "WIEWS")

# Get lists of INSTCODEs to keep for each data source
instcode_list_wiews  <- data_source %>% filter(keep == 'WIEWS') %>% pull(INSTCODE) %>% unique()

# Drop unwanted data sources before merging dataset with Genesys
WIEWS_new15crops   <- WIEWS_new15crops   %>% filter(INSTCODE %in% instcode_list_wiews)

## Standardize ACCENUMB field: remove blank/space between institute abbreviation and number
WIEWS_new15crops  <- WIEWS_new15crops  %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))

# Split the DUPLSITE column into separate rows, trim spaces, join with WIEWS_institute_IDs conversion table
WIEWS_new15crops <- WIEWS_new15crops %>%
  separate_rows(DUPLSITE, sep = ";") %>%
  mutate(DUPLSITE = str_trim(DUPLSITE)) %>%
  mutate(DUPLSITE = as.integer(DUPLSITE)) %>%
  left_join(WIEWS_institute_IDs, by = c("DUPLSITE" = "ID"), relationship = "many-to-one")

# Combine rows back into a single row per original entry, with DUPLSITE and WIEWS_INSTCODE values separated by ";"
WIEWS_new15crops <- WIEWS_new15crops %>%
  group_by(across(-c(DUPLSITE, WIEWS_INSTCODE))) %>%
  summarize(
    DUPLSITE = paste(unique(DUPLSITE), collapse = ";"),
    WIEWS_INSTCODE = paste(unique(WIEWS_INSTCODE), collapse = ";"),
    .groups = 'drop'
  )

WIEWS_new15crops <- WIEWS_new15crops %>%
  select(-DUPLSITE) %>%                 #drop DUPLSITE column with wiews IDs
  rename(DUPLSITE = WIEWS_INSTCODE)  # Rename WIEWS_INSTCODE to DUPLSITE

# recode MLSSTAT as TRUE and FALSE
WIEWS_new15crops$MLSSTAT[WIEWS_new15crops$MLSSTAT == "I"] <-  TRUE
WIEWS_new15crops$MLSSTAT[WIEWS_new15crops$MLSSTAT == "N"] <-  FALSE
WIEWS_new15crops <- WIEWS_new15crops %>% mutate(MLSSTAT = as.logical(MLSSTAT))

############### Genesys PGR: Data Read in and Cleaning ####################
# select columns to keep
Genesys_new15crops <- subset(Genesys_new15crops, select = c(INSTCODE, ACCENUMB,
                                                        GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR,
                                                        GRIN_NAME, CROPNAME, ACQDATE, ACCENAME, SAMPSTAT,
                                                        DONORCODE, DONORNAME, OTHERNUMB, DONORNUMB, # added for PDCI calc
                                                        ORIGCTY, DECLATITUDE,DECLONGITUDE, ELEVATION,
                                                        BREDCODE, ANCEST, DUPLSITE, STORAGE,
                                                        COLLDATE, COLLSITE, COLLSRC, COLLNUMB, COLLCODE,
                                                        MLSSTAT, ACCEURL, DOI))

# Add field: data source
Genesys_new15crops <- cbind(Genesys_new15crops, data_source = "Genesys")

# Get lists of INSTCODEs to keep for each data source
instcode_list_genesys <- data_source %>% filter(keep == 'Genesys') %>% pull(INSTCODE) %>% unique()

# Drop unwanted data sources before merging dataset with WIEWS
Genesys_new15crops <- Genesys_new15crops %>% filter(INSTCODE %in% instcode_list_genesys)

##### correcting manually these species names as they occur in a lot of accessions
Genesys_new15crops$SPECIES <- gsub('z.mays', 'mays', Genesys_new15crops$SPECIES)
Genesys_new15crops$SPECIES <- gsub('o.sativa', 'sativa', Genesys_new15crops$SPECIES)
######

# Replace NA values with empty strings for 'subTaxa' and 'spAuthor'
Genesys_new15crops$SUBTAXA <- ifelse(is.na(Genesys_new15crops$SUBTAXA), "", Genesys_new15crops$SUBTAXA)
Genesys_new15crops$SPAUTHOR <- ifelse(is.na(Genesys_new15crops$SPAUTHOR), "", Genesys_new15crops$SPAUTHOR)

# Concatenate Genus, species, 'subTaxa', and 'spAuthor' with spaces in between
Genesys_new15crops$fullTaxa <- trimws(paste(Genesys_new15crops$GENUS, Genesys_new15crops$SPECIES, Genesys_new15crops$SUBTAXA, Genesys_new15crops$SPAUTHOR))

## Standardize ACCENUMB field. Remove blank/space between institute abbreviation and number
Genesys_new15crops <- Genesys_new15crops %>%
  mutate(ACCENUMB = str_replace_all(ACCENUMB, " ", ""))


####################################################################################################
## Combine Genesys and WIEWS data and Remove duplicates between Genesys and WIEWS, keep Genesys ##################################################
#format for merge
WIEWS_new15crops$SAMPSTAT <- as.numeric(WIEWS_new15crops$SAMPSTAT) #convert to numeric for merge
Genesys_new15crops$SAMPSTAT <- as.numeric(Genesys_new15crops$SAMPSTAT) #convert to numeric for merge

WIEWS_new15crops <- WIEWS_new15crops %>%
  rename(DECLONGITUDE_origin = DECLONGITUDE,
         DECLATITUDE_origin  = DECLATITUDE) %>%
  mutate(DECLONGITUDE = suppressWarnings(as.numeric(DECLONGITUDE_origin)),
         DECLATITUDE  = suppressWarnings(as.numeric(DECLATITUDE_origin)))
Genesys_new15crops <- Genesys_new15crops %>%
  rename(DECLONGITUDE_origin = DECLONGITUDE,
         DECLATITUDE_origin  = DECLATITUDE) %>%
  mutate(DECLONGITUDE = suppressWarnings(as.numeric(DECLONGITUDE_origin)),
         DECLATITUDE  = suppressWarnings(as.numeric(DECLATITUDE_origin)))

Genesys_new15crops$DECLONGITUDE_origin <- as.character(Genesys_new15crops$DECLONGITUDE_origin)
WIEWS_new15crops$DECLONGITUDE_origin  <- as.character(WIEWS_new15crops$DECLONGITUDE_origin)
Genesys_new15crops$DECLATITUDE_origin <- as.character(Genesys_new15crops$DECLATITUDE_origin)
WIEWS_new15crops$DECLATITUDE_origin  <- as.character(WIEWS_new15crops$DECLATITUDE_origin)


Genesys_new15crops$COLLSRC <- as.character(Genesys_new15crops$COLLSRC)
WIEWS_new15crops$COLLSRC  <- as.character(WIEWS_new15crops$COLLSRC)

# bind rows
gen_wiews_new15_df <- bind_rows(Genesys_new15crops, WIEWS_new15crops)
gen_wiews_new15_df$ACCENUMB <- trimws(gen_wiews_new15_df$ACCENUMB)
gen_wiews_new15_df$INSTCODE <- trimws(gen_wiews_new15_df$INSTCODE)
gen_wiews_new15_df$ID <- paste0(gen_wiews_new15_df$ACCENUMB, gen_wiews_new15_df$INSTCODE)

# create table with by GENUS and INSTCODE and data_source before dropping duplicates
gen_wiews_counts <- gen_wiews_new15_df %>%
  group_by(INSTCODE, GENUS, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
write.csv(gen_wiews_counts, '../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_24/gen_wiews_new15_counts_before_dropping_duplicates.csv', row.names = FALSE)

# Remove WIEWS rows where a Genesys row exists with the same (non-missing) DOI
gen_wiews_new15_df <- gen_wiews_new15_df %>%
  filter(
    !(data_source == "WIEWS" &
        !is.na(DOI) &
        DOI %in% gen_wiews_new15_df$DOI[gen_wiews_new15_df$data_source == "Genesys" & !is.na(gen_wiews_new15_df$DOI)]))

# Remove duplicates based on ID number
gen_wiews_new15_df <- gen_wiews_new15_df[!duplicated(gen_wiews_new15_df$ID), ]  # drop duplicates but keep the first occurrence, in this case Genesys

####### correct country codes iso-codes
source("Functions/Correct_country_codes.R")
gen_wiews_new15_df = correct_country_codes(gen_wiews_new15_df, col = 'ORIGCTY')

####### assign organization type ############
source("Functions/Assign_organization_status.R")
gen_wiews_new15_df = assign_org_type(gen_wiews_new15_df, institute_names_no_syn)

# save results
gen_wiews_new15_df$STORAGE <- as.character(gen_wiews_new15_df$STORAGE)
write.csv(gen_wiews_new15_df, '../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_24/gen_wiews_new15_df.csv', row.names = FALSE)


################# SGSV data ##########################################################################
source("Functions/Load_SGSV_data.R")
sgsv = load_SGSV_data('../../GCCSmetricsII/Data/SGSV/Deposits_all_genera_aggregated/SGSV_new15crops_unformatted.xlsx')

# create uniqueID and drop duplicates
sgsv$ACCENUMB <- trimws(sgsv$ACCENUMB)
sgsv$INSTCODE <- trimws(sgsv$INSTCODE)
sgsv$ID <- paste0(sgsv$ACCENUMB, sgsv$INSTCODE)
sgsv <- sgsv[!duplicated(sgsv$ID), ]
# save results
write.csv(sgsv, '../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_24/SGSV_new15crops_processed.csv', row.names = FALSE)


################# FAO WIEWS Indicator data ##########################################################################
# read in FAO WIEWS indicator file and croplist_PG within function
source("Functions/Load_WIEWS_indicator_data.R") # source function
WIEWS_indicator_new15_proccessed <- process_wiews_indicator_data(
  wiews_path = "../../GCCSmetricsII/Data/FAO_WIEWS/Indicator_22_data/FAO_WIEWS_Indicator22.xlsx",
  croplist_path = "../../GCCSmetricsII/Data_processing/Support_files/GCCS_Selected_crops/croplist_new15crops.xlsx")

# save results
write.csv(WIEWS_indicator_new15_proccessed, '../../GCCSmetricsII/Data_processing/1_merge_data/2025_09_24/WIEWS_indicator_new15_processed.csv', row.names = FALSE)

############ END SCRIPT ##############
