### Project: Global Crop Conservation Strategies Metrics ###
### Data sources cleaning individually and Join 
#### Set working directory ####
setwd("path")

#### Install packages ####
# tidyverse already include tidyr , dplyr, readr, magrittr, stringr, readxl
install.packages("tidyverse")
library(tidyverse)
library(readxl)

####################################################################################################
########### Read in all database data for all crops ################################################
BGCI_allcrops <- read_excel("../data_6/data_sources/BGCIPlantSearch_data/BGCI_allcrops_unformatted.xlsx")
WIEWS_allcrops <- read_csv("../data_6/data_sources/FAOWIEWS_data/SDGBrequestExp.csv")
Genesys_allcrops <- read_csv("../data_6/data_sources/GenesysPGR_data/Genesys_allcrops_unformatted.csv") # Read in as a csv, not excel, helped eliminate data loss
GBIF_allcrops <- read_csv("../data_6/data_sources/GBIF_data/GBIF_allcrops_unformatted.csv")

##### read file with country codes, I added na.strings to resolve the problem with NA for Namibia becoming a NaN value
geo_names <- read_csv("../data_6/processing/geo_names.csv" , na = c("", "-"))
## subset only the relevant column to join- 2 letter country code and the 3 letter abbreviation
geo_names <- subset(geo_names, select = c(country2, country3))
#####  file with institute names and FAO INSTCODE, some synonims were added to the list 
institute_names <- read_excel("../data_6/processing/FAO_WIEWS_organizations_PG_with_synonyms.xlsx")
names(institute_names)[names(institute_names) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names)[names(institute_names) == 'Name of organization'] <- 'Name_of_organization'
institute_names_full <- subset(institute_names, select = c(`INSTCODE`, `Name_of_organization`))  %>% drop_na()
institute_names_no_syn <- read_excel("../data_6/processing/FAO_WIEWS_organizations_PG.xlsx")
names(institute_names_no_syn)[names(institute_names_no_syn) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names_no_syn)[names(institute_names_no_syn) == 'Organization authority status'] <- 'ORGANIZATIONTYPE'
institute_names_no_syn <- subset(institute_names_no_syn, select = c(`INSTCODE`, `ORGANIZATIONTYPE`))  %>% drop_na()
WIEWS_institute_IDs <- read_excel("../data_6/processing/WIEWS_instIDs.xlsx")
WIEWS_institute_IDs = subset(WIEWS_institute_IDs, select = c('ID' , 'WIEWS_INSTCODE'))
####################################################################################################
########## Change field names to follow MCPD standard see https://www.fao.org/plant-treaty/tools/toolbox-for-sustainable-use/details/en/c/1367915/ ############################################


############### BGCI Plant Search: Data Read in and Cleaning ####################
# PG notes BGCI country code is country of Botanical Gaarden not origin country of the plant (it may be the same ? but is it a. valid assumption)
# only useful column seems to be insititute name, taxa name in plant search (standardized), and type of germplasm

names(BGCI_allcrops)[names(BGCI_allcrops) == 'Name (in PlantSearch)'] <- 'fullTaxa'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Submitted Name'] <- 'SubmittedName'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Latitude'] <- 'DECLATITUDE'
names(BGCI_allcrops)[names(BGCI_allcrops) == 'Longitude'] <- 'DECLONGITUDE'
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

# see if you can add INSTCODE based on Ex Situ Site Name field
# PG this needs to be completed

# Fields we want to keep
BGCI_allcrops <- subset(BGCI_allcrops, select = c(data_source, fullTaxa, GENUS, SPECIES, STORAGE, DECLATITUDE, DECLONGITUDE ))

############### WIEWS: Data Read in and Cleaning ####################
WIEWS_allcrops <- read_csv("../data_6/data_sources/FAOWIEWS_data/SDGBrequestExp.csv")
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

## Clean country field according to notes from CountryCodes_toClean file
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "ANT"] <- "ATG"
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "BYS"] <- "BLR"
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "SCG"] = "SRB, MNE"
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "YUG"] <- "SLO, HRV, BIH, SRB, MNE, MKD, XKX"
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "CSK"] <- "CZE, SVK"
WIEWS_allcrops['ORIGCTY'][WIEWS_allcrops['ORIGCTY'] == "SUN"] <- "RUS"

# recode MLSSTAT as TRUE and FALSE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "I"] <-  TRUE
WIEWS_allcrops$MLSSTAT[WIEWS_allcrops$MLSSTAT == "N"] <-  FALSE
WIEWS_allcrops <- WIEWS_allcrops %>% mutate(MLSSTAT = as.logical(MLSSTAT))

############### Genesys PGR: Data Read in and Cleaning ####################
# select columns to keep
Genesys_allcrops <- subset(Genesys_allcrops, select = c(INSTCODE, ACCENUMB, 
                                                        GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR, 
                                                        GRIN_NAME, CROPNAME, ACQDATE, ACCENAME, SAMPSTAT, 
                                                        DONORCODE, DONORNAME, OTHERNUMB,
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

## Clean country field according to notes from CountryCodes_toClean file, PG: # PG XAE is CIMMYT and XAM is IRRI as origin of the material, it should not changed to country, ANT is BES
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "ANT"] <- "BES"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "ZAR"] <- "COD"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "ROM"] <- "ROU"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "BYS"] <- "BLR"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "SCG"] <- "SER, MNE"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "YUG"] <- "SLO, HRV, BIH, SRB, MNE, MKD, XKX"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "CSK"] <- "CZE, SVK"
Genesys_allcrops['ORIGCTY'][Genesys_allcrops['ORIGCTY'] == "SUN"] <- "RUS"

####################################################################################################
## Combine Genesys and WIEWS data and Remove duplicates between Genesys and WIEWS, keep Genesys ##################################################
combined_df <- bind_rows(Genesys_allcrops, WIEWS_allcrops)
combined_df$ACCENUMB <- trimws(combined_df$ACCENUMB)
combined_df$INSTCODE <- trimws(combined_df$INSTCODE)
combined_df$ID <- paste0(combined_df$ACCENUMB, combined_df$INSTCODE)
combined_df <- combined_df[!duplicated(combined_df$ID), ]  # drop duplicates but keep the first occurrence, in this case Genesys
# add the other dataset (BGCI)
combined_df2 <- bind_rows(combined_df, BGCI_allcrops)
combined_df <- combined_df[!duplicated(combined_df$ID), ]  # drop duplicates but keep the first occurrence, in this case Genesys
# add the other dataset (BGCI)
combined_df2 <- bind_rows(combined_df, BGCI_allcrops)
