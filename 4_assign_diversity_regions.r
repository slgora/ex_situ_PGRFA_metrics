# it requires tidyverse
# it requires crops dataframe (croplist_PG.xlsx) and countries in regions (countries_in_regions.xlsx)
assign_diversity_regions = function(df, crops, countries_in_regions) {

# Combine 'Genera_primary' and 'Genera_synonyms' columns while avoiding NA values
crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>% 
  paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

genera_to_classify = crops$Genera_primary[crops$CropStrategy != 'Tropical and subtropical forages']

# create named vectors with genus-> regions of diversity 
primary_regions <- setNames(crops$PrimaryRegionsofDiversity, crops$Genera_primary )
secondary_regions <- setNames(crops$SecondaryRegionsofDiversity, crops$Genera_primary )
# create and assign values to column of primary and secondary diversity regions
dataset$Primary_diversity_region = NA
dataset$secondary_diversity_region = NA
dataset <- dataset %>%
  mutate(Primary_diversity_region = ifelse(GENUS %in% genera_to_classify, primary_regions[GENUS], NA)) %>%
  mutate(secondary_diversity_region = ifelse(GENUS %in% genera_to_classify, secondary_regions[GENUS], NA)) 

# create dictionary Country code -> region (following formatting in regions of diversity)
country_regions_dictionary <- setNames(countries_in_regions$PlantsThatFeedTheWorld_Region_new, countries_in_regions$Country_code )
# use ORIGCTY to determine if accession is from primary of secondary region of diversity
dataset$ORIGCTY_region = NA
dataset <- dataset %>%
  mutate(ORIGCTY_region = ifelse(!is.na(Primary_diversity_region), country_regions_dictionary[ORIGCTY], NA)) 

# create True/Flase value for Primary_diversity_region, works by matching strings
dataset$isInPrimary_diversity_region_boolean = NA  
dataset <- dataset %>%
  rowwise() %>%
  mutate(isInPrimary_diversity_region_boolean = ifelse(
    (!is.na(ORIGCTY) & !is.na(Primary_diversity_region)),
    grepl(ORIGCTY_region, Primary_diversity_region),
    NA
  )) %>%
  ungroup()

# create True/Flase value for secondary_diversity_region, works by matching strings
dataset$isInSecondary_diversity_region_boolean = NA  
dataset <- dataset %>%
  rowwise() %>%
  mutate(isInSecondary_diversity_region_boolean = ifelse(
    (!is.na(ORIGCTY) & !is.na(secondary_diversity_region)),
    grepl(ORIGCTY_region, secondary_diversity_region),
    NA
  )) %>%
  ungroup()

return(df)}