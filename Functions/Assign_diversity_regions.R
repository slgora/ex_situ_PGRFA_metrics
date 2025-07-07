# It requires tidyverse
# It requires crops dataframe (croplist_PG.xlsx) and countries in regions (countries_in_regions.xlsx)
# Need to decide if SAMPSTAT is considered here (i.e. breeding material should be 'Not applicable') or later in the code to estimate crop metric
assign_diversity_regions = function(df, crops, countries_in_regions) {

  # Combine 'Genera_primary' and 'Genera_synonyms' columns while avoiding NA values
  crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>% 
    paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

  # create named vectors with genus-> regions of diversity 
  primary_regions <- setNames(crops$PrimaryRegionsofDiversity, crops$Genera_primary )
  secondary_regions <- setNames(crops$SecondaryRegionsofDiversity, crops$Genera_primary )
  
  # create and assign values to column of primary and secondary diversity regions
  df$Primary_diversity_region = NA
  df$secondary_diversity_region = NA

  df <- df %>%
     mutate(Primary_diversity_region = primary_regions[GENUS]) %>%
     mutate(secondary_diversity_region = secondary_regions[GENUS])
  
  # create dictionary Country code -> region (following formatting in regions of diversity)
  country_regions_dictionary <- setNames(countries_in_regions$PlantsThatFeedTheWorld_Region_new, countries_in_regions$Country_code )
  
  # use ORIGCTY to determine if accession is from primary of secondary region of diversity
  df$ORIGCTY_region = NA
  df <- df %>% mutate(ORIGCTY_region = ifelse(!is.na(Primary_diversity_region), country_regions_dictionary[ORIGCTY], NA)) 

  # create True/Flase value for Primary_diversity_region, works by matching strings
  df$fromPrimary_diversity_region = NA  
  df <- df %>%
    rowwise() %>%
    mutate(fromPrimary_diversity_region = ifelse(
      (!is.na(ORIGCTY) & !is.na(Primary_diversity_region)),
      grepl(ORIGCTY_region, Primary_diversity_region),
      NA
    )) %>%
    ungroup()

  # create True/False value for secondary_diversity_region, works by matching strings
  df$fromSecondary_diversity_region = NA  
  df <- df %>%
    rowwise() %>%
    mutate(fromSecondary_diversity_region = ifelse(
      (!is.na(ORIGCTY) & !is.na(secondary_diversity_region)),
      grepl(ORIGCTY_region, secondary_diversity_region),
      NA
    )) %>%
    ungroup()
  # exclude breeding/other material from diversity region flags unless sampstat = 999
  df <- df %>%
    mutate(across(
      c(fromPrimary_diversity_region, fromSecondary_diversity_region),
      ~ if_else(SAMPSTAT > 399 & SAMPSTAT != 999, NA_real_, .)
    ))
  return(df)
}
