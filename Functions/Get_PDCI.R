#PDCI function takes into account 28 columns (genesys):
# Column name synonyms:

# `GENUS` = genus
# `SPECIES`= species
- `SPAUTHOR` = spAuthor
- `SUBTAXA`=subTaxa
- `SUBTAUTHOR`= subTAuthor
# `CROPNAME`= cropName
- `ACQDATE`= acqDate
# `SAMPSTAT`= sampStat, 
- `DONORCODE`= donorCode
- `DONORNAME`= donorName
- `DONORNUMB`=  donorNumb
- `OTHERNUMB`= otherNumb
# `DUPLSITE` = duplSite
# `STORAGE` = storage
- `ACCEURL`= acceUrl
# `MLSSTAT`= mlsStat
# `ORIGCTY`= origCty
- `COLLSITE`= collSite
#`DECLATITUDE` = latitude
# `DECLONGITUDE`= longitude
- `ELEVATION` = elevation
- `COLLDATE`= collDate
- `BREDCODE`= bredCode
- `ANCEST`= ancest
- `COLLSRC`= collSrc
# `ACCENAME`= acceNumb
- `COLLNUMB`= collNumb
- `COLLCODE`= collCode


get_PDCI <- function(df) {
  # Initialize PDCI column with 0
  df$PDCI <- 0
  
  # Function to print debug information
  print_debug_info <- function(df, step) {
    cat(paste("After step:", step, "\n"))
    print(head(df[, c("genus", "species", "storage", "origCty", "sampStat", "acceUrl", "mlsStat", "ancest", "latitude", "longitude", "collDate", "bredCode", "PDCI")], 5))
  }
  
  # Print initial state of the data
  print_debug_info(df, "initial")
  
  # Apply conditions and update PDCI
  df$PDCI <- ifelse(!is.na(df$genus), df$PDCI + 120, df$PDCI)
  print_debug_info(df, "genus")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !df$species %in% c('sp', 'sp.', 'spp.', 'spp'), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "genus and species")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$spAuthor), df$PDCI + 5, df$PDCI)
  print_debug_info(df, "species author")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$subTaxa), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "subTaxa")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$subTaxa) & !is.na(df$subTAuthor), df$PDCI + 5, df$PDCI)
  print_debug_info(df, "subTaxa author")
  
  df$PDCI <- ifelse(!is.na(df$cropName), df$PDCI + 45, df$PDCI)
  print_debug_info(df, "cropName")
  
  df$PDCI <- ifelse(!is.na(df$acqDate), df$PDCI + 10, df$PDCI)
  print_debug_info(df, "acqDate")
  
  df$PDCI <- ifelse(!is.na(df$sampStat), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "sampStat")
  
  df$PDCI <- ifelse(!is.na(df$donorCode), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "donorCode")
  
  df$PDCI <- ifelse(!is.na(df$donorName) & is.na(df$donorCode), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "donorName without donorCode")
  
  df$PDCI <- ifelse(!is.na(df$donorNumb) & (!is.na(df$donorCode) | !is.na(df$donorName)), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "donorNumb with donorCode or donorName")
  
  df$PDCI <- ifelse(!is.na(df$donorNumb) & is.na(df$donorCode) & is.na(df$donorName), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "donorNumb without donorCode or donorName")
  
  df$PDCI <- ifelse(!is.na(df$otherNumb), df$PDCI + 35, df$PDCI)
  print_debug_info(df, "otherNumb")
  
  df$PDCI <- ifelse(!is.na(df$duplSite), df$PDCI + 30, df$PDCI)
  print_debug_info(df, "duplSite")
  
  df$PDCI <- ifelse(!is.na(df$storage), df$PDCI + 15, df$PDCI)
  print_debug_info(df, "storage")
  
  df$PDCI <- ifelse(!is.na(df$acceUrl), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "acceUrl")
  
  df$PDCI <- ifelse(!is.na(df$mlsStat), df$PDCI + 15, df$PDCI)
  print_debug_info(df, "mlsStat")
  
  # Handle origCty and sampStat conditions, considering NA values for sampStat
  df$PDCI <- ifelse(!is.na(df$origCty) & (is.na(df$sampStat) | df$sampStat %/% 100 < 4), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "origCty with sampStat < 4")
  
  df$PDCI <- ifelse(!is.na(df$origCty) & !is.na(df$sampStat) & df$sampStat %/% 100 >= 4, df$PDCI + 40, df$PDCI)
  print_debug_info(df, "origCty with sampStat >= 4")
  
  # Check latitude and longitude conditions
  df$PDCI <- ifelse(!is.na(df$latitude) & !is.na(df$longitude), df$PDCI + 120, df$PDCI)
  print_debug_info(df, "latitude and longitude")
  
  # Check elevation condition
  df$PDCI <- ifelse(!is.na(df$elevation), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "elevation")
  
  # Check collection date condition
  df$PDCI <- ifelse(!is.na(df$collDate), df$PDCI + 30, df$PDCI)
  print_debug_info(df, "collDate")
  
  # Check breeding code condition
  df$PDCI <- ifelse(!is.na(df$bredCode), df$PDCI + 110, df$PDCI)
  print_debug_info(df, "bredCode")
  
  # Check ancestry condition
  df$PDCI <- ifelse(!is.na(df$ancest), df$PDCI + 150, df$PDCI)
  print_debug_info(df, "ancest")
  
  # Normalize PDCI by dividing by 100 
  df$PDCI <- df$PDCI / 100 
  
  # Ensure PDCI is between 0 and 10
  df$PDCI[is.na(df$PDCI)] <- 0
  df$PDCI <- pmin(pmax(df$PDCI, 0), 10)
  
  return(df) 
} 