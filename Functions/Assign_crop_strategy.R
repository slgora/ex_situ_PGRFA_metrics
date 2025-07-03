# Read the Excel file
# crops <- read_excel("../Data_6/processing/croplist_PG.xlsx")
########### Function to assign Crop_strategy categorical variable ############################################

assign_crop_strategy <- function(df, crops, col_name) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Combine 'Genera_primary' and 'Genera_synonyms'
  crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>%
    paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")
  
  crops <- crops %>%
    mutate(genera_list = strsplit(trimws(genera), ",")) %>%
    unnest(genera_list) %>%
    mutate(genera_list = trimws(genera_list)) %>%
    filter(genera_list != "") %>%
    distinct(genera_list, .keep_all = TRUE)
  
  crop_strategy_dict <- setNames(as.character(crops$CropStrategy), crops$genera_list)
  
  
  # Assign Crop_strategy
  df <- df %>%
    mutate(Crop_strategy = crop_strategy_dict[as.character(.[[col_name]])])
  
  return(df)
}
