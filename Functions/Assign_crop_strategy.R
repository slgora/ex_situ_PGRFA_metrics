# Read the Excel file
# crops <- read_excel("../Data_6/processing/croplist_PG.xlsx")
########### Function to assign Crop_strategy categorical variable ############################################
assign_crop_strategy = function(df, crops){
  # Combine 'Genera_primary' and 'Genera_synonyms' columns while avoiding NA values
  crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>% 
    paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

  # Create a 'genera_list' column by splitting the 'genera' column into a list
  crops <- crops %>%
    mutate(genera_list = strsplit(trimws(genera), ","))

  # Explode the 'genera_list' column into separate rows and remove duplicates
  crops <- crops %>%
    unnest(genera_list) %>%
    distinct(genera_list, .keep_all = TRUE) %>%
    mutate(genera_list = trimws(genera_list))  %>%
    drop_na(genera_list)

  # Output: `crop_strategy_dict` is a named vector with genera_list as names and CropStrategy as values
  crop_strategy_dict <- setNames(crops$CropStrategy, crops$genera_list)
  df$Crop_strategy = NA
  df <- df %>%
    mutate(Crop_strategy = ifelse(is.na(Crop_strategy), crop_strategy_dict[GENUS], Crop_strategy))
  return(df)
}
