## Function to assign category based on SAMPSTAT value, leave SAMPSTAT unchanged
## categories: Wild, Weedy, Crop, or Unknown
assign_crop_status = function(df){

  # Make a temporary character vector for SAMPSTAT, with NAs replaced by '0'
  sampstat_tmp <- as.character(ifelse(is.na(df$SAMPSTAT), 0, as.integer(df$SAMPSTAT)))

  # Define the crop codes
  crop_codes <- c('300', '400', '410', '411', '412', '413', '414', '415', '416', '417', '418', '419', '420', '421', '422', '423', '500', '600')
                
  # Assign isCrop based on sampstat_tmp
  df$crop_status <- ifelse(sampstat_tmp %in% c('100', '110', '120', '130'), 'Wild',
                     ifelse(sampstat_tmp == '200', 'Weedy',
                      ifelse(sampstat_tmp %in% crop_codes, 'Crop', 'Unknown')))
  return(df)
}