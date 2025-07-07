# Safety duplication function to detect if any duplication site falls outside the holding country's 3-letter code
duplicates_out_country <- function(site, pat) {
  # If site is a character string with ';', split it into a vector
  if (is.character(site) && length(site) == 1 && grepl(";", site)) {
    site <- unlist(strsplit(site, ";"))
    site <- trimws(site) # remove leading/trailing spaces
  }
  
  res <- 0  # default result: no out-of-country duplicates found
  
  for (i in site) {
    # Skip if the site is missing (NA), a known internal code, or starts with the expected 3-letter pattern
    if (is.na(i) || i %in% c('NOR051', '', 'nan') || substr(i, 1, 3) == pat) {
      next  # continue to the next site in the list
    } else {
      return(1)  # found at least one site that violates the expected pattern → out-of-country SD
    }
  }
  
  return(res)  # return 0 if no out-of-country duplicates were found
}

