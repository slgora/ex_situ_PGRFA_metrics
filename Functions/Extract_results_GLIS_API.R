###### test extract API res 
#' Extract results from GLIS API call and return a data frame.
#'
#' @param results_API_call A list of results from the GLIS API (parsed JSON).
#' @return A data frame with columns for all required fields.
extract_result_GLIS <- function(results_API_call) {
  # Helper function for safely extracting nested fields
  safeget <- function(d, ...) {
    keys <- list(...)
    for (key in keys) {
      if (is.list(d) && !is.null(d[[key]])) {
        d <- d[[key]]
      } else {
        return(NA)
      }
    }
    if (is.character(d)){
       return(d)}
    else if (is.numeric(d)){
      return(d)}
       else {return(NA) }
  }
  
  # Initialize list to collect records
  r <- list()
  
  # Extract values for each accession
  for (i in results_API_call) {
    r[[length(r) + 1]] <- list(
      DOI          = if (!is.null(i$doi)) i$doi else NA,
      ACCENUMB     = safeget(i, "M02"),
      INSTCODE     = safeget(i, "user", "wiews"),
      GENUS        = safeget(i, "M05", "genus"),
      SPECIES      = safeget(i, "M05", "species"),
      SPAUTH       = safeget(i, "R04", "spauth"),
      SUBTAXA      = safeget(i, "R04", "subtaxa"),
      STAUTH       = safeget(i, "R04", "stauth"),
      SAMPSTAT     = safeget(i, "R03", "code"),
      ORIGCTY      = safeget(i, "A03"),
      DECLATITUDE  = safeget(i, "A08"),
      DECLONGITUDE = safeget(i, "A09"),
      MLS          = safeget(i, "R07", "code"),
      date_DOI     = safeget(i, "info", "doiregistered"),
      HISTORICAL   = safeget(i, "R08")
    )
  }
  
  # Convert list of records to data frame
  #df <- do.call(rbind.data.frame, r)
  df <- do.call(bind_rows, r)
  # Set column names
  colnames(df) <- c('DOI', 'ACCENUMB', 'INSTCODE', 'GENUS', 'SPECIES', 
                    'SPAUTH', 'SUBTAXA', 'STAUTH', 'SAMPSTAT', 'ORIGCTY',
                    'DECLATITUDE', 'DECLONGITUDE', 'MLS', 'date_DOI', 'HISTORICAL')
  
  # Convert MLS column to integer type if possible
  df$MLS <- as.integer(as.character(df$MLS))
  
  return(df)
}
