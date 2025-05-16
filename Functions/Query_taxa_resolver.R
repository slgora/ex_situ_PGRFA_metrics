# function to query API of https://verifier.globalnames.org
query_taxa_resolver <- function(taxa, sources = c('196')){
  if (is.character(taxa)){
    taxa_format <- gsub(" ", "+", taxa)
    URL <- paste0('https://verifier.globalnames.org/api/v1/verifications/', taxa_format, 
                  '?data_sources=', paste(sources, collapse = "|"), 
                  '&all_matches=false&capitalize=true&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.8')
    result_dict <- "undetermined"  # Initialize result_dict to handle cases where the API call fails
    tryCatch({
      r <- GET(URL)
      result <- content(r, "text", encoding = "UTF-8")
      result_dict <- fromJSON(result)
    }, error = function(e){
      return(NULL)
    })
  } else {
    return("undetermined")
  }
  return(result_dict)
}
