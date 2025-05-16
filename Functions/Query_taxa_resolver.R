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

# function to extract the best results from the query search 
extract_best_result <- function(list_res){
  final <- list()
  for (i in list_res){
    # added to handle the case one of the results of the query is NULL
    if (is.null(i)) {
      final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
    } else if (!("names" %in% names(i))) {
      final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
    } else {
      match_type <- i["names"][[1]]["matchType"]
      if (match_type != "NoMatch"){
        input_name <-  i["names"][[1]]$name
        matched_name <-i["names"][[1]]$bestResult$matchedName
        output_name <- i["names"][[1]]$bestResult$currentName
        status <-  i["names"][[1]]$bestResult$taxonomicStatus
        final <- append(final, list(c(input_name, matched_name, match_type, status, output_name)))
      } else {
        final <- append(final, list(c( i["names"][[1]]$name, 'no_match', 'no_match', 'no_match', 'no_match')))
      }
    }}
  return(final)
}
