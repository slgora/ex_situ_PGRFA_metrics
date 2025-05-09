### 

library(tidyr)
library(httr)
library(jsonlite)

# read dataset 
df = read.csv("combined07_05_25.csv", header = TRUE )

####################
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

# taxa list to be standardised
taxa_list <- unique(trimws(na.omit(df$fullTaxa)))

# loop trough taxa list and query the API
result_queries_WFO <- list()
result_queries_GRIN <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))
  best_result_GRIN <- query_taxa_resolver(i, c('6'))
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  result_queries_GRIN <- append(result_queries_GRIN, list(best_result_GRIN))
  counter <- counter + 1
}

# extract best result from the result of the queries
res_WFO <- extract_best_result(result_queries_WFO)
res_GRIN <- extract_best_result(result_queries_GRIN)
# create a taxa dictionary to be used to map the standardised taxa to the taxa in the dataset
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
taxa_standardized_df_GRIN <- as.data.frame(do.call(rbind, res_GRIN))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
colnames(taxa_standardized_df_GRIN) <- c('input_name', 'matched_name_GRIN', 'match_type_GRIN', 'status_GRIN', 'output_name_GRIN')

taxa_standardized_df_WFO <- cbind(taxa_standardized_df_WFO, data_source = "WFO")
taxa_standardized_df_GRIN <- cbind(taxa_standardized_df_GRIN, data_source = "GRIN")

# Join with institute_names to add duplInstName
taxa_standardized_df <- taxa_standardized_df_GRIN %>%
  full_join(taxa_standardized_df_WFO, by = c("input_name" = "input_name")) 

# save table with results from both WFO and GRIN
df_save_results <- apply(taxa_standardized_df,2,as.character)
write.csv(df_save_results, 'standardized_taxa7_05_25.csv', row.names = FALSE)
