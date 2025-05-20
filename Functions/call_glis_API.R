call_glis_API <- function(parameters) {
  # Load required packages
  library(httr)
  library(jsonlite)
  
  # Base URL for the GLIS API
  url <- "https://glis.fao.org/glisapi/v1/pgrfas?"
  
  # Make initial request to get pagination info
  resp <- GET(url, query = parameters)
  print(resp$url)
  headers <- headers(resp)
  pages <- as.integer(headers[["X-Pagination-Page-Count"]])
  print(paste("limit:", headers[["X-Rate-Limit-Limit"]]))
  print(paste("time:", headers[["X-Rate-Limit-Reset"]]))
  print(paste("pages:", pages))
  
  # Iterate through pages and collect results
  results <- list()
  for (i in 1:pages) {
    # Add/override the page parameter
    params_page <- parameters
    params_page[["page"]] <- i
    resp_page <- GET(url, query = params_page)
    json_data <- content(resp_page, as = "text", encoding = "UTF-8")
    page_results <- fromJSON(json_data, simplifyVector = FALSE)
    results <- c(results, page_results)
    Sys.sleep(1) # Sleep to respect rate limits
  }
  return(results)
}

############## Notes on how to use the function ####################
# Define your parameters as a named list
#params <- list(
#  `_format` = "json",
#  `_pretty` = FALSE,
#  `per-page` = 100,
#  genus = "Alocasia"
#)
#
# Call your function
#a <- call_glis_API(params)