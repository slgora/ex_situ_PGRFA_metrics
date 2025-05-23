# function to call FAO GLIS API and fetch data according to parameters 
# retries connecting after a defined time if the connection fails

call_glis_API <- function(parameters, max_retries = 3, wait_seconds = 2, auth = NULL) {
  # when calling API parameter auth = list(user='type_username', password ='enter_password_here')
  # auth = NULL for unregistered users
  # Load required packages
  library(httr)
  library(jsonlite)
  
  # Base URL for the GLIS API
  url <- "https://glis.fao.org/glisapi/v1/pgrfas?"
  
  # Helper function for robust GET with retries and authentication
  robust_GET <- function(url, query, max_retries, wait_seconds, auth) {
    attempt <- 1
    repeat {
      resp <- tryCatch(
        {
          # Handle possible authentication types
          if (is.null(auth)) {
            GET(url, query = query)
          } else if (!is.null(auth$token)) {
            GET(url, query = query, add_headers(Authorization = paste("Bearer", auth$token)))
          } else if (!is.null(auth$user) && !is.null(auth$password)) {
            GET(url, query = query, authenticate(auth$user, auth$password))
          } else if (!is.null(auth$headers)) {
            GET(url, query = query, auth$headers)
          } else {
            GET(url, query = query)
          }
        },
        error = function(e) {
          message(sprintf("API connection failed (attempt %d): %s", attempt, e$message))
          NULL
        }
      )
      if (!is.null(resp) && status_code(resp) == 200) {
        return(resp)
      } else if (attempt >= max_retries) {
        stop(sprintf("API did not respond after %d attempts.", max_retries))
      } else {
        message(sprintf("Retrying in %d seconds...", wait_seconds))
        Sys.sleep(wait_seconds)
        attempt <- attempt + 1
      }
    }
  }
  
  # Make initial request to get pagination info
  resp <- robust_GET(url, query = parameters, max_retries = max_retries, wait_seconds = wait_seconds, auth = auth)
  print(resp$url)
  headers <- headers(resp)
  pages <- as.integer(headers[["X-Pagination-Page-Count"]])
  print(paste("limit:", headers[["X-Rate-Limit-Limit"]]))
  print(paste("time:", headers[["X-Rate-Limit-Reset"]]))
  print(paste("pages:", pages))
  
  # Iterate through pages and collect results
  results <- list()
  for (i in 1:pages) {
    cat(sprintf("Processing page %d of %d\n", i, pages))
    params_page <- parameters
    params_page[["page"]] <- i
    resp_page <- robust_GET(url, query = params_page, max_retries = max_retries, wait_seconds = wait_seconds, auth = auth)
    json_data <- content(resp_page, as = "text", encoding = "UTF-8")
    if (startsWith(json_data, "<!DOCTYPE html>")) {
      print("Received HTML instead of JSON. Check API status and parameters.")
      print(json_data)
      next
    }
    # Handle potential JSON parsing errors
    page_results <- tryCatch(
      {
        fromJSON(json_data, simplifyVector = FALSE)
      },
      error = function(e) {
        print(sprintf("Failed to parse JSON on page %d: %s", i, e$message))
        return(NULL)
      }
    )
    if (!is.null(page_results)) {
      results <- c(results, page_results)
    }
    Sys.sleep(1) # Sleep to respect rate limits
  }
  return(results)
}

###### example of parameters
#params <- list(
#  `_format` = "json",
#  `_pretty` = FALSE,
#  `per-page` = 100,
#  genus = 'Cajanus'
#)
####### example on how to call the function
# call_glis_API(parameters = params, max_retries = 100, wait_seconds = 10, auth = credentials)
