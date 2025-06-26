#### Install packages ####
# tidyverse includes tidyr, dplyr, readr, magrittr, stringr, ggplot2, purrr, tibble, etc.
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")

#### Load packages ####
library(tidyverse)
library(httr)
library(jsonlite)

#' Query GBIF occurrence API using full-text genus search
#'
#' Calls the GBIF occurrence API using a full-text 'q=' search,
#' which mirrors the behavior of the GBIF website portal.
#' Returns the total number of occurrence records for a genus.
#'
#' @param name A genus name (or any free-text string) to search for.
#' @param max_retries Maximum number of retry attempts in case of connection failure. Default is 3.
#' @param wait_seconds Number of seconds to wait between retries. Default is 2.
#' @return Integer representing the number of occurrence records, or 0 if none or if query fails.
#' @examples
#' get_gbif_count("Lens")
get_gbif_count <- function(name, max_retries = 3, wait_seconds = 2) {
  if (is.na(name) || name == "") return(0)
  
  attempt <- 1
  repeat {
    tryCatch({
      res <- httr::GET("https://api.gbif.org/v1/occurrence/search",
                       query = list(q = name, limit = 0))
      if (httr::status_code(res) == 200) {
        parsed <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
        return(parsed$count)
      } else {
        warning(sprintf("Attempt %d failed: HTTP %d", attempt, httr::status_code(res)))
      }
    }, error = function(e) {
      message(sprintf("API query failed on attempt %d: %s", attempt, e$message))
    })
    
    if (attempt >= max_retries) {
      message(sprintf("Failed to retrieve GBIF count for '%s' after %d attempts.", name, max_retries))
      return(0)
    }
    
    Sys.sleep(wait_seconds)
    attempt <- attempt + 1
  }
}
