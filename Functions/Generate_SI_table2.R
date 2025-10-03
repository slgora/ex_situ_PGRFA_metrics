# -------------------------------------------------------------------------------------------
#' Generate SI Table 2: Accession Count by Taxon Table for Each Crop Strategy
#'
#' For each crop strategy, this function produces a table showing the taxon name
#' and the number of accessions (from genebank collections databases), sorted in descending order.
#'
#' @param accessions_per_taxa Data frame with columns Crop_strategy, Standardized_taxa, accession_count
#'        (can be a single sheet or all sheets combined; function will split by crop)
#'
#' @return Named list of tibbles (one per crop), columns: Taxon, Number of accessions (from genebank collections databases)
#'
#' @import dplyr purrr
#' @export
# -------------------------------------------------------------------------------------------
generate_si_table2 <- function(accessions_per_taxa) {
  library(dplyr)
  library(purrr)
  
  # Helper for formatting counts with commas
  format_int <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    out <- as.character(num)
    out[is.na(num)] <- NA_character_
    big <- !is.na(num) & num > 1e4
    out[big] <- format(
      num[big],
      big.mark    = ",",
      scientific  = FALSE,
      trim        = TRUE
    )
    out
  }
  
  accessions_per_taxa %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy))) %>%
    map(function(df) {
      df %>%
        arrange(desc(accession_count)) %>%
        transmute(
          Taxon = Standardized_taxa,
          `Number of accessions (from genebank collections databases)` = format_int(accession_count)
        )
    })
}
