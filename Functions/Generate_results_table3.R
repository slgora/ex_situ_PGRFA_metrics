#' Generate Table 3 Summary for Institutional and Diversity Metrics
#'
#' This function generates a formatted summary table (Table 3) for each crop strategy,
#' using institutional and diversity metrics defined in a guide. It extracts both
#' Number and Percentage values for each metric, applies consistent formatting, and
#' arranges rows in a predefined order based on reporting standards.
#'
#' Metric definitions are mapped from guide variables to standardized column names
#' in the unified metrics data frame. The function supports both summarized and
#' individual metric variables and ensures that missing values are handled gracefully.
#'
#' @param tbl_number Integer. The table number used to filter relevant metrics from the guide (typically 3).
#' @param metrics_guide Data frame. A tidy guide file defining metric labels, roles, and variable names.
#'   Must include the following columns:
#'   \itemize{
#'     \item \code{"Pertains to Table"}
#'     \item \code{"Metric Role"} — either "Number" or "Percentage"
#'     \item \code{"Metric Name in Results Table (or summaries text)"} — used for row labels
#'     \item \code{"Row in Table"}
#'     \item \code{"Name of Summary Variable (if summarized)"} or \code{"Name of Individual Metric Variable"}
#'   }
#' @param all_metrics Data frame. A unified metrics table containing one row per crop strategy.
#'   Must include a column named \code{"Crop_strategy"} and all relevant metric columns.
#'
#' @return A named list of data frames, one per crop strategy. Each data frame contains:
#'   \describe{
#'     \item{\code{Metric}}{The reporting label for each metric, in standardized order.}
#'     \item{\code{Number}}{Formatted numeric value (with commas if >10,000).}
#'     \item{\code{Percentage}}{Formatted percentage value with \code{"%"} symbol.}
#'   }
#'
#' @details
#' Metrics are returned in the following fixed order:
#' \enumerate{
#'   \item Total number of accessions in genebank collections
#'   \item Number of institutions holding genebank collections
#'   \item Number of distinct taxonomic names in genebank collections
#'   \item Number of accessions of crop wild relatives (CWR) in genebank collections
#'   \item Number of accessions of weedy materials in genebank collections
#'   \item Number of accessions of landraces in genebank collections
#'   \item Number of accessions of breeding materials in genebank collections
#'   \item Number of accessions of improved varieties in genebank collections
#'   \item Number of accessions of other materials in genebank collections
#'   \item Number of accessions not marked with an improvement type in genebank collections
#'   \item Number of countries where germplasm has been collected for genebank collections
#'   \item Number of accessions in genebank collections from the primary region(s) of diversity
#'   \item Number of accessions in genebank collections from the primary and secondary region(s) of diversity
#'   \item Number of taxa in botanic garden collections
#'   \item Number of botanic institutions holding botanic garden collections
#' }
#'
#' @examples
#' \dontrun{
#' table3_output <- generate_table3(
#'   tbl_number = 3,
#'   metrics_guide = guide_df,
#'   all_metrics = metrics_df
#' )
#' table3_output[["Beans"]]
#' }
#'
#' @export
generate_table3 <- function(tbl_number, metrics_guide, all_metrics) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  guide_clean <- metrics_guide %>%
    filter(`Pertains to Table` == tbl_number, `Metric Role` %in% c("Number", "Percentage")) %>%
    mutate(role_variable = coalesce(
      na_if(`Name of Summary Variable (if summarized)`, ""),
      na_if(`Name of Individual Metric Variable`, "")
    )) %>%
    mutate(role_variable = case_when(
      role_variable == "accession_by_crop_strategy"         ~ "accessions_count",
      role_variable == "unique_institutions"                ~ "unique_instcount",
      role_variable == "unique_taxa"                        ~ "unique_taxa_count",
      role_variable == "cwr_metric"       & `Metric Role` == "Number"     ~ "SAMPSTAT100_count",
      role_variable == "cwr_metric"       & `Metric Role` == "Percentage" ~ "SAMPSTAT100_perc",
      role_variable == "weedy_metric"     & `Metric Role` == "Number"     ~ "SAMPSTAT200_count",
      role_variable == "weedy_metric"     & `Metric Role` == "Percentage" ~ "SAMPSTAT200_perc",
      role_variable == "landrace_metric"  & `Metric Role` == "Number"     ~ "SAMPSTAT300_count",
      role_variable == "landrace_metric"  & `Metric Role` == "Percentage" ~ "SAMPSTAT300_perc",
      role_variable == "breeding_metric"  & `Metric Role` == "Number"     ~ "SAMPSTAT400s_count",
      role_variable == "breeding_metric"  & `Metric Role` == "Percentage" ~ "SAMPSTAT400s_perc",
      role_variable == "improved_metric"  & `Metric Role` == "Number"     ~ "SAMPSTAT500_count",
      role_variable == "improved_metric"  & `Metric Role` == "Percentage" ~ "SAMPSTAT500_perc",
      role_variable == "othervar_metric"  & `Metric Role` == "Number"     ~ "SAMPSTAT999_count",
      role_variable == "othervar_metric"  & `Metric Role` == "Percentage" ~ "SAMPSTAT999_perc",
      role_variable == "no_SAMPSTAT_metric" & `Metric Role` == "Number"     ~ "SAMPSTATna_count",
      role_variable == "no_SAMPSTAT_metric" & `Metric Role` == "Percentage" ~ "SAMPSTATna_perc",
      role_variable == "country_count"    ~ "unique_countrycount",
      role_variable == "primary_region_metric"   & `Metric Role` == "Number"     ~ "count",
      role_variable == "primary_region_metric"   & `Metric Role` == "Percentage" ~ "isinprimaryregion_perc",
      role_variable == "diversity_regions_metric"& `Metric Role` == "Number"     ~ "isindiversityregions_count",
      role_variable == "diversity_regions_metric"& `Metric Role` == "Percentage" ~ "isindiversityregions_perc",
      role_variable == "BGCI_taxa_count"          ~ "bgci_unique_taxa_count",
      role_variable == "BGCI_inst_count"          ~ "unique_inst_count",
      TRUE ~ role_variable
    ))
  
  guide_wide <- guide_clean %>%
    select(`Row in Table`,
           Metric = `Metric Name in Results Table (or summaries text)`,
           `Metric Role`,
           role_variable) %>%
    group_by(`Row in Table`) %>%
    mutate(Metric = first(Metric)) %>%
    pivot_wider(names_from = `Metric Role`, values_from = role_variable) %>%
    ungroup()
  
  desired_order <- c(
    "Total number of accessions in genebank collections",
    "Number of institutions holding genebank collections",
    "Number of distinct taxonomic names in genebank collections",
    "Number of accessions of crop wild relatives (CWR) in genebank collections",
    "Number of accessions of weedy materials in genebank collections",
    "Number of accessions of landraces in genebank collections",
    "Number of accessions of breeding materials in genebank collections",
    "Number of accessions of improved varieties in genebank collections",
    "Number of accessions of other materials in genebank collections",
    "Number of accessions not marked with an improvement type in genebank collections",
    "Number of countries where germplasm has been collected for genebank collections",
    "Number of accessions in genebank collections from the primary region(s) of diversity",
    "Number of accessions in genebank collections from the primary and secondary region(s) of diversity",
    "Number of taxa in botanic garden collections",
    "Number of botanic institutions holding botanic garden collections"
  )
  
  guide_wide <- guide_wide %>%
    mutate(Metric = factor(Metric, levels = desired_order)) %>%
    arrange(Metric)
  
  format_number <- function(x) {
    ifelse(is.na(x), NA_character_,
           ifelse(x > 1e4, format(x, big.mark = ",", scientific = FALSE), as.character(x)))
  }
  
  format_percent <- function(x) {
    ifelse(is.na(x), NA_character_, sprintf("%.2f%%", x))
  }
  
  # Step 5: Build summary table per crop strategy
  all_metrics %>%
    split(.[[crop_column]]) %>%
    map(function(df) {
      crop_row <- df[1, ]
      guide_wide %>%
        mutate(
          Number = format_number(as.numeric(crop_row[[Number]])),
          Percentage = format_percent(as.numeric(crop_row[[Percentage]]))
        ) %>%
        select(Metric, Number, Percentage)
    })
}
