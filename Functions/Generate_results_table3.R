#' generate_table3
#'
#' Generates Table 3 summary output for a selected crop across multiple institutional and diversity metrics.
#'
#' This function pulls Number and Percentage metrics for each crop strategy (e.g. Aroids, Beans, Maize),
#' using metric definitions in the guide and matching values from a unified data frame of preprocessed metrics.
#' It supports summary fields pulled from distinct columns and ensures proper formatting (e.g. commas,
#' percentages). The guide specifies which variable corresponds to each role and which crops the data
#' apply to.
#'
#' @param tbl_number Integer. The table number from the guide to filter relevant metrics (typically 3).
#' @param metrics_guide Data frame. A tidy guide file defining the metric labels, roles (Number/Percentage),
#'        and variable names used for summary extraction. Must include the following columns:
#'        - "Pertains to Table"
#'        - "Metric Role"
#'        - "Metric Name in Results Table (or summaries text)"
#'        - "Row in Table"
#'        - Either "Name of Summary Variable (if summarized)" or "Name of Individual Metric Variable"
#' @param all_metrics Data frame. A unified data frame containing all crop strategy metrics.
#'        Must include a column named "Crop_strategy" and relevant metric columns.
#'
#' @return A named list of data frames, one per crop strategy, each with columns:
#'         - Metric: Label used in reporting
#'         - Number: Extracted metric value (formatted with comma if over 10,000)
#'         - Percentage: Formatted percentage value with `%` symbol
generate_table3 <- function(tbl_number, metrics_guide, all_metrics) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  # Step 1: Clean and remap guide rows with distinct variables per role
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
  
  # Step 2: Pivot guide so each metric is one row with both roles
  guide_wide <- guide_clean %>%
    select(`Row in Table`,
           Metric = `Metric Name in Results Table (or summaries text)`,
           `Metric Role`,
           role_variable) %>%
    group_by(`Row in Table`) %>%
    mutate(Metric = first(Metric)) %>%
    pivot_wider(names_from = `Metric Role`, values_from = role_variable) %>%
    ungroup()
  
  # Step 3: Get crop list from all_metrics
  all_crops <- unique(as.character(all_metrics[[crop_column]]))
  
  # Step 4: Build output for each crop
  crop_tables <- all_crops %>%
    set_names() %>%
    map(function(crop) {
      guide_wide %>%
        mutate(
          Number = map_chr(Number, function(varname) {
            if (!varname %in% names(all_metrics)) return("")
            val <- all_metrics %>%
              filter(!!sym(crop_column) == crop) %>%
              pull(varname) %>%
              discard(is.na) %>%
              first()
            if (!is.null(val) && !is.na(val)) {
              if (val > 10000) format(val, big.mark = ",", digits = 3)
              else format(val, digits = 3)
            } else ""
          }),
          Percentage = map_chr(Percentage, function(varname) {
            if (!varname %in% names(all_metrics)) return("")
            val <- all_metrics %>%
              filter(!!sym(crop_column) == crop) %>%
              pull(varname) %>%
              discard(is.na) %>%
              first()
            if (!is.null(val) && !is.na(val)) paste0(format(round(val, 2), nsmall = 2), "%") else ""
          })
        ) %>%
        arrange(`Row in Table`) %>%
        transmute(
          Metric,
          Number,
          Percentage
        )
    })
  
  return(crop_tables)
}
