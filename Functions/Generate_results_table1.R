# -------------------------------------------------------------------------------------------
#' Generate Table 1: Crop-Specific Summary Metrics Table
#'
#' Generates a list of crop-specific summary tables for a specified PTFTW results table.
#' Each table is structured using a metrics guide that maps rows, labels, roles, and variables.
#' Metric roles (e.g. Value, Number of countries, Evenness, Interdependence) are pivoted into columns,
#' and indicators are extracted from the summary dataset for each crop strategy.
#'
#' Special formatting is applied to the "Value" column: numeric zeros and missing values are replaced with "—"
#' to match the formatting of other columns.
#'
#' @param tbl_number Integer specifying which metrics guide table to process (e.g. 1 for Table 1)
#' @param summary_df Data frame of aggregated metrics (e.g. output from process_PTFTW_metrics)
#' @param metrics_guide Data frame mapping variable names to labels, roles, and row order
#'
#' @return Named list of data frames, one per crop strategy, formatted for reporting Table 1
#'
#' @import dplyr purrr tidyr
#' @export
# -------------------------------------------------------------------------------------------
generate_table1 <- function(tbl_number, summary_df, metrics_guide) {
  library(dplyr)
  library(purrr)
  library(tidyr)
  
  # Desired metric order for Table 1
  desired_order <- c(
    "Harvested area worldwide (ha)",
    "Total production worldwide (tonnes)",
    "Gross production value worldwide (current thousand USD)",
    "Export quantity worldwide (tonnes)",
    "Export value worldwide (current thousand USD)",
    "Import quantity worldwide (tonnes)",
    "Import value worldwide (current thousand USD)",
    "Contribution to calories in global food supplies (kcal/capita/day)",
    "Contribution to protein in global food supplies (g/capita/day)",
    "Contribution to fat in global food supplies (g/capita/day)",
    "Contribution to food weight in global food supplies (g/capita/day)",
    "Number of public pageviews on Wikipedia over one year"
  )
  
  crop_column <- names(summary_df)[1]  # e.g. "Crop_strategy"
  
  crop_tables <- summary_df[[crop_column]] %>%
    unique() %>%
    set_names() %>%
    map(function(crop) {
      
      table1_guide <- metrics_guide %>%
        filter(`Pertains to Table` == tbl_number)
      
      long_metrics <- table1_guide %>%
        select(
          `Row in Table`,
          `Metric Name in Results Table (or summaries text)`,
          `Name of Individual Metric Variable`,
          `Metric Role`
        ) %>%
        mutate(
          Metric = `Metric Name in Results Table (or summaries text)`,
          raw_value = purrr::map_chr(`Name of Individual Metric Variable`, function(colname) {
            if (!is.na(colname) && colname %in% names(summary_df)) {
              val <- summary_df %>%
                filter(!!sym(crop_column) == crop) %>%
                pull(colname) %>%
                first()
              if (!is.na(val)) {
                if (is.numeric(val) && val == 0) {
                  "—"
                } else {
                  format(val, digits = 3)
                }
              } else {
                "—"
              }
            } else {
              "—"
            }
          })
        ) %>%
        mutate(Metric = factor(Metric, levels = desired_order)) %>%
        arrange(Metric)
      
      wide <- long_metrics %>%
        pivot_wider(
          id_cols = Metric,
          names_from = `Metric Role`,
          values_from = raw_value,
          values_fill = list(raw_value = "—")
        )
      
      final <- wide %>%
        arrange(match(Metric, desired_order)) %>%
        transmute(
          Metric,
          Value,
          `Number of Countries Where Significant Contributor` = `Number of countries where significant contributor`,
          `Evenness of Contribution Across World Regions` = `Evenness of contribution across world regions`,
          `Estimated International Interdependence` = `Estimated international interdependence`
        )
      
      return(final)
    })
  
  return(crop_tables)
}
