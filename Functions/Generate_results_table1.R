# -------------------------------------------------------------------------------------------
#' Generate Table 1: Crop-Specific Summary Metrics Table
#'
#' Generates a list of crop-specific summary tables for a specified PTFTW results table.
#' Each table is structured using a metrics guide that maps rows, labels, roles, and variables.
#' Metric roles (e.g. Value, Number of countries, Evenness, Interdependence) are pivoted into columns,
#' and indicators are extracted from the summary dataset for each crop strategy.
#'
#' Special formatting is applied:
#' • numeric zeros and missing values → "—"
#' • values > 10,000 get comma separators (e.g. 12,345)
#' • row "Number of public pageviews on Wikipedia over one year" retains its numeric values
#'
#' @param tbl_number Integer specifying which metrics guide table to process (e.g. 1 for Table 1)
#' @param summary_df Data frame of aggregated metrics (e.g. output from process_PTFTW_metrics)
#' @param metrics_guide Data frame mapping variable names to labels, roles, and row order
#'
#' @return Named list of data frames, one per crop strategy, formatted for reporting Table 1
#' @import dplyr purrr tidyr
#' @export
# -------------------------------------------------------------------------------------------
generate_table1 <- function(tbl_number, summary_df, metrics_guide) {
  library(dplyr); library(purrr); library(tidyr)
  
  crop_column <- names(summary_df)[1]
  wiki_lbl    <- "Number of public pageviews on Wikipedia over one year"
  
  # Helper: formats a single number, inserting commas if >10,000
  format_number <- function(x, digits = 3) {
    if (is.na(x)) {
      return(NA_character_)
    }
    if (x > 1e4) {
      # no scientific notation, comma as thousands separator
      return(format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    # default: 3 significant digits
    format(x, digits = digits)
  }
  
  crop_tables <- summary_df[[crop_column]] %>%
    unique() %>%
    set_names() %>%
    map(function(crop) {
      
      table1_guide <- metrics_guide %>%
        filter(`Pertains to Table` == tbl_number)
      
      long_metrics <- table1_guide %>%
        select(
          `Row in Table`,
          `Order in table`,
          `Metric Name in Results Table (or summaries text)`,
          `Name of Individual Metric Variable`,
          `Metric Role`
        ) %>%
        mutate(
          raw_value = pmap_chr(
            list(
              `Name of Individual Metric Variable`,
              `Metric Name in Results Table (or summaries text)`
            ),
            function(colname, metric_label) {
              if (!is.na(colname) && colname %in% names(summary_df)) {
                val <- summary_df %>%
                  filter(!!sym(crop_column) == crop) %>%
                  pull(colname) %>% first()
                # Wikipedia row: always show numeric value (NA → "0")
                if (metric_label == wiki_lbl) {
                  if (is.na(val)) "0" else format_number(val)
                } else {
                  # other rows: zero/NA → dash, else formatted number
                  if (is.na(val) || (is.numeric(val) && val == 0)) {
                    "—"
                  } else {
                    format_number(val)
                  }
                }
              } else {
                "—"
              }
            }
          )
        )
      
      wide <- long_metrics %>%
        pivot_wider(
          id_cols     = `Row in Table`,
          names_from  = `Metric Role`,
          values_from = raw_value,
          values_fill = list(raw_value = "—")
        )
      
      labels <- long_metrics %>%
        filter(`Metric Role` == "Value") %>%
        select(`Row in Table`, Metric = `Metric Name in Results Table (or summaries text)`)
      
      final <- wide %>%
        left_join(labels, by = "Row in Table") %>%
        arrange(`Row in Table`) %>%
        transmute(
          Metric,
          Value,
          `Number of Countries Where Significant Contributor` = `Number of countries where significant contributor`,
          `Evenness of Contribution Across World Regions`   = `Evenness of contribution across world regions`,
          `Estimated International Interdependence`          = `Estimated international interdependence`
        )
      
      return(final)
    })
  
  return(crop_tables)
}
