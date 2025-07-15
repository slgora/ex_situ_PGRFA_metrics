# -------------------------------------------------------------------------------------------
# Function: generate_table1
#
# Description:
# Generates a list of crop-specific summary tables for a specified PTFTW results table.
# Each table is structured using a metrics guide that maps rows, labels, roles, and variables.
# Metric roles (e.g. Value, Number of countries, Evenness, Interdependence) are pivoted into columns,
# and indicators are extracted from the summary dataset for each crop strategy.
#
# Inputs:
# - tbl_number: Integer specifying which metrics guide table to process (e.g. 1 for Table 1)
# - summary_df: Data frame of aggregated metrics (e.g. output from process_PTFTW_metrics)
# - metrics_guide: Data frame mapping variable names to labels, roles, and row order
#
# Output:
# - A named list of data frames, one per crop strategy, formatted for reporting Table 1
# -------------------------------------------------------------------------------------------

## generate metrics table 1 function   
generate_table1 <- function(tbl_number, summary_df, metrics_guide) {
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
          `Order in table`,
          `Metric Name in Results Table (or summaries text)`,
          `Name of Individual Metric Variable`,
          `Metric Role`
        ) %>%
        mutate(
          raw_value = purrr::map_chr(`Name of Individual Metric Variable`, function(colname) {
            if (!is.na(colname) && colname %in% names(summary_df)) {
              val <- summary_df %>%
                filter(!!sym(crop_column) == crop) %>%
                pull(colname)
              if (length(val) > 0 && !is.na(val)) format(val, digits = 3) else ""
            } else {
              ""
            }
          })
        )
      
      # Pivot roles into columns based on Row in Table
      wide <- long_metrics %>%
        pivot_wider(
          id_cols = `Row in Table`,
          names_from = `Metric Role`,
          values_from = raw_value
        )
      
      # Pull metric labels from 'Value' rows
      labels <- long_metrics %>%
        filter(`Metric Role` == "Value") %>%
        select(`Row in Table`, Metric = `Metric Name in Results Table (or summaries text)`)
      
      final <- wide %>%
        left_join(labels, by = "Row in Table") %>%
        arrange(`Row in Table`) %>%
        transmute(
          Crop = crop,
          Metric,
          Value,
          `Number of countries where significant contributor`,
          `Evenness of contribution across world regions`,
          `Estimated international interdependence`
        )
      
      return(final)
    })
  
  return(crop_tables)
}
