# -------------------------------------------------------------------------------------------
#' Generate Table 6: Crop-Specific Number Metrics
#'
#' Builds Table 6 for each crop. This function:
#'   1. Reads your `metrics_guide` to identify metrics flagged for Table 6
#'      where Role = "Number"
#'   2. Joins each metric to its corresponding summary data frame in `metric_dfs`
#'   3. Extracts the numeric value for the current crop
#'   4. Formats large numbers with commas (e.g. 12,345) and small ones normally
#'   5. Returns a named list of tibbles, one per Crop_strategy, each with:
#'      - Metric: the label for the row
#'      - Number: the formatted value for that crop (or blank if missing)
#'
#' @param metrics_guide Data frame of metric definitions. Must include:
#'   - `Pertains to Table` (numeric): table number (only 6 is used)
#'   - `Metric Role` (chr): must be "Number"
#'   - `Name of Summary Variable (if summarized)` (chr): data frame key in `metric_dfs`
#'   - `Name of Individual Metric Variable` (chr): column name in that data frame
#'   - `Metric Name in Results Table (or summaries text)` (chr): the label to display
#'
#' @param metric_dfs Named list of data frames. Each data frame must have:
#'   - a column matching `Crop_strategy`
#'   - columns named in `Name of Individual Metric Variable`
#'
#' @return A named list of tibbles. Each list element is named by its
#' Crop_strategy and contains two columns:
#'   - Metric: the metric label
#'   - Number: the formatted value (or "" if missing)
#'
#' @import dplyr purrr stringr
#' @export
#'
#' @examples
#' # metric_dfs <- list( )
#' table6_by_crop <- generate_table6(metrics_guide, metric_dfs)
#' }
# -------------------------------------------------------------------------------------------
generate_table6 <- function(metrics_guide, metric_dfs) {
  library(dplyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  # 1) Identify only the Number metrics for Table 6
  guide_tbl <- metrics_guide %>%
    filter(`Pertains to Table` == 6,
           `Metric Role`       == "Number") %>%
    mutate(
      df_name  = str_trim(`Name of Summary Variable (if summarized)`),
      var_name = str_trim(`Name of Individual Metric Variable`),
      Metric   = `Metric Name in Results Table (or summaries text)`
    ) %>%
    filter(!is.na(df_name), !is.na(var_name))
  
  # 2) Get full list of crops from metric data frames
  all_crops <- reduce(metric_dfs, full_join, by = crop_column) %>%
    pull(!!sym(crop_column)) %>%
    unique()
  
  # 3) Build each crop's table
  crop_tables <- set_names(all_crops) %>%
    map(function(crop) {
      
      # For each metric, extract & format the value
      result <- guide_tbl %>%
        mutate(Number = map_chr(seq_along(df_name), function(i) {
          df_name_i  <- df_name[i]
          varname_i  <- var_name[i]
          
          # Missing data-frame or column check
          if (!df_name_i %in% names(metric_dfs)) {
            message("Missing data frame: ", df_name_i)
            return("")
          }
          df <- metric_dfs[[df_name_i]]
          if (!all(c(crop_column, varname_i) %in% names(df))) {
            message("Missing column '", varname_i, "' in ", df_name_i)
            return("")
          }
          
          # Extract the first non-NA value for this crop
          val <- df %>%
            filter(.data[[crop_column]] == crop) %>%
            pull(varname_i) %>%
            discard(is.na) %>%
            first()
          
          # Format: commas for >10k, else plain
          if (!is.null(val)) {
            if (is.numeric(val)) {
              if (val > 10000) format(val, big.mark = ",", digits = 3)
              else format(val, digits = 3)
            } else {
              as.character(val)
            }
          } else {
            ""
          }
        })) %>%
        select(Metric, Number)
      
      # If no rows, return empty placeholders
      if (nrow(result) == 0) {
        result <- tibble(
          Metric = guide_tbl$Metric,
          Number = rep("", nrow(guide_tbl))
        )
      }
      result
    })
  
  return(crop_tables)
}
