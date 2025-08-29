#' generate_table7
#'
#' Generates crop-specific Table 7 summaries from guide-driven mappings.
#'
#' @param metrics_guide Data frame containing metric mappings for Table 7:
#'   - Pertains to Table == 7
#'   - Metric Role == "Number"
#'   - Name of Summary Variable (if summarized): data frame name
#'   - Name of Individual Metric Variable: column containing the metric
#'
#' @param metric_dfs Named list of data frames containing crop-level metrics.
#'   Each data frame must include a "Crop_strategy" column.
#'
#' @return A named list of tibbles, one per crop, with:
#'   - Metric: label from guide
#'   - Number: formatted value from the appropriate data frame
#'
#' - Blank strings returned when values are missing or unmatched.
#' - Metric values are formatted with 1 decimal place and comma as a thousand separator if >=10,000.
#' - Fully modular: dynamically responds to guide definitions and data frame input.
generate_table7 <- function(metrics_guide, metric_dfs) {
  library(dplyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  # Step 1: Filter guide for Table 7 metrics
  guide_tbl <- metrics_guide %>%
    filter(`Pertains to Table` == 7, `Metric Role` == "Number") %>%
    mutate(
      df_name  = `Name of Summary Variable (if summarized)` %>% na_if("") %>% str_trim(),
      var_name = `Name of Individual Metric Variable` %>% na_if("") %>% str_trim(),
      Metric   = `Metric Name in Results Table (or summaries text)`
    ) %>%
    filter(!is.na(df_name), !is.na(var_name))
  
  # Step 2: Get crop list from all metric data frames
  all_crops <- unique(reduce(metric_dfs, full_join, by = crop_column)[[crop_column]])
  
  # Step 3: Build result per crop
  crop_tables <- all_crops %>%
    set_names() %>%
    map(function(crop) {
      guide_tbl %>%
        mutate(Number = map_chr(seq_along(df_name), function(i) {
          if (!df_name[i] %in% names(metric_dfs)) return("")
          df <- metric_dfs[[df_name[i]]]
          if (!all(c(crop_column, var_name[i]) %in% names(df))) return("")
          val <- df %>%
            filter(!!sym(crop_column) == crop) %>%
            pull(var_name[i])
          if (length(val) > 0 && !is.na(val)) {
            # Always format to 1 decimal place
            formatted <- formatC(as.numeric(val), format = "f", digits = 1, big.mark = ",")
            formatted
          } else ""
        })) %>%
        select(Metric, Number)
    })
  
  return(crop_tables)
}
