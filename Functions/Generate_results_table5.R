#' generate_table5
#'
#' Generates crop-specific Table 5 summaries using guide-driven mappings.
#'
#' @param metrics_guide Data frame containing:
#'   - Pertains to Table == 5
#'   - Metric Role == "Number" or "Percentage"
#'   - Name of Summary Variable and Individual Metric Variable
#'
#' @param metric_dfs Named list of data frames containing crop-level metrics.
#'   Each must include a "Crop_strategy" column.
#'
#' @return A named list of tibbles per crop:
#'   - Metric
#'   - Number (formatted)
#'   - Percentage (formatted or blank if not mapped)
generate_table5 <- function(metrics_guide, metric_dfs) {
  library(dplyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  # 1) Filter only Table 5 and keep Number vs Percentage
  g5 <- metrics_guide %>%
    filter(`Pertains to Table` == 5,
           `Metric Role` %in% c("Number", "Percentage")) %>%
    mutate(
      MetricLabel = `Metric Name in Results Table (or summaries text)`,
      base_key    = str_to_lower(
        str_remove(MetricLabel,
                   regex("^(Number of |Percent of )", ignore_case = TRUE))
      )
    )
  
  # 2) Build a small table for Number‐rows
  num_tbl <- g5 %>%
    filter(`Metric Role` == "Number") %>%
    transmute(
      base_key,
      Metric    = MetricLabel,
      count_df  = `Name of Summary Variable (if summarized)`,
      count_var = `Name of Individual Metric Variable`
    )
  
  # 3) Build a small table for Percentage‐rows
  perc_tbl <- g5 %>%
    filter(`Metric Role` == "Percentage") %>%
    transmute(
      base_key,
      perc_df  = `Name of Summary Variable (if summarized)`,
      perc_var = `Name of Individual Metric Variable`
    )
  
  # 4) Join them so each base_key has both count + perc (or NA)
  guide_full <- full_join(num_tbl, perc_tbl, by = "base_key")
  
  # 5) Get the full list of crops
  all_crops <- metric_dfs %>%
    reduce(full_join, by = crop_column) %>%
    pull(!!sym(crop_column)) %>%
    unique()
  
  # 6) Build one data.frame per crop
  crop_tables <- map(set_names(all_crops), function(crop) {
    guide_full %>%
      mutate(
        Number = map2_chr(count_df, count_var, function(df_name, var_name) {
          if (is.na(df_name) || is.na(var_name) || !df_name %in% names(metric_dfs))
            return(" ")
          df <- metric_dfs[[df_name]]
          if (!var_name %in% names(df)) return(" ")
          val <- df %>% filter(!!sym(crop_column) == crop) %>% pull(var_name)
          if (length(val) && !is.na(val)) format(val, big.mark = ",") else " "
        }),
        Percentage = map2_chr(perc_df, perc_var, function(df_name, var_name) {
          if (is.na(df_name) || is.na(var_name) || !df_name %in% names(metric_dfs))
            return(" ")
          df <- metric_dfs[[df_name]]
          if (!var_name %in% names(df)) return(" ")
          val <- df %>% filter(!!sym(crop_column) == crop) %>% pull(var_name)
          if (length(val) && !is.na(val)) paste0(format(val, digits = 2), "%") else " "
        })
      ) %>%
      select(Metric, Number, Percentage)
  })
  
  return(crop_tables)
}
