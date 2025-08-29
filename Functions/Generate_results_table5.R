#' Generate Table 5 Summary for Storage, Regeneration, and Safety Duplication Metrics
#'
#' This function generates a formatted summary table (Table 5) for each crop strategy,
#' using storage type, regeneration status, and safety duplication metrics defined in a guide.
#' It extracts both Number and Percentage values for each metric, applies consistent formatting,
#' and ensures the following:
#'
#' 1. The first letter of each metric is capitalized, with exceptions for "Svalbard" and "DNA".
#' 2. Any rows with blank or NA metrics are removed.
#' 3. For specific metrics (e.g., "regenerated 2012-2014", "in need of regeneration 2012-2014", "without budget for regeneration 2012-2014"),
#'    the Percentage field is blank (""), not "—".
#' 4. All missing or unavailable numeric values are displayed as a dash ("—").
#' 5. Numbers greater than 10,000 are formatted with commas, smaller numbers have no comma and no decimals.
#' 6. Percentages are formatted as "xx.xx%".
#' 7. For the metric "Accessions in genebank collections safety duplicated in Svalbard", the Number column is "0"
#'    and the Percentage column is "0.00%" for Aroids, Breadfruit, and Cassava (regardless of actual data).
#'
#' @param metrics_guide Data frame. A tidy guide file defining metric labels, roles, and variable names.
#'   Must include columns for Table, Role, df name, var name, and display label.
#' @param metric_dfs Named list of data frames. Each must include a "Crop_strategy" column.
#'
#' @return A named list of data frames, one per crop. Each data frame contains:
#'   \itemize{
#'     \item Metric (character)
#'     \item Number (character; numbers formatted with commas for values >10,000, or "—" if missing)
#'     \item Percentage (character; formatted as "xx.xx%", "—" if missing, or "" for specific metrics)
#'   }
#'
#' @examples
#' # Assuming PTFTW_metrics_guide and PTFTW_metric_dfs are loaded:
#' table5_by_crop <- generate_table5(PTFTW_metrics_guide, PTFTW_metric_dfs)
#' print(table5_by_crop[["Barley"]])
#'
#' @author slgora
#' @export
generate_table5 <- function(metrics_guide, metric_dfs) {
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  
  crop_column <- "Crop_strategy"
  
  # 1. Extract Table 5 relevant metrics and create a 'metric_stub' for matching
  guide_tbl <- metrics_guide %>%
    filter(`Pertains to Table` == 5,
           `Metric Role` %in% c("Number", "Percentage"),
           !is.na(`Metric Name in Results Table (or summaries text)`),
           `Metric Name in Results Table (or summaries text)` != "") %>%
    mutate(
      metric_stub = str_remove(`Metric Name in Results Table (or summaries text)`, "^(Number|Percent) of\\s*"),
      metric_stub = str_remove(metric_stub, "^of\\s*"),
      df_name = coalesce(na_if(str_trim(`Name of Summary Variable (if summarized)`), ""),
                         na_if(str_trim(`Name of Individual Metric Variable`), "")),
      var_name = coalesce(na_if(str_trim(`Name of Individual Metric Variable`), ""),
                          na_if(str_trim(`Name of Summary Variable (if summarized)`), "")),
      Role = `Metric Role`
    ) %>%
    filter(!is.na(df_name), !is.na(var_name))
  
  # 2. Pivot so each metric_stub is a row, with Number/Percentage columns
  guide_wide <- guide_tbl %>%
    select(metric_stub, df_name, var_name, Role) %>%
    pivot_wider(
      names_from = Role,
      values_from = c(df_name, var_name),
      names_sep = "_"
    )
  
  # 3. Get all crops
  all_crops <- unique(reduce(metric_dfs, full_join, by = crop_column)[[crop_column]])
  
  # 4. Formatting helpers
  format_number <- function(val) {
    if (is.null(val) || is.na(val)) return("—")
    numval <- suppressWarnings(as.numeric(val))
    if (is.na(numval)) return("—")
    # Only add comma if number >= 10000 (10,000)
    if (abs(numval) >= 10000) {
      return(format(numval, big.mark = ",", scientific = FALSE, trim = TRUE))
    } else {
      return(as.character(round(numval, 0)))
    }
  }
  format_percent <- function(val) {
    if (is.null(val) || is.na(val)) return("—")
    numval <- suppressWarnings(as.numeric(val))
    if (is.na(numval)) return("—")
    paste0(format(round(numval, 2), nsmall = 2), "%")
  }
  
  # 5. Metrics that should have blank Percentage fields (adjust patterns as needed)
  blank_percent_patterns <- c(
    "regenerated 2012-2014",
    "in need of regeneration 2012-2014",
    "without budget for regeneration 2012-2014"
  )
  
  # 6. Build output per crop
  crop_tables <- set_names(all_crops) %>%
    map(function(crop) {
      tbl <- guide_wide %>%
        mutate(
          Number = pmap_chr(
            list(df_name_Number, var_name_Number),
            function(df_nm, var_nm) {
              if (is.na(df_nm) || !df_nm %in% names(metric_dfs)) return("—")
              df <- metric_dfs[[df_nm]]
              if (!is.data.frame(df) || !all(c(crop_column, var_nm) %in% names(df))) return("—")
              val <- df %>% filter(.data[[crop_column]] == crop) %>% pull(var_nm) %>% discard(is.na) %>% first()
              format_number(val)
            }
          ),
          Percentage = pmap_chr(
            list(df_name_Percentage, var_name_Percentage),
            function(df_nm, var_nm) {
              if (is.na(df_nm) || !df_nm %in% names(metric_dfs)) return("—")
              df <- metric_dfs[[df_nm]]
              if (!is.data.frame(df) || !all(c(crop_column, var_nm) %in% names(df))) return("—")
              val <- df %>% filter(.data[[crop_column]] == crop) %>% pull(var_nm) %>% discard(is.na) %>% first()
              format_percent(val)
            }
          ),
          # Capitalize the first letter of the metric
          Metric = stringr::str_to_sentence(str_trim(metric_stub))
        ) %>%
        # Manual capitalization exceptions
        mutate(
          Metric = str_replace_all(Metric, "svalbard", "Svalbard"),
          Metric = str_replace_all(Metric, "\\bdna\\b", "DNA")
        ) %>%
        # Remove rows where Metric is NA or blank
        filter(!is.na(Metric), Metric != "" & Metric != "Na") %>%
        # Set Percentage to "" for specific metrics
        mutate(
          Percentage = ifelse(
            (stringr::str_detect(tolower(Metric), paste(blank_percent_patterns, collapse = "|"))) & Percentage == "—",
            "",
            Percentage
          )
        ) %>%
        select(Metric, Number, Percentage)
      
      # ---- Svalbard override for Aroids, Breadfruit, Cassava ----
      if (crop %in% c("Aroids", "Breadfruit", "Cassava")) {
        idx <- which(tbl$Metric == "Accessions in genebank collections safety duplicated in Svalbard")
        if (length(idx) > 0) {
          tbl$Number[idx] <- "0"
          tbl$Percentage[idx] <- "0.00%"
        }
      }
      tbl
    })
  
  return(crop_tables)
}
