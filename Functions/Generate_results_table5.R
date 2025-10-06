#' Generate Table 5 Summary for Storage, Regeneration, and Safety Duplication Metrics
#'
#' This function generates a formatted summary table (Table 5) for each crop strategy,
#' using storage type, regeneration status, and safety duplication metrics defined in a guide.
#' It extracts both Number and Percentage values for each metric, applies consistent formatting,
#' and enforces a custom row order and names as specified by the user.
#'
#' @param metrics_guide Data frame. A tidy guide file defining metric labels, roles, and variable names.
#'   Must include columns for Table, Role, df name, var name, and display label.
#' @param metric_dfs Named list of data frames. Each must include a "Crop_strategy" column.
#'
#' @return A named list of data frames, one per crop. Each data frame contains:
#'   \itemize{
#'     \item Metric (character)
#'     \item Number (character; numbers formatted with commas for values ≥1,000, or "—" if missing)
#'     \item Percentage (character; formatted as "xx.x%", "—" if missing, or "" for specific metrics)
#'   }
#'
#' @examples
#' # Assuming PTFTW_metrics_guide and PTFTW_metric_dfs are loaded:
#' table5_by_crop <- generate_table5(PTFTW_metrics_guide, PTFTW_metric_dfs)
#' print(table5_by_crop[["Barley"]])
#'
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
    # Add comma if number ≥ 1,000
    if (abs(numval) >= 1000) {
      return(format(numval, big.mark = ",", scientific = FALSE, trim = TRUE))
    } else {
      return(as.character(round(numval, 0)))
    }
  }
  format_percent <- function(val) {
    if (is.null(val) || is.na(val)) return("—")
    numval <- suppressWarnings(as.numeric(val))
    if (is.na(numval)) return("—")
    sprintf("%.1f%%", numval)
  }
  
  # 5. Metrics that should have blank Percentage fields (adjust patterns as needed)
  blank_percent_patterns <- c(
    "regenerated 2012-2014",
    "in need of regeneration 2012-2014",
    "without budget for regeneration 2012-2014"
  )
  
  # 6. Custom row names and order for Table 5
  desired_metric_order <- c(
    "Number of accessions held in seed storage in genebank collections",
    "Number of accessions held in short-term seed storage in genebank collections",
    "Number of accessions held in medium-term seed storage in genebank collections",
    "Number of accessions held in long-term seed storage in genebank collections",
    "Number of accessions held in seed storage of undefined type in genebank collections",
    "Number of accessions held in field storage in genebank collections",
    "Number of accessions held in in-vitro storage in genebank collections",
    "Number of accessions held in cryo storage in genebank collections",
    "Number of accessions held as DNA in genebank collections",
    "Number of accessions held in other storage in genebank collections",
    "Number of accessions not marked with a storage type in genebank collections",
    "Number of accessions in genebank collections regenerated 2012-2014",
    "Number of accessions in genebank collections in need of regeneration 2012-2014",
    "Number of accessions in genebank collections in need of regeneration without budget for regeneration 2012-2014",
    "Number of accessions safety duplicated out of the country in genebank collections",
    "Number of accessions in genebank collections safety duplicated in Svalbard"
  )
  
  # 7. Build output per crop
  crop_tables <- set_names(all_crops) %>%
    map(function(crop) {
      # Calculate the Number and Percentage columns in the order of guide_wide
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
          metric_name_number = paste0("Number of ", str_trim(metric_stub))
        ) %>%
        # Remove rows where metric_name_number is blank/NA
        filter(!is.na(metric_name_number), metric_name_number != "" & metric_name_number != "Na") %>%
        # Set Percentage to "" for specific metrics
        mutate(
          Percentage = ifelse(
            (stringr::str_detect(tolower(metric_name_number), paste(blank_percent_patterns, collapse = "|"))) & Percentage == "—",
            "",
            Percentage
          )
        ) %>%
        # Select using custom Metric names and order
        select(Metric = metric_name_number, Number, Percentage)
      
      # Force table to have exactly the desired rows, in order, even if some are missing in underlying data
      tbl <- tibble(Metric = desired_metric_order) %>%
        left_join(tbl, by = "Metric") %>%
        mutate(
          Number = ifelse(is.na(Number), "—", Number),
          Percentage = ifelse(is.na(Percentage), "—", Percentage)
        )
      
      # ---- Svalbard override for Aroids, Breadfruit, Cassava ----
      if (crop %in% c("Aroids", "Breadfruit", "Cassava")) {
        idx <- which(tbl$Metric == "Number of accessions in genebank collections safety duplicated in Svalbard")
        if (length(idx) > 0) {
          tbl$Number[idx] <- "0"
          tbl$Percentage[idx] <- "0.0%"
        }
      }
      tbl
    })
  
  return(crop_tables)
}
