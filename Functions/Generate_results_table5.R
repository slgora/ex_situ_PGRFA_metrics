#' Generate Table 5 Summary for Storage, Regeneration, and Safety Duplication Metrics
#'
#' This function generates a formatted summary table (Table 5) for each crop strategy,
#' using storage type, regeneration status, and safety duplication metrics defined in a guide.
#' It extracts both Number and Percentage values for each metric, applies consistent formatting,
#' and removes any rows with missing metric labels to avoid blank entries.
#'
#' @param tbl_number Integer. The table number used to filter relevant metrics from the guide (typically 5).
#' @param metrics_guide Data frame. A tidy guide file defining metric labels, roles, and variable names.
#'   Must include the following columns:
#'   \itemize{
#'     \item \code{"Pertains to Table"}
#'     \item \code{"Metric Role"} — either "Number" or "Percentage"
#'     \item \code{"Metric Name in Results Table (or summaries text)"}
#'     \item \code{"Row in Table"}
#'     \item \code{"Name of Summary Variable (if summarized)"} or \code{"Name of Individual Metric Variable"}
#'   }
#' @param all_metrics Data frame. A unified metrics table containing one row per crop strategy.
#'   Must include a column named \code{"Crop_strategy"} and all relevant metric columns.
#'
#' @return A named list of data frames, one per crop strategy. Each data frame contains:
#'   \describe{
#'     \item{\code{Metric}}{The reporting label for each metric.}
#'     \item{\code{Number}}{Formatted numeric value (with commas if >10,000).}
#'     \item{\code{Percentage}}{Formatted percentage value with \code{"%"} symbol, if applicable.}
#'   }
#'
#' @export
generate_table5 <- function(tbl_number, metrics_guide, all_metrics) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  crop_column <- "Crop_strategy"
  
  # Step 1: Clean and remap guide rows with distinct variables per role
  guide_clean <- metrics_guide %>%
    filter(`Pertains to Table` == tbl_number, `Metric Role` %in% c("Number", "Percentage")) %>%
    filter(!is.na(`Metric Name in Results Table (or summaries text)`),
           `Metric Name in Results Table (or summaries text)` != "") %>%
    mutate(role_variable = coalesce(
      na_if(`Name of Summary Variable (if summarized)`, ""),
      na_if(`Name of Individual Metric Variable`, "")
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
