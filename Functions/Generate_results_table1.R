#' Generate Table 1
#'
#' This function creates a list of tables, one for each crop, summarizing key metrics for publication-ready output.
#' It applies custom formatting and exception logic to handle missing data and special cases.
#'
#' @param df A data.frame containing crop metrics, with columns matching the FAOSTAT and Wikipedia metric naming scheme.
#'
#' @details
#' The function produces, for each crop in the data, a table with the following columns:
#'   - Metric
#'   - Global Value (see special formatting rules below)
#'   - Number of countries where significant contributor
#'   - Evenness of contribution across world regions
#'   - Estimated international interdependence
#'
#' Special cases and formatting:
#'   - All missing or empty values are replaced with a dash ("—").
#'   - For "Number of public pageviews on Wikipedia over one year", the last three columns are always blank ("").
#'   - For crops "Aroids" and "Grasspea", all columns in the rows for
#'       "Export quantity worldwide (tonnes)",
#'       "Export value worldwide (current thousand USD)",
#'       "Import quantity worldwide (tonnes)",
#'       "Import value worldwide (current thousand USD)"
#'     are set to dashes ("—"), regardless of actual data.
#'   - For the "Global Value" column: 
#'       * Values < 999: two decimals.
#'       * Values ≥ 999: truncate (no decimals), comma as thousands separator.
#'
#' @return A named list of data.frames, one per crop, with formatted metrics.
#'
#' @examples
#' table1_by_crop <- generate_table1(PTFTW_metrics)
# ------------------------------------------------------------------------------
generate_table1 <- function(df) {
  metric_names <- c(
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
  metric_cols <- c(
    "crop_use-faostat-production-area_harvested_ha",
    "crop_use-faostat-production-production_quantity_tonnes",
    "crop_use-faostat-production-gross_production_value_us",
    "crop_use-faostat-trade-export_quantity_tonnes",
    "crop_use-faostat-trade-export_value_tonnes",
    "crop_use-faostat-trade-import_quantity_tonnes",
    "crop_use-faostat-trade-import_value_tonnes",
    "crop_use-faostat-food_supply-food_supply_kcal",
    "crop_use-faostat-food_supply-protein_supply_quantity_g",
    "crop_use-faostat-food_supply-fat_supply_quantity_g",
    "crop_use-faostat-food_supply-food_supply_quantity_g",
    "crop_use-public_interest-wikipedia_pageviews-taxon"
  )
  num_countries_cols <- c(
    "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
    "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
    "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
    "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
    "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
    "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
    NA
  )
  evenness_cols <- c(
    "crop_use-faostat_equality_of_use-gini_production-area_harvested_ha",
    "crop_use-faostat_equality_of_use-gini_production-production_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_production-gross_production_value_us",
    "crop_use-faostat_equality_of_use-gini_trade-export_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-export_value_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-import_quantity_tonnes",
    "crop_use-faostat_equality_of_use-gini_trade-import_value_tonnes",
    "crop_use-faostat_equality_of_use-gini_food_supply-food_supply_kcal",
    "crop_use-faostat_equality_of_use-gini_food_supply-protein_supply_quantity_g",
    "crop_use-faostat_equality_of_use-gini_food_supply-fat_supply_quantity_g",
    "crop_use-faostat_equality_of_use-gini_food_supply-food_supply_quantity_g",
    NA
  )
  interdependence_cols <- c(
    "interdependence-faostat-production-area_harvested_ha",
    "interdependence-faostat-production-production_quantity_tonnes",
    "interdependence-faostat-production-gross_production_value_us",
    "interdependence-faostat-trade-export_quantity_tonnes",
    "interdependence-faostat-trade-export_value_tonnes",
    "interdependence-faostat-trade-import_quantity_tonnes",
    "interdependence-faostat-trade-import_value_tonnes",
    "interdependence-faostat-food_supply-food_supply_kcal",
    "interdependence-faostat-food_supply-protein_supply_quantity_g",
    "interdependence-faostat-food_supply-fat_supply_quantity_g",
    "interdependence-faostat-food_supply-food_supply_quantity_g",
    NA
  )
  
  dash_metrics <- c(
    "Export quantity worldwide (tonnes)",
    "Export value worldwide (current thousand USD)",
    "Import quantity worldwide (tonnes)",
    "Import value worldwide (current thousand USD)"
  )
  
  # Custom formatter for Global Value column
  format_global_value <- function(x) {
    if (is.na(x) || x == "" || is.null(x)) return("—")
    x_num <- suppressWarnings(as.numeric(x))
    if (is.na(x_num)) return("—")
    if (abs(x_num) < 999) {
      formatted <- formatC(x_num, format = "f", digits = 2)
    } else {
      x_trunc <- trunc(x_num)
      formatted <- formatC(x_trunc, format = "d", big.mark = ",")
    }
    return(formatted)
  }
  
  safe_extract_and_format <- function(row, cols, formatter) {
    sapply(cols, function(col) formatter(if (is.na(col) || !(col %in% names(row))) NA else row[[col]]))
  }
  
  # For the other columns, keep the old logic (always two decimals or dash)
  format_number <- function(x) {
    if (is.na(x) || x == "" || is.null(x)) return("—")
    x_num <- suppressWarnings(as.numeric(x))
    if (is.na(x_num)) return("—")
    formatted <- formatC(x_num, format = "f", digits = 2, big.mark = ifelse(abs(x_num) >= 10000, ",", ""))
    return(formatted)
  }
  
  all_crops <- unique(df$Crop_strategy)
  names(all_crops) <- all_crops
  
  crop_tables <- lapply(all_crops, function(crop) {
    row <- df[df$Crop_strategy == crop, ]
    tab <- data.frame(
      Metric = metric_names,
      `Global Value` = safe_extract_and_format(row, metric_cols, format_global_value),
      `Number of countries where significant contributor` = safe_extract_and_format(row, num_countries_cols, format_number),
      `Evenness of contribution across world regions` = safe_extract_and_format(row, evenness_cols, format_number),
      `Estimated international interdependence` = safe_extract_and_format(row, interdependence_cols, format_number),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    # Remove dashes for Wikipedia row in the last three columns
    idx_wiki <- which(tab$Metric == "Number of public pageviews on Wikipedia over one year")
    tab[idx_wiki, "Number of countries where significant contributor"] <- ""
    tab[idx_wiki, "Evenness of contribution across world regions"] <- ""
    tab[idx_wiki, "Estimated international interdependence"] <- ""
    
    # For Aroids and Grasspea, set dashes for all columns for specific metrics
    if (crop %in% c("Aroids", "Grasspea")) {
      for (m in dash_metrics) {
        idx <- which(tab$Metric == m)
        if(length(idx) > 0) {
          tab[idx, 2:ncol(tab)] <- "—"
        }
      }
    }
    tab
  })
  names(crop_tables) <- all_crops
  return(crop_tables)
}
