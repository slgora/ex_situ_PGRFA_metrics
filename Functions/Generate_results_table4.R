#' Generate Table 4: Crop‐Specific Metrics Summary
#'
#' Constructs Table 4 for each crop strategy, summarizing eight key metrics across:
#' - Genebank holdings (international and national)
#' - Annex I inclusion
#' - GLIS DOI and MLS notifications
#' - MLS status in international and national collections
#'
#' The function dynamically maps metric roles and sources using a guide table,
#' applies conditional filters (e.g., `A15_collection`), and aggregates values where needed.
#' Percentages are recalculated from totals when required, and all outputs are formatted for reporting:
#' - Numbers are comma-formatted if >10,000
#' - Percentages are rounded to two decimal places with a "%" symbol
#'
#' @param metrics_guide A data frame containing metadata for metric mapping.
#'   Must include columns:
#'   - `Pertains to Table`
#'   - `Metric Name in Results Table (or summaries text)`
#'   - `Metric Role` ("Number" or "Percentage")
#'
#' @param metric_dfs A named list of data frames, each containing a `Crop_strategy` column.
#'   Expected names and columns:
#'   - `accessions_by_org_type`: n_records, percent, A15_collection
#'   - `annex1_count`: count_includedannex1
#'   - `annex1_perc`: annex1_perc
#'   - `GLIS_dois_count`: dois
#'   - `GLIS_MLS_count`: MLS_notified
#'   - `mls_by_orgtype`: count_includedmls, count_notincludedmls,
#'     percent_includedmls, percent_notincludedmls, total_crop_records, A15_collection
#'
#' @return A named list of tibbles, one per crop strategy, each with columns:
#'   - Metric
#'   - Number (character, formatted)
#'   - Percentage (character, formatted with %)
#'
#' @export
#'
#' @examples
#' table4_results <- generate_table4(metrics_guide = guide_df, metric_dfs = list_of_metric_dfs)

generate_table4 <- function(metrics_guide, metric_dfs) {
  library(dplyr); library(purrr); library(stringr); library(tidyr)
  
  crop_column <- "Crop_strategy"
  
  guide_tbl <- metrics_guide %>%
    filter(`Pertains to Table` == 4, `Metric Role` %in% c("Number", "Percentage")) %>%
    transmute(
      raw_metric = `Metric Name in Results Table (or summaries text)`,
      Role       = `Metric Role`,
      Metric     = if_else(Role == "Percentage",
                           str_replace(raw_metric, "^Percent of", "Number of"),
                           raw_metric)
    ) %>%
    mutate(
      df_name = case_when(
        Metric == "Number of accessions in genebank collections in international institutions" ~ "accessions_by_org_type",
        Metric == "Number of accessions in genebank collections in national or other institutions" ~ "accessions_by_org_type",
        Metric == "Number of accessions in genebank collections in Annex I" & Role == "Number"     ~ "annex1_count",
        Metric == "Number of accessions in genebank collections in Annex I" & Role == "Percentage" ~ "annex1_perc",
        Metric == "Number of accessions with DOI (Plant Treaty GLIS 2024)"                         ~ "GLIS_dois_count",
        Metric == "Number of accessions included in the Multilateral System (MLS) (Plant Treaty GLIS 2025)" ~ "GLIS_MLS_count",
        Metric %in% c(
          "Number of accessions included in the Multilateral System (MLS) (genebank collections databases)",
          "Number of accessions included in the Multilateral System (MLS) that are in international collections (genebank collections databases)",
          "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)"
        ) ~ "mls_by_orgtype",
        TRUE ~ NA_character_
      ),
      var_name = case_when(
        Metric == "Number of accessions in genebank collections in international institutions" & Role == "Number"     ~ "n_records",
        Metric == "Number of accessions in genebank collections in international institutions" & Role == "Percentage" ~ "percent",
        Metric == "Number of accessions in genebank collections in national or other institutions" & Role == "Number"     ~ "n_records",
        Metric == "Number of accessions in genebank collections in national or other institutions" & Role == "Percentage" ~ "percent",
        Metric == "Number of accessions in genebank collections in Annex I" & Role == "Number"     ~ "count_includedannex1",
        Metric == "Number of accessions in genebank collections in Annex I" & Role == "Percentage" ~ "annex1_perc",
        Metric == "Number of accessions with DOI (Plant Treaty GLIS 2024)"                         ~ "dois",
        Metric == "Number of accessions included in the Multilateral System (MLS) (Plant Treaty GLIS 2025)" ~ "MLS_notified",
        Metric == "Number of accessions included in the Multilateral System (MLS) (genebank collections databases)" & Role == "Number"     ~ "count_includedmls",
        Metric == "Number of accessions included in the Multilateral System (MLS) (genebank collections databases)" & Role == "Percentage" ~ "percent_includedmls",
        Metric == "Number of accessions included in the Multilateral System (MLS) that are in international collections (genebank collections databases)" & Role == "Number"     ~ "count_includedmls",
        Metric == "Number of accessions included in the Multilateral System (MLS) that are in international collections (genebank collections databases)" & Role == "Percentage" ~ "percent_includedmls",
        Metric == "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)" & Role == "Number"     ~ "count_notincludedmls",
        Metric == "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)" & Role == "Percentage" ~ "percent_notincludedmls",
        TRUE ~ NA_character_
      ),
      filter_expr = case_when(
        Metric == "Number of accessions in genebank collections in international institutions" ~ "A15_collection == TRUE",
        Metric == "Number of accessions in genebank collections in national or other institutions" ~ "A15_collection == FALSE",
        Metric == "Number of accessions included in the Multilateral System (MLS) that are in international collections (genebank collections databases)" ~ "A15_collection == TRUE",
        # FIX: remove filter for "not included in MLS" to allow full crop-wide aggregation
        Metric == "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)" ~ NA_character_,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(df_name), !is.na(var_name))
  
  all_crops <- metric_dfs %>%
    reduce(full_join, by = crop_column) %>%
    pull(!!sym(crop_column)) %>%
    unique()
  
  out <- map(set_names(all_crops), function(crop) {
    vals <- guide_tbl %>%
      mutate(Value = pmap_chr(
        list(df_name, var_name, filter_expr, Role),
        function(df_nm, var_nm, filt, role) {
          df <- metric_dfs[[df_nm]]
          if (is.null(df) || !all(c(crop_column, var_nm) %in% names(df))) return("")
          dfc <- df %>% filter(.data[[crop_column]] == crop)
          if (!is.na(filt)) dfc <- dfc %>% filter(eval(parse(text = filt)))
          
          # Fix: aggregate counts and recalculate percentages for MLS metrics
          v <- if (df_nm == "mls_by_orgtype" &&
                   var_nm == "count_includedmls") {
            sum(dfc[["count_includedmls"]], na.rm = TRUE)
          } else if (df_nm == "mls_by_orgtype" &&
                     var_nm == "percent_includedmls") {
            total <- unique(dfc$total_crop_records)
            count <- sum(dfc[["count_includedmls"]], na.rm = TRUE)
            if (length(total) == 1 && total > 0) round(100 * count / total, 2) else NA
          } else if (df_nm == "mls_by_orgtype" &&
                     var_nm == "count_notincludedmls") {
            sum(dfc[["count_notincludedmls"]], na.rm = TRUE)
          } else if (df_nm == "mls_by_orgtype" &&
                     var_nm == "percent_notincludedmls") {
            total <- unique(dfc$total_crop_records)
            count <- sum(dfc[["count_notincludedmls"]], na.rm = TRUE)
            if (length(total) == 1 && total > 0) round(100 * count / total, 2) else NA
          } else {
            dfc[[var_nm]][1]
          }
          
          if (is.na(v)) return("")
          
          if (role == "Percentage") {
            sprintf("%.2f%%", v)
          } else {
            formatC(v, format = "f", digits = 0)
          }
        }
      )) %>% select(Metric, Role, Value)
    
    tbl <- vals %>%
      pivot_wider(names_from = Role, values_from = Value, values_fill = "") %>%
      mutate(
        Number_raw = suppressWarnings(as.numeric(Number)),
        Number = ifelse(Number_raw > 10000,
                        format(Number_raw, big.mark = ",", scientific = FALSE),
                        as.character(Number_raw)),
        Percentage = coalesce(Percentage, "")
      ) %>%
      select(Metric, Number, Percentage) %>%
      slice(match(c(
        "Number of accessions in genebank collections in international institutions",
        "Number of accessions in genebank collections in national or other institutions",
        "Number of accessions in genebank collections in Annex I",
        "Number of accessions with DOI (Plant Treaty GLIS 2024)",
        "Number of accessions included in the Multilateral System (MLS) (Plant Treaty GLIS 2025)",
        "Number of accessions included in the Multilateral System (MLS) (genebank collections databases)",
        "Number of accessions included in the Multilateral System (MLS) that are in international collections (genebank collections databases)",
        "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)"
      ), Metric))
    
    return(tbl)
  })
  
  return(out)
}
