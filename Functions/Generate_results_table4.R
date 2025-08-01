# -------------------------------------------------------------------------------------------
#' Generate Table 4: Crop‐Specific Metrics Summary
#'
#' Builds Table 4 (8 rows × 3 columns) for each crop, pulling Number and
#' Percentage values. The Number column is exported as numeric where possible,
#' and values above 10,000 are formatted with commas for readability.
#' All percentages are shown to two decimal places with a % symbol.
#'
#' @param metrics_guide Data frame with your metrics guide (must include
#'   columns `Pertains to Table`, `Metric Name in Results Table (or summaries text)`,
#'   and `Metric Role`).
#'
#' @param metric_dfs Named list of data frames, each with a `Crop_strategy`
#'   column and the following data frames:
#'     - accessions_by_org_type: n_records, percent, A15_collection
#'     - annex1_count: count_includedannex1
#'     - annex1_perc: annex1_perc
#'     - GLIS_dois_count: dois
#'     - GLIS_MLS_count: MLS_notified
#'     - mls_by_orgtype: count_includedmls, count_notincludedmls,
#'       percent_includedmls, percent_notincludedmls, A15_collection
#'
#' @return Named list of tibbles (one per crop) with columns:
#'   Metric | Number (formatted with commas if >10,000) | Percentage (as string with %)
#' @export
# -------------------------------------------------------------------------------------------
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
        Metric == "Number of accessions with DOI (Plant Treaty GLIS 2024)"                          ~ "GLIS_dois_count",
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
        Metric == "Number of accessions not included in the Multilateral System (MLS) (genebank collections databases)" ~ "A15_collection == FALSE",
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
          
          v <- dfc[[var_nm]][1]
          if (is.na(v)) return("")
          
          if (role == "Percentage") {
            if (v <= 1) v <- v * 100
            sprintf("%.2f%%", round(v, 2))
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
