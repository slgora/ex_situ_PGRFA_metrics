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
--------------------------------------------------------------------------------
  generate_table5 <- function(metrics_guide, metric_dfs) {
    library(dplyr)
    library(purrr)
    library(stringr)
    
    crop_column <- "Crop_strategy"
    
    format_int <- function(x) {
      num <- suppressWarnings(as.numeric(x))
      out <- as.character(x)
      ok  <- !is.na(num) & num > 1e4
      out[ok] <- format(num[ok], big.mark = ",", scientific = FALSE, trim = TRUE)
      out
    }
    
    safe_percent <- function(x) {
      num <- suppressWarnings(as.numeric(x))
      if (length(num) != 1 || is.na(num)) return("")
      paste0(format(round(num, 2), nsmall = 2), "%")
    }
    
    pull_one <- function(df, var_name, crop) {
      vals <- df %>% filter(!!sym(crop_column) == crop) %>% pull(var_name)
      if (length(vals) > 1) {
        warning(
          "Multiple values for ", var_name,
          " in data frame for crop '", crop, "'. Using the first."
        )
      }
      if (length(vals) < 1) return(NA)
      vals[[1]]
    }
    
    # 1) filter & prep the guide
    g5 <- metrics_guide %>%
      filter(`Pertains to Table` == 5,
             `Metric Role`   %in% c("Number", "Percentage")) %>%
      mutate(
        MetricLabel = `Metric Name in Results Table (or summaries text)`,
        base_key    = str_to_lower(
          str_remove(MetricLabel,
                     regex("^(Number of |Percent of )", ignore_case = TRUE))
        )
      )
    
    num_tbl <- g5 %>%
      filter(`Metric Role` == "Number") %>%
      transmute(
        base_key,
        Metric    = MetricLabel,
        count_df  = `Name of Summary Variable (if summarized)`,
        count_var = `Name of Individual Metric Variable`
      )
    
    perc_tbl <- g5 %>%
      filter(`Metric Role` == "Percentage") %>%
      transmute(
        base_key,
        perc_df  = `Name of Summary Variable (if summarized)`,
        perc_var = `Name of Individual Metric Variable`
      )
    
    guide_full <- full_join(num_tbl, perc_tbl, by = "base_key")
    
    # 2) extract all crop names, correctly
    all_crops <- metric_dfs %>%
      map(~ .x[[crop_column]]) %>%
      flatten_chr() %>%
      unique()
    
    # 3) build one table per crop
    map(set_names(all_crops), function(crop) {
      guide_full %>%
        mutate(
          Number = map2_chr(count_df, count_var, function(df_name, var_name) {
            if (is.na(df_name) || is.na(var_name) || !(df_name %in% names(metric_dfs)))
              return("")
            val <- pull_one(metric_dfs[[df_name]], var_name, crop)
            if (!is.na(val)) format_int(val) else ""
          }),
          Percentage = map2_chr(perc_df, perc_var, function(df_name, var_name) {
            if (is.na(df_name) || is.na(var_name) || !(df_name %in% names(metric_dfs)))
              return("")
            val <- pull_one(metric_dfs[[df_name]], var_name, crop)
            safe_percent(val)
          })
        ) %>%
        select(Metric, Number, Percentage) %>%
        mutate(
          Number = ifelse(
            Metric == "Number of accessions in genebank collections safety duplicated in Svalbard" & Number == "",
            "–",
            Number
          ),
          Percentage = ifelse(
            Metric == "Number of accessions in genebank collections safety duplicated in Svalbard" & Percentage == "",
            "–",
            Percentage
          )
        )
    })
  }
  
