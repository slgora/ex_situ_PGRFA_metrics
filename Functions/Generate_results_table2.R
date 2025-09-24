# -------------------------------------------------------------------------------------------
#' Generate Table 2: Top 20 Institutional Accession Tables by Crop Strategy
#'
#' For vegetative crops (Aroids, Breadfruit, Cassava, Strawberry, Sweetpotato), replaces
#' the long-term storage column with "Number of accessions conserved in vitro or in cryo storage"
#' using the count_with_30_or_40 column from storage_30_or_40_metric_byinst.
#'
#' Data prep (joining and column selection) is performed inside the function.
#'
#' @param institution_accessions_summary Data frame with institutional metrics (by crop/institution)
#' @param storage_30_or_40_metric_byinst Data frame with count_with_30_or_40 by crop/institution
#'
#' @return Named list of tibbles (one per crop) with formatted columns
#'
#' @import dplyr purrr tidyr
#' @export
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary, storage_30_or_40_metric_byinst) {
  library(dplyr)
  library(purrr)
  library(tidyr)
  
  vegetative_crops <- c("Aroids", "Breadfruit", "Cassava", "Strawberry", "Sweetpotato")
  
  # Helper: Format integers with commas if > 10,000
  format_int <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    out <- as.character(num)
    out[is.na(num)] <- NA_character_
    big <- !is.na(num) & num > 1e4
    out[big] <- format(
      num[big],
      big.mark    = ",",
      scientific  = FALSE,
      trim        = TRUE
    )
    out
  }
  
  # Helper: Format storage strings, adding comma to leading number if >10,000
  format_storage_string <- function(x) {
    sapply(x, function(val) {
      if (is.na(val) || val == "") return(NA_character_)
      m <- regexec("^([0-9,]+)(.*)", val)
      regmatch <- regmatches(val, m)[[1]]
      if (length(regmatch) != 3) return(val) # No match, return original
      num <- suppressWarnings(as.numeric(gsub(",", "", regmatch[2])))
      rest <- regmatch[3]
      if (!is.na(num) & num > 1e4) {
        num_str <- format(num, big.mark = ",", scientific = FALSE, trim = TRUE)
        paste0(num_str, rest)
      } else {
        paste0(regmatch[2], rest)
      }
    }, USE.NAMES = FALSE)
  }
  
  # Helper: Extract the numeric part from storage string (e.g. "5652 (storage=13)" -> 5652)
  extract_storage_count <- function(x) {
    num <- suppressWarnings(as.numeric(sub("^([0-9,]+).*", "\\1", x)))
    num[is.na(num)] <- 0
    num
  }
  
  # Join summary with storage_30_or_40 by crop/institution
  df <- institution_accessions_summary %>%
    left_join(
      storage_30_or_40_metric_byinst %>% 
        select(Crop_strategy, INSTCODE, count_with_30_or_40),
      by = c("Crop_strategy", "INSTCODE")
    )
  
  # Main split and formatting
  df %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy))) %>%
    map(function(df) {
      crop <- unique(df$Crop_strategy)
      is_veg <- crop %in% vegetative_crops
      
      # ---- Column setup: choose which storage column to use and what label ----
      if (is_veg) {
        storage_col <- "count_with_30_or_40"
        storage_label <- "Number of accessions conserved in vitro or in cryo storage"
        storage_for_other <- function(x) sum(as.numeric(x), na.rm = TRUE)
        storage_format <- function(x) format_int(x)
      } else {
        storage_col <- "Number.of.accessions.in.long.term.storage.(-18-20.C).and.source"
        storage_label <- "Number of accessions in long term storage (-18-20 C) and source"
        storage_for_other <- function(x) {
          total <- sum(extract_storage_count(x), na.rm = TRUE)
          if (total > 0) paste0(format(total, big.mark = ",", scientific = FALSE, trim = TRUE), " (storage=13)")
          else NA_character_
        }
        storage_format <- function(x) format_storage_string(x)
      }
      
      df <- df %>%
        rename(
          mls_glis_raw       = `Number.of.accessions.included.in.MLS.(from.GLIS)`,
          mls_genebank_raw   = `Number.of.accessions.included.in.MLS.(from.genebank.collections.databases)`
        ) %>%
        arrange(desc(institution_accessions_count)) %>%
        mutate(
          cumulative_raw = cumsum(institution_accessions_count) / total_accessions * 100
        )
      
      top_df <- df %>%
        slice_head(n = 20) %>%
        transmute(
          `Institution Code` = INSTCODE,
          `Institution Name` = Institute_name,
          `Number of accessions` = institution_accessions_count,
          `Percent of total` = institution_accessions_perc,
          `Cumulative percent` = cumulative_raw,
          !!storage_label := .data[[storage_col]],
          `Number of accessions included in MLS (from GLIS)` = mls_glis_raw,
          `Number of accessions included in MLS (from genebank collections databases)` = mls_genebank_raw
        )
      
      remaining_df <- df %>% slice(-(1:20))
      
      if (nrow(remaining_df) > 0) {
        storage_other_val <- storage_for_other(remaining_df[[storage_col]])
        other_row <- tibble(
          `Institution Code` = NA_character_,
          `Institution Name` = paste0("Other institutions (n = ", nrow(remaining_df), ")"),
          `Number of accessions` = sum(remaining_df$institution_accessions_count, na.rm = TRUE),
          `Percent of total` = sum(remaining_df$institution_accessions_perc, na.rm = TRUE),
          `Cumulative percent` = 100,
          !!storage_label := storage_other_val,
          `Number of accessions included in MLS (from GLIS)` = sum(as.numeric(remaining_df$mls_glis_raw), na.rm = TRUE),
          `Number of accessions included in MLS (from genebank collections databases)` = sum(as.numeric(remaining_df$mls_genebank_raw), na.rm = TRUE)
        )
        top_df <- bind_rows(top_df, other_row)
      }
      
      # Format columns after binding
      top_df <- top_df %>%
        mutate(
          `Number of accessions` = format_int(`Number of accessions`),
          `Percent of total` = sprintf("%.2f%%", as.numeric(`Percent of total`)),
          `Cumulative percent` = sprintf("%.2f%%", as.numeric(`Cumulative percent`)),
          `Number of accessions included in MLS (from GLIS)` = format_int(`Number of accessions included in MLS (from GLIS)`),
          `Number of accessions included in MLS (from genebank collections databases)` = format_int(`Number of accessions included in MLS (from genebank collections databases)`),
          !!storage_label := storage_format(.data[[storage_label]])
        )
      
      top_df
    })
}
