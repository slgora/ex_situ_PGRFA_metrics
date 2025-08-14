# -------------------------------------------------------------------------------------------
#' Generate Table 2: Crop-Specific Institutional Accession Tables (Top 80% + Summary Row)
#'
#' Takes a summary of institutional accession counts (plus precomputed fields)
#' and returns a named list of tibbles, one per Crop_strategy. Each tibble includes:
#'   • Institutions contributing to the top 80% of accessions (by count)
#'   • One summary row for remaining institutions, showing total count and how many there are
#'   • Columns:
#'       - Institution Code
#'       - Institution Name
#'       - Number of accessions
#'       - Percent of total (two decimals, with “%” suffix)
#'       - Cumulative percent (two decimals, with “%” suffix)
#'       - Number of accessions in long term storage (-18–20 C) and source
#'       - Number of accessions included in MLS (from GLIS)
#'
#' Adds comma separators for any count > 10,000.
#'
#' @param institution_accessions_summary Data frame with at least these columns:
#'   - Crop_strategy (chr)
#'   - INSTCODE (chr)
#'   - Institute_name (chr)
#'   - institution_accessions_count (num)
#'   - total_accessions (num)
#'   - institution_accessions_perc (num)
#'   - Number.of.accessions.in.long.term.storage.(-18-20.C).and.source (chr)
#'   - Number.of.accessions.included.in.MLS.(from.GLIS) (num)
#'
#' @return Named list of tibbles (one per crop) with columns:
#'   Institution Code | Institution Name | Number of accessions |
#'   Percent of total | Cumulative percent |
#'   Number of accessions in long term storage (-18–20 C) and source |
#'   Number of accessions included in MLS (from GLIS)
#'
#' @import dplyr purrr
#' @export
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
  library(dplyr)
  library(purrr)
  
  # -------------------------------------------------------------------------
  # Vectorized helper: turn x > 10,000 into comma‐separated text
  # -------------------------------------------------------------------------
  format_int <- function(x) {
    num <- as.numeric(x)
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
  
  # -------------------------------------------------------------------------
  # Split by crop, format each tibble
  # -------------------------------------------------------------------------
  institution_accessions_summary %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy))) %>%
    map(function(df) {
      df <- df %>%
        rename(
          storage_source_raw = `Number.of.accessions.in.long.term.storage.(-18-20.C).and.source`,
          mls_glis_raw       = `Number.of.accessions.included.in.MLS.(from.GLIS)`
        ) %>%
        arrange(desc(institution_accessions_count)) %>%
        mutate(
          cumulative_raw = cumsum(institution_accessions_count) / total_accessions * 100
        )
      
      top_df <- df %>%
        filter(cumulative_raw <= 80) %>%
        transmute(
          `Institution Code` = INSTCODE,
          `Institution Name` = Institute_name,
          `Number of accessions` = institution_accessions_count,
          `Percent of total` = sprintf("%.2f%%", institution_accessions_perc),
          `Cumulative percent` = sprintf("%.2f%%", cumulative_raw),
          `Number of accessions in long term storage (-18-20 C) and source` = storage_source_raw,
          `Number of accessions included in MLS (from GLIS)` = mls_glis_raw
        )
      
      remaining_df <- df %>% filter(cumulative_raw > 80)
      
      if (nrow(remaining_df) > 0) {
        other_row <- tibble(
          `Institution Code` = NA_character_,
          `Institution Name` = paste0("Other institutions (n = ", nrow(remaining_df), ")"),
          `Number of accessions` = sum(remaining_df$institution_accessions_count, na.rm = TRUE),
          `Percent of total` = sprintf("%.2f%%", sum(remaining_df$institution_accessions_perc, na.rm = TRUE)),
          `Cumulative percent` = "100.00%",
          `Number of accessions in long term storage (-18-20 C) and source` = NA_character_,
          `Number of accessions included in MLS (from GLIS)` = sum(as.numeric(remaining_df$mls_glis_raw), na.rm = TRUE)
        )
        
        top_df <- bind_rows(top_df, other_row)
      }
      
      top_df %>%
        mutate(
          across(
            c(
              `Number of accessions`,
              `Number of accessions included in MLS (from GLIS)`
            ),
            format_int
          )
        )
    })
}
