# -------------------------------------------------------------------------------------------
#' Generate Table 2: Top 20 Institutional Accession Tables by Crop Strategy
#'
#' Produces a named list of tibbles, one per Crop_strategy, showing:
#'   • Top 20 institutions by accession count
#'   • One summary row aggregating all remaining institutions
#'   • Cumulative percentage of accessions for each row
#'
#' Each tibble includes the following columns:
#'   - Institution Code
#'   - Institution Name
#'   - Number of accessions
#'   - Percent of total (formatted to two decimals with “%” suffix)
#'   - Cumulative percent (formatted to two decimals with “%” suffix)
#'   - Number of accessions in long term storage (-18–20 C) and source
#'   - Number of accessions included in MLS (from GLIS)
#'   - Number of accessions included in MLS (from genebank collections databases)
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
#'   - Number.of.accessions.included.in.MLS.(from.genebank.collections.databases) (num)
#'
#' @return Named list of tibbles (one per crop) with formatted columns
#'
#' @import dplyr purrr
#' @export
# -------------------------------------------------------------------------------------------
generate_table2 <- function(institution_accessions_summary) {
  library(dplyr)
  library(purrr)
  
  # Helper: Format integers with commas if > 10,000
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
  
  institution_accessions_summary %>%
    arrange(Crop_strategy, desc(institution_accessions_count)) %>%
    group_split(Crop_strategy) %>%
    set_names(map_chr(., ~ unique(.x$Crop_strategy))) %>%
    map(function(df) {
      df <- df %>%
        rename(
          storage_source_raw = `Number.of.accessions.in.long.term.storage.(-18-20.C).and.source`,
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
          `Percent of total` = sprintf("%.2f%%", institution_accessions_perc),
          `Cumulative percent` = sprintf("%.2f%%", cumulative_raw),
          `Number of accessions in long term storage (-18-20 C) and source` = storage_source_raw,
          `Number of accessions included in MLS (from GLIS)` = mls_glis_raw,
          `Number of accessions included in MLS (from genebank collections databases)` = mls_genebank_raw
        )
      
      remaining_df <- df %>% slice(-(1:20))
      
      if (nrow(remaining_df) > 0) {
        # Sum numeric part of storage for "other institutions"
        total_storage_other <- sum(extract_storage_count(remaining_df$storage_source_raw), na.rm = TRUE)
        storage_other_display <- if (total_storage_other > 0) {
          num_str <- format(total_storage_other, big.mark = ",", scientific = FALSE, trim = TRUE)
          paste0(num_str, " (storage=13)")
        } else NA_character_
        
        other_row <- tibble(
          `Institution Code` = NA_character_,
          `Institution Name` = paste0("Other institutions (n = ", nrow(remaining_df), ")"),
          `Number of accessions` = sum(remaining_df$institution_accessions_count, na.rm = TRUE),
          `Percent of total` = sprintf("%.2f%%", sum(remaining_df$institution_accessions_perc, na.rm = TRUE)),
          `Cumulative percent` = "100.00%",
          `Number of accessions in long term storage (-18-20 C) and source` = storage_other_display,
          `Number of accessions included in MLS (from GLIS)` = sum(as.numeric(remaining_df$mls_glis_raw), na.rm = TRUE),
          `Number of accessions included in MLS (from genebank collections databases)` = sum(as.numeric(remaining_df$mls_genebank_raw), na.rm = TRUE)
        )
        
        top_df <- bind_rows(top_df, other_row)
      }
      
      # Format necessary columns
      top_df %>%
        mutate(
          across(
            c(
              `Number of accessions`,
              `Number of accessions included in MLS (from GLIS)`,
              `Number of accessions included in MLS (from genebank collections databases)`
            ),
            format_int
          ),
          `Number of accessions in long term storage (-18-20 C) and source` =
            format_storage_string(`Number of accessions in long term storage (-18-20 C) and source`)
        )
    })
}
