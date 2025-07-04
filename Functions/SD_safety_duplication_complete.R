

# safety duplication function

safety_duplication_complete <- function(df, groupby = 'genus') {
  df <- df %>%
    mutate(
      SD_out_country = mapply(duplicates_out_country, duplSite_LIST, holding_country),
      SD_in_country  = mapply(duplicates_in_country,  duplSite_LIST, holding_country, instCode),
      SD_SDGV = mapply(duplicates_SDGV, duplSite_LIST),
      SD_only_in_country = mapply(duplicates_only_in_country, duplSite_LIST, holding_country, instCode),
      all_sd = SD_in_country + SD_SDGV + SD_out_country,
      no_SD = all_sd < 1
    )
  
  grouped <- df %>% group_by(!!sym(groupby))
  results <- grouped %>%
    summarise(
      SD_in_country = sum(SD_in_country),
      SD_out_country = sum(SD_out_country),
      SD_only_in_country = sum(SD_only_in_country),
      SD_SDGV = sum(SD_SDGV),
      no_SD = sum(no_SD),
      accessions_total = n()
    ) %>%
    arrange(desc(accessions_total)) %>%
    mutate(
      SD_in_country_per = 100 * (SD_in_country / accessions_total),
      SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
      SD_out_country_per = 100 * (SD_out_country / accessions_total),
      SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
      no_SD_per = 100 * (no_SD / accessions_total)
    ) %>%
    rename(
      `SD_in_country` = SD_in_country,
      `SD_out_country` = SD_out_country,
      `SD_only_in_country` = SD_only_in_country,
      `SD_SDGV` = SD_SDGV,
      `no_SD` = no_SD,
      `accessions_total` = accessions_total,
      `SD_in_country_per` = SD_in_country_per,
      `SD_only_in_country_per` = SD_only_in_country_per,
      `SD_out_country_per` = SD_out_country_per,
      `SD_SDGV_per` = SD_SDGV_per,
      `no_SD_per` = no_SD_per
    )
  return(results)
  #return(df) # line used only for debugging
}

