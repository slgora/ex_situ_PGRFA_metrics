##### download data from GLIS using the API, save JSON files in working directory
crops <- read_excel("../Data_6/processing/croplist_PG.xlsx")
crops$genera <- trimws(ifelse(is.na(crops$Genera_primary), "", crops$Genera_primary)) %>% 
  paste(trimws(ifelse(is.na(crops$Genera_synonyms), "", crops$Genera_synonyms)), sep = ",")

genera = c(crops$Genera_primary , crops$Genera_synonyms)  # use also synonyms
genera = genera[!is.na(genera)]   # drop NA

# call glis_API function for each Genus listed in genera
# added line to save results for each item in the loop
results <- lapply(genera, function(g) {
  params <- list(
    `_format` = "json",
    `_pretty` = FALSE,
    `per-page` = 100,
    genus = g
  )
  res = call_glis_API(parameters = params, max_retries = 100, wait_seconds = 10)
  write_json(res, path = paste0("GLIS_result_", g, ".json"), pretty = TRUE, auto_unbox = TRUE)
  return(res)
})
