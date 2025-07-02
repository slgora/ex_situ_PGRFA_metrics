

# safety duplication function 

duplicates_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      next
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      return(1)
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}
