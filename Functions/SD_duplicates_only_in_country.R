

# safety duplication function 

duplicates_only_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(0) 
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      res <- 1    
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      return(0)
    }
  }
  return(res)
}
