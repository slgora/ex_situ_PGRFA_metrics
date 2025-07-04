

# safety duplication function

duplicates_SDGV <- function(site) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(1)   
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}