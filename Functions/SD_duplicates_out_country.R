
# SD function 
duplicates_out_country <- function(site, pat) {
  res <- 0
  for (i in site) {
    if (i %in% c('NOR051', '', 'nan') || substr(i, 1, 3) == pat) {
      next
    } else {
      return(1)
    }
  }
  return(res)
}