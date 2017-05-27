process.year_filter <- function(viz) {
  #not really aggregating anymore - just drops > year old data, adds dateTime
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(assertthat)
  deps <- readDepends(viz)
  allDataDF <- deps[['fetchGA']] #list of all depends
  table <- deps[['project_table']]
  #drop data before longer than a year ago, and not in the table yaml
  allDataDF <- allDataDF %>%
    mutate(date = as.Date(date)) %>%
    filter(date > (max(date) - duration(1, "year")), viewID %in% table$viewID) 
  
  #make sure none of today's data snuck in on one-off GA pull
  assert_that(max(allDataDF$date) <= (Sys.Date() - 1))
  assert_that(anyDuplicated(as.data.table(allDataDF)) == 0)
  #make sure there aren't duplicates!

  
  #add dateTime
  saveRDS(object = allDataDF, file=viz[["location"]], compress = FALSE)
}
