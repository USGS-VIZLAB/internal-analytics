process.aggregate_ga <- function(viz) {
  #not really aggregating anymore - just drops > year old data, adds dateTime
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(assertthat)
  allDataDF <- readDepends(viz)[['fetchGA']] #list of all depends

  #drop data before longer than a year ago
  allDataDF <- allDataDF %>%
    mutate(date = as.Date(date)) %>%
    filter(date > (max(date) - duration(1, "year"))) 
  
  #make sure none of today's data snuck in on one-off GA pull
  assert_that(max(allDataDF$date) == (Sys.Date() - 1))
  assert_that(anyDuplicated(as.data.table(allDataDF)) == 0)
  #make sure there aren't duplicates!

  
  #add dateTime
  saveRDS(object = allDataDF, file=viz[["location"]], compress = FALSE)
}
