process.aggregate_ga <- function(viz) {
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(assertthat)
  viz.data <- readDepends(viz) #list of all depends
  allDataDF <- do.call("bind_rows", viz.data)
  rm(viz.data) # free memory

  #drop data before longer than a year ago
  allDataDF <- allDataDF %>%
    mutate(date = as.Date(date)) %>%
    filter(date > (max(date) - duration(1, "year"))) %>%
    distinct()
  
  #make sure none of today's data snuck in on one-off GA pull
  assert_that(max(allDataDF$date) == (Sys.Date() - 1))
  
  #add dateTime
  saveRDS(object = allDataDF, file=viz[["location"]], compress = FALSE)
}
