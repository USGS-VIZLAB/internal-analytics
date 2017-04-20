process.aggregate_ga <- function(viz) {
  library(dplyr)
  library(data.table)
  viz.data <- readDepends(viz) #list of all depends
  allDataDF <- do.call("bind_rows", viz.data)
  
  #drop data before 2014 we aren't using right now
  allDataDF <- allDataDF %>% mutate(date = as.Date(date)) %>% 
              filter(date > as.Date("2014-01-01"))
  
  #add dateTime
  fwrite(allDataDF, file=viz[["location"]], quote = TRUE, row.names = FALSE)
}