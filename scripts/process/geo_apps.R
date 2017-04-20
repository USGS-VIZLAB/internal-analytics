process.geo_apps <- function(viz = as.viz("geo_apps")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  range_text <- viz[["rangetext"]]
  
  range_days = seq(Sys.Date(), length = 2, by = range_text)
  
  regions <- select(viz.data, date, viewID, region) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= range_days[2]) %>%
    select(-date)
  
  saveRDS(regions, file=viz[["location"]])
  
}