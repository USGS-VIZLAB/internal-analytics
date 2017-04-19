process.source_counts <- function(viz = as.viz("source_counts")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  range_text <- viz[["rangetext"]]
  
  range_days = seq(Sys.Date(), length = 2, by = range_text)
  
  source_counts <- select(viz.data, date, viewID,source) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= range_days[2]) %>%
    select(-date)
  
  saveRDS(source_counts, file=viz[["location"]])
  
}