process.device_type <- function(viz = as.viz("device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  device_users <- select(viz.data, date, viewID,deviceCategory) %>%
    mutate(date = as.Date(date)) 
  
  saveRDS(device_users, file=viz[["location"]])
  
}
