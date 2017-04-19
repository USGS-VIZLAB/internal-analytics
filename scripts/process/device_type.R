process.device_type <- function(viz = as.viz("device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  

  
  saveRDS(session_users, file=viz[["location"]])
  
}
