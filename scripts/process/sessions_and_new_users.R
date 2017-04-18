process.sessions_and_new_users <- function(viz = as.viz("sessions_and_new_users")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  session_users <- select(viz.data, date,viewID,sessions,newUsers) %>%
                            mutate(date = as.Date(date)) 
  
  saveRDS(session_users, file=viz[["location"]])
  
}
