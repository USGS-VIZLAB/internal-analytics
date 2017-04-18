process.sessions_and_new_users <- function(viz = as.viz("sessions_and_new_users")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  session_users <- select(viz.data, date,viewID,sessions,newUsers) %>%
                            mutate(date = as.Date(date)) %>%
                            filter(date >= seq(Sys.Date(), length = 2, by = "-1 months")[2])
  
  saveRDS(session_users, file=viz[["location"]])
  
}
