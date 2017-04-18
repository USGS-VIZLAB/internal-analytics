process.sessions_and_new_users_daily <- function(viz = as.viz("sessions_and_new_users_daily")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  yesterday <- as.POSIXct(seq(Sys.Date()-1, length = 2, by = "-1 day")[1])
  the_day_before <- as.POSIXct(seq(Sys.Date()-1, length = 2, by = "-1 day")[2])
  
  session_users <- select(viz.data, dateTime,viewID,sessions,newUsers) %>%
    mutate(dateTime = as.POSIXct(dateTime)) %>%
    filter(dateTime >= the_day_before) %>%
    filter(dateTime <= yesterday) 
  
  saveRDS(session_users, file=viz[["location"]])
  
}