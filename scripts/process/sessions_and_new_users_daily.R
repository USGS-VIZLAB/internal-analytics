process.sessions_and_new_users_daily <- function(viz = as.viz("sessions_and_new_users_daily")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["clean_up_raw"]]
  
  yesterday <- seq(Sys.Date()-1, length = 2, by = "-1 day")[1]
  the_day_before <- seq(Sys.Date()-1, length = 2, by = "-1 day")[2]
  
  session_users <- select(viz.data, date, dateTime,viewID,sessions,newUsers) %>%
    mutate(date = as.Date(date)) %>%
    filter(dateTime > the_day_before) %>%
    filter(dateTime <= yesterday) %>%
    mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
    select(-date)

  
  saveRDS(session_users, file=viz[["location"]])
  
}