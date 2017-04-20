process.sessions_and_new_users_daily <- function(viz = as.viz("sessions_and_new_users_daily")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  yesterday <- seq(Sys.Date()-1, length = 2, by = "-1 day")[1]
  the_day_before <- seq(Sys.Date()-1, length = 2, by = "-1 day")[2]
  
  session_users <- select(viz.data, date, hour, viewID,sessions,newUsers) %>%
    mutate(date = as.Date(date)) %>%
    filter(date > the_day_before) %>%
    filter(date <= yesterday) %>%
    mutate(dateTime = as.POSIXct(paste0(date, "T", hour, ":00:00"), 
                                 format = "%Y-%m-%dT%H:%M:%SZ")) %>%
    select(-date)

  
  saveRDS(session_users, file=viz[["location"]])
  
}