#one-off to chunk the big NWIS call in case it fails midway

library(dplyr)
library(googleAnalyticsR)
library(data.table)
library(googleAuthR)
library(lubridate)

begin <- "2016" #Jan 1st
thru <- "2017" #dec 31st

starts <- seq(ymd(paste0(begin,'-01-01')), ymd(paste0(thru, "-01-01")), by = "years")
ends <- seq(ymd(paste0(begin, '-12-31')), ymd(paste0(thru,'-12-31')), by = "years")

gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')
new_GA_DF <- data.frame()
view <- '83660223'
for(i in 1:length(starts)) {
  dateRange = c(starts[i], ends[i])
  idDF <- google_analytics_4(viewId =view, date_range = dateRange, 
                             metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                             dimensions = c("hour", "date",
                                            "deviceCategory", 'region', 'source'), 
                             max = -1, anti_sample = TRUE)
  idDF <- mutate(idDF, viewID = view)
  new_GA_DF <- bind_rows(new_GA_DF, idDF)
  #fwrite(x = new_GA_DF, file = "wwChunk.csv", 
         #quote = TRUE)
}