#one-off to chunk the big NWIS call in case it fails midway

library(dplyr)
library(googleAnalyticsR)
library(data.table)
library(googleAuthR)
library(lubridate)
starts <- seq(ymd('2009-01-01'), ymd("2017-01-01"), by = "years")
ends <- c(seq(ymd('2010-01-01'), ymd('2017-01-01'), by = "years"), Sys.Date() -1)

gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')
new_GA_DF <- data.frame()
view <- "18857756"
for(i in 1:length(starts)) {
  dateRange = c(starts[i], ends[i])
  idDF <- google_analytics_4(viewId =view, date_range = dateRange, 
                             metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                             dimensions = c("year","month", "day", "hour",
                                            "deviceCategory", 'region', 'source'), 
                             max = -1, anti_sample = TRUE)
  idDF <- mutate(idDF, viewID = view)
  new_GA_DF <- bind_rows(new_GA_DF, idDF)
  fwrite(x = new_GA_DF, file = "wwChunk.csv", 
         quote = TRUE)
}