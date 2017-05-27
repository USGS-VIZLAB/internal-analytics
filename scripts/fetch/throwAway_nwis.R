#one-off to chunk big initial pulls

library(dplyr)
library(googleAnalyticsR)
library(data.table)
library(googleAuthR)
library(lubridate)
library(aws.s3)
library(aws.signature)

use_credentials(profile = "default")
currentDF <- s3readRDS(object = "internal-analytics/data/current.rds", 
                       bucket = "vizlab-data")

begin <- "2011" #Jan 1st
thru <- "2015" #dec 31st

starts <- seq(ymd(paste0(begin,'-01-01')), ymd(paste0(thru, "-01-01")), by = "years")
ends <- seq(ymd(paste0(begin, '-12-31')), ymd(paste0(thru,'-12-31')), by = "years")
#ends <- replace(ends, length(ends), Sys.Date() - 1)

gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')


#df of new accounts
accSub <- before2016
new_GA_DF <- data.frame()
for(v in accSub$id){
  view <- v
  #print(filter(accSub, viewId == view))
  for(i in 1:length(starts)) {
    dateRange = c(starts[i], ends[i])
    idDF <- google_analytics_4(viewId =view, date_range = dateRange, 
                               metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                               dimensions = c("hour", "date",
                                              "deviceCategory", 'region', 'source'), 
                               max = -1, anti_sample = TRUE)
    if(!is.null(idDF)){
      idDF <- mutate(idDF, viewID = view)
    }
    new_GA_DF <- bind_rows(new_GA_DF, idDF)
    #fwrite(x = new_GA_DF, file = "wwChunk.csv", 
    #quote = TRUE)
  }
}

currentDF <- bind_rows(currentDF, new_GA_DF)
saveRDS(object = currentDF, file = "cache/fetch/current.rds")
