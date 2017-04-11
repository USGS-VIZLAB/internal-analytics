#fetch Google Analytics data for all viewIDs supplied
library(googleAnalyticsR)
library(dplyr)
library(data.table)
library(sbtools)
fetch.GAviews <- function(viz) {
  #first get SB data - does it have data through yesterday? if no, and 
  #update is true, run the fetcher for intervening time, and append, upload
  
  #get file from sb with fetcher
  viz[['fetcher']] <- 'sciencebase'
  viz[['remoteFilename']] <- basename(viz[['location']])
  yamlIds <- viz[['viewIDs']]
  fetch(as.fetcher(viz))
  
  #check if it is up to date (at least has yesterday's data) for each ID
  fileDF <- fread(viz[['location']], colClasses = c(viewID = "character"))
  fileDF <- mutate(fileDF, date = as.Date(paste(year, month, day, sep = "-")))
  fileDF_summary <- group_by(fileDF, viewID) %>% summarise(lastDate = max(date)) 
  
  #all views should have current data right?
  needToUpdate <- filter(fileDF_summary, lastDate < Sys.Date())
  idsUpdate <- c(needToUpdate$viewID, yamlIds[!yamlIds %in% fileDF_summary$viewID])  
  
  if(length(idsUpdate) > 0) {
    message("Sciencebase file is out of date, updating from GA")
    dateRange <- c("2007-01-01", as.character(Sys.Date())) #arbitrary early start date
    allDF <- data.frame()
    for(i in ids) { 
      #API is limited to 7 dimensions per call 
      idDF <- google_analytics_4(viewId =i, date_range = dateRange, 
                                 metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                                 dimensions = c("year","month", "day", "hour",
                                                "deviceCategory", 'region', 'source'), 
                                 max = -1, anti_sample = TRUE)
      idDF <- mutate(idDF, viewID = i)
      allDF <- bind_rows(allDF, idDF)
      print(paste("finished", i))
    }
    fwrite(allDF, file = viz[['location']], quote = TRUE, row.names = FALSE)
    #update sb
    item_replace_files(viz[['remoteItemId']], viz[['location']])
  } else {
    message("Sciencebase file is up to date, using that")
  }
}

