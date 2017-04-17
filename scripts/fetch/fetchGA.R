#fetch Google Analytics data for all viewIDs supplied
library(googleAnalyticsR)
library(dplyr)
library(data.table)
library(sbtools)
library(assertthat)
fetch.GAviews <- function(viz) {
  #first get SB data - does it have data through yesterday? if no, and 
  #update is true, run the fetcher for intervening time, and append, upload
  
  #get file from sb with fetcher
  viz[['fetcher']] <- 'sciencebase'
  viz[['remoteFilename']] <- basename(viz[['location']])
  fetch(as.fetcher(viz))
  message('Downloaded SB file')
  
  if(viz[['update']]) {
    masterTable <- fread('data/ga_table.csv', colClasses = "character")
    masterTable <- filter(masterTable, login == viz[['login']])
    #check if it is up to date (has yesterday's data) for each ID
    fileDF <- fread(viz[['location']], colClasses = c(viewID = "character", year = "character",
                                                      month = "character", day = "character",
                                                      hour = "character"))
    fileDF_date <- mutate(fileDF, date = as.Date(paste(year, month, day, sep = "-")))
    fileDF_summary <- group_by(fileDF_date, viewID) %>% summarise(lastDate = max(date)) 
    
    #all views should have current data right?
    needToUpdate <- filter(fileDF_summary, lastDate < Sys.Date() - 1)
    #get out of date AND new IDs
    viewID <- masterTable$viewID[!masterTable$viewID %in% fileDF_summary$viewID]
    newIDs <- data.frame(viewID = viewID,
                         lastDate = as.Date(rep("2007-01-01", length(viewID))),#arbitrary early start date
                         stringsAsFactors = FALSE)  
    needToUpdate <- bind_rows(needToUpdate, newIDs)
    
    if(nrow(needToUpdate) > 0) {
      message("Sciencebase file is out of date, updating from GA")
      new_GA_DF <- data.frame()
      ga_auth() #need to already have token 
      for(i in 1:nrow(needToUpdate)) { 
        #NOTE: only want to pull full days, so don't pull today's data!  
        #this way we can just append the new data without having overlap
        dateRange <- c(needToUpdate$lastDate[i], Sys.Date() - 1 )
        
        #API is limited to 7 dimensions per call 
        idDF <- google_analytics_4(viewId =needToUpdate$viewID[i], date_range = dateRange, 
                                   metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'), 
                                   dimensions = c("year","month", "day", "hour",
                                                  "deviceCategory", 'region', 'source'), 
                                   max = -1, anti_sample = TRUE)
        idDF <- mutate(idDF, viewID = needToUpdate$viewID[i])
        new_GA_DF <- bind_rows(new_GA_DF, idDF)
        print(paste("finished", needToUpdate$viewID[i]))
      }
      allDF <- bind_rows(fileDF, new_GA_DF)
      testDate <- mutate(allDF, date = paste(year, month, day, sep = "-"))  
      assert_that(!as.character(Sys.Date()) %in% testDate$date)
      assert_that(as.character(Sys.Date() - 1) %in% testDate$date)
      
      fwrite(allDF, file = viz[['location']], quote = TRUE, row.names = FALSE)
      #update sb
      item_replace_files(viz[['remoteItemId']], viz[['location']])
    } else {
      message("Sciencebase file is up to date, using that")
    }
  }
}

