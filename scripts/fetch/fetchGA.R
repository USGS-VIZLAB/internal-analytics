#fetch Google Analytics data for all viewIDs supplied
library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)
library(data.table)
library(assertthat)
library(aws.s3)
library(aws.signature)

fetch.GAviews <- function(viz) {
  #first get SB data - does it have data through yesterday? if no, and
  #update is true, run the fetcher for intervening time, and append, upload

  if(!viz[['useLocal']]) {
    print(sessionInfo())
    #get file from sb with fetcher
    message("Downloading from S3")
    use_credentials(profile = viz[['s3Profile']])
    save_object(object = viz[['s3Path']], bucket = viz[['bucket']],
                file = viz[['location']])
    message('Downloaded S3 file')
    fileDF <- readRDS(viz[['location']])
    
    
    if(viz[['update']]) {
      masterTable <- readDepends(viz)[['project_table']]
      #check if it is up to date (has yesterday's data) for each ID
      fileDF <- readRDS(viz[['location']])
      fileDF_summary <- group_by(fileDF, viewID) %>% summarise(lastDate = max(date))

      #all views should have current data right?
      needToUpdate <- filter(fileDF_summary, lastDate < Sys.Date() - 1)
      #get out of date AND new IDs
      viewID <- masterTable$viewID[!masterTable$viewID %in% fileDF_summary$viewID]
      newIDs <- data.frame(viewID = as.character(viewID),
                           lastDate = as.Date(rep("2016-01-01", length(viewID))),#arbitrary early start date
                           stringsAsFactors = FALSE)
      needToUpdate <- bind_rows(needToUpdate, newIDs)

      if(nrow(needToUpdate) > 0) {
        message("S3 file is out of date, updating from GA")
        new_GA_DF <- data.frame()
        gar_auth_service(file.path(Sys.getenv("HOME"), ".vizlab/VIZLAB-a48f4107248c.json"))
        for(i in seq_len(nrow(needToUpdate))) {
          #NOTE: only want to pull full days, so don't pull today's data!
          #this way we can just append the new data without having overlap
          dateRange <- c(as.character(needToUpdate$lastDate[i] + 1), as.character(Sys.Date() - 1 ))

          #API is limited to 7 dimensions per call
          idDF <- google_analytics_4(viewId = needToUpdate$viewID[i], date_range = dateRange,
                                     metrics = c("sessions", "users", 'newUsers', 'avgSessionDuration'),
                                     dimensions = c("date", "hour",
                                                    "deviceCategory", 'region', 'source'),
                                     max = -1, anti_sample = TRUE, slow_fetch = TRUE)
          if (!is.null(idDF)) {
            idDF <- mutate(idDF, viewID = needToUpdate$viewID[i])
            new_GA_DF <- bind_rows(new_GA_DF, idDF)
            print(paste("finished", needToUpdate$viewID[i]))
          }
        }

        allDF <- bind_rows(fileDF, new_GA_DF)
        assert_that(max(allDF$date) == (Sys.Date() - 1)) #note: is allDF$date char or date?

        saveRDS(allDF, file = viz[['location']])
        message("Updating S3...")
        put_object(file = viz[['location']], object = viz[['s3Path']],
                   bucket = viz[['bucket']])
        message("Done uploading")
      } else {
        message("S3 file is up to date, using that")
      }
    } else {message("update set to false in viz.yaml, using S3 file")}
  } else {message("useLocal = TRUE, not downloading anything")
      if(!file.exists(viz[['location']])) {
        stop("You don't have any local data!  Set useLocal = FALSE to download
             from S3")
      }
    }
}

