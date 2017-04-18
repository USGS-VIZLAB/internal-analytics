visualize.viz_y_sessions <- function(viz = as.viz("viz_y_sessions")){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    year_days = seq(Sys.Date(), length = 2, by = "-1 year")
    sub_data_year <- select(sub_data, date, sessions) %>%
      filter(date >= year_days[2]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) 
    
    png(paste0("cache/visualize/",i,"_year.png")) 
      par(oma=c(0,0,0,0),mar=c(8,4,8,1),las=1)
      plot(x = sub_data_year$date, sub_data_year$sessions, axes=FALSE, 
           type="l",xlab="",ylab="")
      axis(1, at=c(0,max(sub_data_year$date)), 
           labels = c("",""),tck = 0)
      axis(2, at=c(0,max(sub_data_year$sessions)), 
           labels = c(0,max(sub_data_year$sessions)),tck = 0)
    dev.off()
  }
  
  x <- data.frame(id = unique(viz.data$viewID),
                  loc = paste0("cache/visualize/",
                               unique(viz.data$viewID),
                               "_year.png"),
                  type = "year_line",
                  stringsAsFactors = FALSE)
  write.csv(x, file=viz[["location"]], row.names = FALSE)

  
}
