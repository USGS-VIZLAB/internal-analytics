visualize.viz_y_sessions <- function(viz){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  range_text <- viz[["rangetext"]]
  plot_type <- viz[["plottype"]]
  
  year_days = seq(Sys.Date(), length = 2, by = range_text)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_year <- select(sub_data, date, sessions) %>%
      filter(date >= year_days[2]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) 
    
    location <- paste0("cache/visualize/",i,"_year.png")
    
    if(nrow(sub_data_year) > 0){
      
      png(location) 
      par(oma=c(0,0,0,0),mar=c(8,4,8,1),las=1)
      plot(x = sub_data_year$date, sub_data_year$sessions, axes=FALSE, 
           type="l",xlab="",ylab="")
      axis(1, at=c(0,max(sub_data_year$date)), 
           labels = c("",""),tck = 0)
      axis(2, at=c(0,max(sub_data_year$sessions)), 
           labels = c(0,max(sub_data_year$sessions)),tck = 0)
      dev.off()
    } else {
      png(location) 
      par(oma=c(0,0,0,0),mar=c(8,4,8,1),las=1)
        plot(1, axes=FALSE, 
             type="n",xlab="",ylab="")
        axis(1, at=c(0,10), 
             labels = c("",""),tck = 0)
        axis(2, at=c(0,10), 
             labels = c(0,10),tck = 0)      
      dev.off()
    }
      
      x <- bind_rows(x, data.frame(id = i,
                                   loc = location,
                                   type = plot_type,
                                   stringsAsFactors = FALSE))      

  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)

}
