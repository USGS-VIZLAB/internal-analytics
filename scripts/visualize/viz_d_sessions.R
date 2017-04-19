visualize.viz_d_sessions <- function(viz){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users_daily"]]
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]

  
  for(i in unique(viz.data$viewID)){
    
    sub_data <- select(viz.data, dateTime, viewID, sessions) %>%
      filter(viewID == i) %>%
      group_by(dateTime) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) 
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    if(nrow(sub_data) > 0){
      
      png(location) 
      par(oma=c(0,0,0,0),mar=c(8,4,8,1),las=1)
      plot(x = sub_data$dateTime, sub_data$sessions, axes=FALSE, 
           type="b",xlab="",ylab="")
      axis(1, at=c(0,max(sub_data$dateTime)), 
           labels = c("",""),tck = 0)
      axis(2, at=c(0,max(sub_data$sessions)), 
           labels = c(0,max(sub_data$sessions)),tck = 0)
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
