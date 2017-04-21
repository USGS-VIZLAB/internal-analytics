visualize.viz_d_sessions <- function(viz){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users_daily"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]

  max_date <- max(viz.data$dateTime, na.rm = TRUE)
  full_dates <- seq(max_date, length = 24, by=-60*60)
  empty_df <- data.frame(dateTime=full_dates)
  
  for(i in unique(viz.data$viewID)){
    
    sub_data <- select(viz.data, dateTime, viewID, sessions) %>%
      filter(viewID == i) %>%
      group_by(dateTime) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      data.frame() %>%
      right_join(empty_df, by="dateTime") %>%
      arrange(desc(dateTime))
    
    sub_data$sessions[is.na(sub_data$sessions)] <- 0
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    png(location, width = width, height = height)
    par(oma=c(0,0,0,0),
        mar=c(2,3,2,1),
        las=1, 
        mgp = c(2,0.5,0),
        tck=0.05)
    
    if(nrow(sub_data) > 0){
      plot(x = sub_data$dateTime, 
           sub_data$sessions, 
           type="b",xlab="",ylab="",yaxt='n',
           frame.plot = FALSE)
    } else { 
      plot(1, axes=FALSE, 
           type="n",xlab="",ylab="")
    }
    axis(1, at=c(par()$usr[1],par()$usr[2]), 
         labels = c("",""),lwd.tick=0)
    axis(2, at=c(par()$usr[3],max(sub_data$sessions)), 
         labels = c("",max(sub_data$sessions)),tck = 0)
    title(main = paste0(range(sub_data$dateTime), collapse = " to "), 
          cex.main = 1, adj=0)
    dev.off()
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
