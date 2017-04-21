visualize.viz_y_sessions <- function(viz){
  library(dplyr)
  library(ggplot2)
  
  height = viz[["height"]]
  width = viz[["width"]]
  dpi = viz[["dpi"]]
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  range_text <- viz[["rangetext"]]
  plot_type <- viz[["plottype"]]
  
  max_date <- max(viz.data$date, na.rm = TRUE)
  
  year_days = seq(max_date, length = 2, by = range_text)
  full_dates <- seq(max_date, length = -as.numeric(diff(year_days))+1, by=-1)
  empty_df <- data.frame(date = full_dates)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_year <- select(sub_data, date, sessions) %>%
      filter(date >= year_days[2],
             date <= year_days[1]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      data.frame() %>%
      right_join(empty_df, by="date") %>%
      arrange(desc(date))
    
    sub_data_year$sessions[is.na(sub_data_year$sessions)] <- 0
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    png(location, height = height, width = width)
    
    par(oma=c(0,0,0,0),
        mar=c(2,3,2,1),
        las=1, 
        mgp = c(2,0.5,0),
        tck=0.05)
    
    if(nrow(sub_data_year) > 0){
      
      if(range_text == "-1 week"){
        plot(x = sub_data_year$date, 
             sub_data_year$sessions, 
             type="b",xlab="",ylab="",yaxt='n',
             frame.plot = FALSE)
      } else {
        plot(x = sub_data_year$date, 
             sub_data_year$sessions, 
             type="l",xlab="",ylab="",yaxt='n',
             frame.plot = FALSE)
      }

    } else {
      plot(1, axes=FALSE, 
           type="n",xlab="",ylab="")
    }
      
    axis(1, at=c(par()$usr[1],par()$usr[2]), 
         labels = c("",""),lwd.tick=0)
    axis(2, at=c(0,max(sub_data_year$sessions)), 
         labels = c(0,max(sub_data_year$sessions)),tck = 0)
    title(main = paste0(range(sub_data_year$date), collapse = " to "), 
            cex.main = 1, adj=0)
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      

  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)

}
