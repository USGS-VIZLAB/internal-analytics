visualize.viz_y_sessions <- function(viz){
  library(dplyr)
  library(ggplot2)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
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
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    if(nrow(sub_data_year) > 0){
      
      gplot <- ggplot(data = sub_data_year, aes(x=date, y=sessions))
      
      if(range_text == "-1 week"){
        gplot <- gplot +
          geom_line(aes(x=date, y=sessions)) +
          geom_point() + geom_line()
      } else {
        gplot <- gplot +
          geom_line()
      }

    } else {
      gplot <- ggplot(data = data.frame(date= 1, sessions = 1)) 
    }
      
    gplot <- gplot +
      theme_minimal() +
      ylab("") + xlab("") +
      labs(title = paste(range(sub_data_year$date), collapse = " to ")) +
      theme(plot.title = element_text(size=7))
    
    ggsave(location, width = width, height = height, dpi = 72)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      

  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)

}
