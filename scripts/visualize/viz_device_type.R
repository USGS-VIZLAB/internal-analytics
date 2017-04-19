visualize.viz_device_type <- function(viz = as.viz("viz_device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["device_type"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  
  range_days = seq(Sys.Date(), length = 2, by = range_text)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_range <- sub_data %>%
      filter(date >= range_days[2]) %>%
      select(-date) %>%
      group_by(deviceCategory) %>%
      summarize(totals = n())
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    if(nrow(sub_data_range) > 0){
      
      png(location) 
      par(oma=c(0,0,0,0),las=1)
      barplot(rev(sub_data_range$totals), horiz=TRUE,
              names.arg=rev(sub_data_range$deviceCategory))
      dev.off()
    } else {
      png(location) 
      par(oma=c(0,0,0,0),las=1)
      barplot(c(0,0,0), horiz=TRUE,
              names.arg=c("desktop","mobile","tablet"),
              xlim = c(0,10))    
      dev.off()
    }
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
