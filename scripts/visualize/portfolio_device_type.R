visualize.portfolio_device_type <- function(viz = as.viz("portfolio_device_type")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["device_type"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  range_text <- viz[["rangetext"]]
  range_days = seq(Sys.Date(), length = 2, by = range_text)
  
  sub_data_range <- viz.data %>%
    filter(date >= range_days[2]) %>%
    select(-date) %>%
    group_by(deviceCategory) %>%
    summarize(totals = n())
    
  png(viz[["location"]], height = height, width = width) 
  par(oma=c(0,0,0,0),las=1)
  if(nrow(sub_data_range) > 0){
    barplot(rev(sub_data_range$totals), horiz=TRUE,
            names.arg=rev(sub_data_range$deviceCategory))
    
  } else {
    barplot(c(0,0,0), horiz=TRUE,
            names.arg=c("desktop","mobile","tablet"),
            xlim = c(0,10))    
  }
  dev.off()

}
