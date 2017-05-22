visualize.viz_device_type <- function(viz = as.viz("viz_device_type")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["device_type"]]
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    sub_data_range <- sub_data %>%
      filter(date >= range_days[2]) %>%
      select(-date) %>%
      group_by(deviceCategory) %>%
      summarize(totals = n())
    
    if(nrow(sub_data_range) <3){
      sub_data_range <- rbind(sub_data_range[,c("deviceCategory","totals")],
                              data.frame(deviceCategory=rep("",3-nrow(sub_data_range)),
                                        totals = rep(NA,3-nrow(sub_data_range)),
                                        stringsAsFactors = FALSE) )
    }
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    port_device <-   ggplot(data = sub_data_range) +
      geom_col(aes(x = reorder(deviceCategory, totals), y=totals), fill = bar_line_col) +
      coord_flip() +
      theme_minimal() +
      ylab("Total Sessions") +
      theme(axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            axis.text = element_text(size = 14),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      scale_y_continuous(labels = comma)
    
    ggsave(port_device, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
