visualize.viz_device_type <- function(viz = as.viz("viz_device_type")){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["device_type"]]
  height = viz[["height"]]
  width = viz[["width"]]
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
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    port_device <-   ggplot(data = sub_data_range) +
      geom_col(aes(x = reorder(deviceCategory, totals), y=totals), fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      ylab("Total Sessions") +
      theme(axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            axis.text = element_text(size = 14),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    ggsave(port_device, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
