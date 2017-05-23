visualize.portfolio_device_type <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["viz_data"]]
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  viz.data <- select(viz.data, viewID,deviceCategory) 
  
  port_device <- plot_device(viz.data, bar_line_col)
  
  ggsave(port_device, filename = viz[["location"]], 
         height = height, width = width)

}

visualize.viz_device_type <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["viz_data"]]
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]

  viz.data <- select(viz.data, viewID,deviceCategory) 
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)

    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    port_device <- plot_device(sub_data, bar_line_col)
    
    ggsave(port_device, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}

plot_device <- function(viz.data, bar_line_col){
  
  sub_data_range <- viz.data %>%
    group_by(deviceCategory) %>%
    summarize(totals = n())
  
  if(nrow(sub_data_range) == 0){
    sub_data_range <- data.frame(deviceCategory = c("desktop","mobile","tablet"),
                                 totals = c(NA,NA,NA),
                                 stringsAsFactors = FALSE)
  }
  
  port_device <-   ggplot(data = sub_data_range) +
    geom_col(aes(x = reorder(deviceCategory, totals), y=totals), fill = bar_line_col) +
    coord_flip() +
    theme_minimal() +
    ylab("Total Sessions") +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_text(size = 14),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin=unit(c(0.1,1,0.1,0.1),"cm")) +
    scale_y_continuous(labels = comma)
  
  return(port_device)
}
