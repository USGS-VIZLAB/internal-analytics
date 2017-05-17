visualize.portfolio_device_type <- function(viz = as.viz("portfolio_device_type")){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["device_type"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  range_text <- viz[["rangetext"]]
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  sub_data_range <- viz.data %>%
    filter(date >= range_days[2]) %>%
    select(-date) %>%
    group_by(deviceCategory) %>%
    summarize(totals = n())
    
  max_char = max(nchar(sub_data_range$deviceCategory), na.rm = TRUE)
  
  if(nrow(sub_data_range) == 0){
    sub_data_range <- data.frame(deviceCategory = c("desktop","mobile","tablet"),
                                 totals = c(NA,NA,NA),
                                 stringsAsFactors = FALSE)
  }
  
  port_device <-   ggplot(data = sub_data_range) +
    geom_col(aes(x = reorder(deviceCategory, totals), y=totals)) +
    coord_flip() +
    theme_minimal() +
    ylab("Total Sessions") +
    theme(axis.title.y=element_blank(),
          axis.text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  ggsave(port_device, filename = viz[["location"]], 
         height = height, width = width)

}
