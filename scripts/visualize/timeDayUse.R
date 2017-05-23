#portfolio-wide
library(dplyr)
library(ggplot2)
library(scales)

visualize.timeDayUse_all <- function(viz=as.viz("timeDayUse_port")) {
  
  viz.data <- readDepends(viz)[["aggregate_ga"]] #not sure which
  hourSum <- group_by(viz.data, hour) %>% summarise(n = n()) #need to set to numeric?
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  hourSum$hour <- as.numeric(hourSum$hour)
  
  port_device <-   ggplot(data = hourSum) +
    geom_col(aes(x = hour, y=n)) +
    theme_minimal() +
    ylab("Sessions") +
    xlab("Hour of the Day") +
    theme(axis.title.y=element_blank(),
          axis.text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = comma) 
  
  ggsave(port_device, filename = viz[["location"]], 
         height = height, width = width)
  
  
}

visualize.timeDayUse_app <- function(viz=as.viz("timeDayUse_app")) {
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  plot_type <- viz[["plottype"]]
  range_text <- viz[["rangetext"]]
  bar_line_col = viz[["bar_line_col"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  range_days = rev(seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text))
  
  for(i in unique(viz.data$viewID)) {
    location <- paste0("cache/visualize/timeDayUse_", i, ".png")
    
    hourSum <- filter(viz.data, viewID == i) %>% 
      filter(date >= range_days[1]) %>%
      group_by(hour) %>% 
      summarise(n = n()) #need to set to numeric?

    hourSum$hour <- as.numeric(hourSum$hour)
    
    port_device <-   ggplot(data = hourSum) +
      geom_col(aes(x = hour, y=n), fill = bar_line_col) +
      theme_minimal() +
      ylab("Sessions") +
      xlab("Hour of the Day") +
      theme(axis.title.y=element_blank(),
            axis.text = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
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


fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
