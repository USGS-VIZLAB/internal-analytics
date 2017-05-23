visualize.portfolio_source <- function(viz = as.viz("portfolio_source")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(grid)

  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  viz.data <- readDepends(viz)[["viz_data"]]

  viz.data <- viz.data %>%
    select(-date)
  
  port_source <- plot_sources(viz.data, bar_line_col)

  ggsave(port_source, filename = viz[["location"]], 
         height = height, width = width)
  
}

visualize.viz_source <- function(viz = as.viz("viz_source")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  viz.data <- readDepends(viz)[["viz_data"]]
  
  viz.data <-  viz.data %>%
    select(-date)
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    port_source <- plot_sources(sub_data, bar_line_col)
    
    ggsave(port_source, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}

plot_sources <- function(viz.data, bar_line_col){
  source_sum <- viz.data %>%
    group_by(source) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  source_sum <- source_sum[1:min(c(10, nrow(source_sum))),]
  
  source_sum$source <- gsub("\\."," ", source_sum$source)
  source_sum$source <- str_wrap(source_sum$source, width = 25)
  source_sum$source <- gsub(" ","\\.", source_sum$source)
  source_sum$source <- gsub("\n","\\.\n", source_sum$source)
  
  if(nrow(source_sum) == 0){
    source_sum <- data.frame(source=c("google","(direct)"),
                           count = c(NA,NA),
                           stringsAsFactors = FALSE) 
  } 
  
  port_source <- ggplot(data = source_sum) +
    geom_col(aes(x = reorder(source, count), y=count), fill = bar_line_col) +
    coord_flip() +
    theme_minimal() +
    ylab("Total Sessions") +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          panel.border = element_blank(),
          plot.margin=unit(c(0.1,1,0.1,0.1),"cm")) +
    scale_y_continuous(labels = comma)
  return(port_source)
}

