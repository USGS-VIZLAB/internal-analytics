visualize.portfolio_source <- function(viz = as.viz("portfolio_source")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  source_sum <- viz.data %>%
    group_by(source) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  source_sum <- source_sum[1:min(c(10, nrow(source_sum))),]
  
  source_sum$source <- gsub("\\."," ", source_sum$source)
  source_sum$source <- str_wrap(source_sum$source, width = 25)
  source_sum$source <- gsub(" ","\\.", source_sum$source)
  source_sum$source <- gsub("\n","\\.\n", source_sum$source)
  
  port_source <- ggplot(data = source_sum) +
    geom_col(aes(x = reorder(source, count), y=count), fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    ylab("Total Sessions") +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          panel.border = element_blank()) +
    scale_y_continuous(labels = comma)
  
  ggsave(port_source, filename = viz[["location"]], 
         height = height, width = width)
  
}
