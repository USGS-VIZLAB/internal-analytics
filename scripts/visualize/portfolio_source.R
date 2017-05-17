visualize.portfolio_source <- function(viz = as.viz("portfolio_source")){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  source_sum <- viz.data %>%
    group_by(source) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  source_sum <- source_sum[1:min(c(10, nrow(source_sum))),]
  
  port_source <- ggplot(data = source_sum) +
    geom_col(aes(x = reorder(source, count), y=count)) +
    coord_flip() +
    theme_minimal() +
    ylab("Total Sessions") +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          panel.border = element_blank())
  
  ggsave(port_source, filename = viz[["location"]], 
         height = height, width = width)
  
}
