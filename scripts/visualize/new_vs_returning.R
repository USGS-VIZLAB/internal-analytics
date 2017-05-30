visualize.viz_new_vs_returning <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["viz_data"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  plottype <- viz[["plottype"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    port_source <- new_return_plot(sub_data, bar_line_col)
    
    location <- paste0("cache/visualize/",i,plottype,".png")
    
    ggsave(port_source, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  

  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}

visualize.viz_new_vs_returning_portfolio <- function(viz = as.viz("portfolio_new_return_year")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["viz_data"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]

  port_source <- new_return_plot(viz.data, bar_line_col)
  
  ggsave(port_source, filename = viz[["location"]], 
         height = height, width = width)
    
}

new_return_plot <- function(viz.data, bar_line_col){
  library(ggplot2)
  library(scales)
  
  newUsers <- sum(viz.data$newUsers, na.rm = TRUE)
  returningUsers <- sum(viz.data$sessions, na.rm = TRUE) - newUsers
  
  x <- data.frame(sessions = c(newUsers,returningUsers),
                  users = c("New Users","Returning Users"),
                  stringsAsFactors = FALSE)
  
  port_source <- ggplot(data = x) +
    geom_col(aes(x = reorder(users, sessions), y=sessions), fill = bar_line_col) +
    coord_flip() +
    theme_minimal() +
    ylab("Sessions") +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          panel.border = element_blank(),
          plot.margin=unit(c(0.1,1,0.1,0.1),"cm")) +
    scale_y_continuous(labels = comma)
  
  return(port_source)
}
