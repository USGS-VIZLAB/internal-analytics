visualize.viz_month_sessions <- function(viz = as.viz("viz_month_sessions")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  
  viz.data <- viz.data %>%
    filter(date >= seq(max(viz.data$date, na.rm = TRUE), length = 2, by = "-1 months")[2])
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    newUsers <- sum(sub_data$newUsers, na.rm = TRUE)
    returningUsers <- sum(sub_data$sessions, na.rm = TRUE) - newUsers

    x <- data.frame(sessions = c(newUsers,returningUsers),
                    users = c("New Users","Returning Users"),
                    stringsAsFactors = FALSE)
    
    location <- paste0("cache/visualize/",i,"_session_pie.png")
    
    port_source <- ggplot(data = x) +
      geom_col(aes(x = reorder(users, sessions), y=sessions), fill = bar_line_col) +
      coord_flip() +
      theme_minimal() +
      ylab("Sessions") +
      theme(axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 14),
            panel.border = element_blank()) +
      scale_y_continuous(labels = comma)
    
    ggsave(port_source, filename = location, 
           height = height, width = width)
    
  }
  
  x <- data.frame(id = unique(viz.data$viewID),
                  loc = paste0("cache/visualize/",
                               unique(viz.data$viewID),
                               "_session_pie.png"),
                  type = "session_pie",
                  stringsAsFactors = FALSE)
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
