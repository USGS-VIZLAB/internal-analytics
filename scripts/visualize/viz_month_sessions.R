visualize.viz_month_sessions <- function(viz = as.viz("viz_month_sessions")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  viz.data <- viz.data %>%
    filter(date >= seq(Sys.Date(), length = 2, by = "-1 months")[2])
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    newUsers <- sum(sub_data$newUsers, na.rm = TRUE)
    sessions <- sum(sub_data$sessions, na.rm = TRUE) - newUsers
    
    percent_new <- 100*newUsers/(sum(sub_data$sessions, na.rm = TRUE))
    percent_new <- sprintf(percent_new, fmt = "%1.1f")
    
    x <- matrix(c(newUsers, sessions))
    row.names(x) <- c("New Users","Sessions")
    
    
    png(paste0("cache/visualize/",i,"_session_pie.png"))
      pie(x, labels = c(paste0("New", percent_new,"%"),
                      "Returning"))
    
    dev.off()
    
  }
  
  x <- data.frame(id = unique(viz.data$viewID),
                  loc = paste0("cache/visualize/",
                               unique(viz.data$viewID),
                               "_session_pie.png"),
                  type = "session_pie",
                  stringsAsFactors = FALSE)
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
