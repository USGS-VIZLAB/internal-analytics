visualize.viz_month_sessions <- function(viz = as.viz("viz_month_sessions")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    newUsers <- sum(sub_data$newUsers, na.rm = TRUE)
    sessions <- sum(sub_data$sessions, na.rm = TRUE) - newUsers
    x <- matrix(c(newUsers, sessions))
    row.names(x) <- c("New Users","Sessions")
    
    
    png(paste0("cache/visualize/",i,"_session_bar.png"))
    par(mar=c(9,1,9,1))
    barplot(x, horiz=TRUE, axes=FALSE)
    axis(1, lwd = 2, at = pretty(c(0, sum(x))))
    dev.off()
    
  }
  
  x <- data.frame(id = unique(viz.data$viewID),
                  loc = paste0("cache/visualize/",
                               unique(viz.data$viewID),
                               "_session_bar.png"),
                  type = "session_bar",
                  stringsAsFactors = FALSE)
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
