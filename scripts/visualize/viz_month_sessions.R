visualize.viz_month_sessions <- function(viz = as.viz("viz_month_sessions")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    sessions <- sum(sub_data$sessions, na.rm = TRUE)
    newUsers <- sum(sub_data$newUsers, na.rm = TRUE)
    
    x <- matrix(c(sessions, newUsers))
    row.names(x) <- c("Sessions","New Users")
    

    png(paste0("cache/visualize/",i,"_session_bar.png"))

    barplot(x, main="Previous Month", horiz=TRUE, 
              legend=rownames(x), axes=FALSE)
    axis(1, lwd = 2, at = pretty(c(0, sum(x))))
    dev.off()
    
  }
  
  write.csv(paste0("cache/visualize/",
                   unique(viz.data$viewID),
                   "_session_bar.png"), 
            file=viz[["location"]], row.names = FALSE)
  
}
