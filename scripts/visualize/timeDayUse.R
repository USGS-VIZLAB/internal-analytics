#portfolio-wide
library(dplyr)
visualize.timeDayUse_all <- function(viz) {
  viz.data <- readDepends(viz)[["sessions_and_new_users"]] #not sure which
  hourSum <- group_by(viz.data, hour) %>% summarise(n = n()) #need to set to numeric?
  png("cache/visualize/timeDayUse_all.png")
  plot(hourSum$hour, hourSum$n, type = "l", xlab = "Hour of Day", 
       ylab = "Sessions", main = "Portfolio-wide sessions, sum total per hour for all apps", 
       xaxt = "n", lwd = 2, col = "blue")
  axis(side = 1, at = 0:23, labels = FALSE)
  axis(side = 1, at = seq(0, 24, by = 4), tck = -0.04)
  dev.off()
}

visualize.timeDayUse_app <- function(viz) {
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  for(i in unique(viz.data$viewID)) {
    
    hourSum <- filter(viz.data, viewID == i) %>% group_by(hour) %>% summarise(n = n()) #need to set to numeric?
    png(paste0("cache/visualize/timeDayUse_", i, ".png"))
    plot(hourSum$hour, hourSum$n, type = "l", xlab = "Hour of Day", 
         ylab = "Sessions", main = "Applications sessions, sum total per hour", 
         xaxt = "n", lwd = 2, col = "blue")
    axis(side = 1, at = 0:23, labels = FALSE)
    axis(side = 1, at = seq(0, 24, by = 4), tck = -0.04)
    dev.off()
  }
}
