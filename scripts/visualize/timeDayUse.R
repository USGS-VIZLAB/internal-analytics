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
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  height = viz[["height"]]
  width = viz[["width"]]
  plot_type <- viz[["plottype"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  for(i in unique(viz.data$viewID)) {
    location <- paste0("cache/visualize/timeDayUse_", i, ".png")
    hourSum <- filter(viz.data, viewID == i) %>% group_by(hour) %>% summarise(n = n()) #need to set to numeric?
    png(location, height = height, width = width)
    plot(hourSum$hour, hourSum$n, type = "l", xlab = "Hour of Day", 
         ylab = "Sessions", main = "Applications sessions, sum total per hour", 
         xaxt = "n", lwd = 2, col = "blue")
    axis(side = 1, at = 0:23, labels = FALSE)
    axis(side = 1, at = seq(0, 24, by = 4), tck = -0.04)
    dev.off()
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))    
  }
  write.csv(x, file=viz[["location"]], row.names = FALSE)
}
