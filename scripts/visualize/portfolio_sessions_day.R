visualize.portfolio_sessions_day <- function(viz){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users_daily"]]
  
  
  summary_sessions <- viz.data %>%
    group_by(viewID) %>%
    summarize(newUsers = sum(newUsers, na.rm = TRUE),
              sessions = sum(sessions, na.rm = TRUE)) %>%
    mutate(oldUsers = sessions - newUsers) %>%
    arrange(sessions)
  
  dater <- t(as.matrix(summary_sessions[,c("newUsers", "oldUsers")]))
  
  png(viz[["location"]])
  par(las=1)
  barplot(dater, horiz = TRUE,
          names.arg = summary_sessions$viewID)
  dev.off()
  
}
