visualize.viz_ymd_sessions <- function(viz = as.viz("viz_ymd_sessions")){
  library(dplyr)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]
  
  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    year_days = seq(Sys.Date(), length = 2, by = "-1 year")
    sub_data_year <- select(sub_data, date, sessions) %>%
      filter(date >= year_days[2]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      mutate(graph = "Year",
             date = as.integer(date) - min(as.integer(date)))
    
    month_days <- seq(Sys.Date(), length = 2, by = "-1 month")
    sub_data_month <- select(sub_data, date, sessions) %>%
      filter(date >= month_days[2]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      mutate(graph = "Month",
             date = seq(0,max(sub_data_year$date),length.out = -as.numeric(diff(month_days))-1))
    
    week_days = seq(Sys.Date(), length = 2, by = "-1 week")
    sub_data_week <- select(sub_data, date, sessions) %>%
      filter(date >= week_days[2]) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      mutate(graph = "Week",
             date = seq(0,max(sub_data_year$date),length.out = -as.numeric(diff(week_days))-1))
    
    sub_data_daily <- select(sub_data, date, sessions) %>%
      filter(date >= seq(Sys.Date()-1, length = 2, by = "-1 day")[2]) %>%
      filter(date <= Sys.Date()-1) %>%
      mutate(graph = "Daily",
             date = as.integer(date))
    
    graph_data <- bind_rows(sub_data_year, 
                            sub_data_month, 
                            sub_data_week, 
                            sub_data_daily)
    graph_data$graph <- factor(graph_data$graph, c("Year","Month","Week","Daily"))
    
    ggplot(data = graph_data) +
      geom_line(aes(x=date, y=sessions)) +
      facet_wrap( ~ graph, nrow = 4)

  }
  

  
}
