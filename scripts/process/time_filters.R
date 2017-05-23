process.year_filter <- function(viz = as.viz("year_filter")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  range_text <- "-1 year"
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  year_data <- viz.data %>%
    filter(date >= range_days[2]) 
  
  saveRDS(year_data, file=viz[["location"]], compress = FALSE)
  
}

process.month_filter <- function(viz = as.viz("month_filter")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  range_text <- "-1 month"
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  month_data <- viz.data %>%
    filter(date >= range_days[2]) 
  
  saveRDS(month_data, file=viz[["location"]], compress = FALSE)
  
}

process.week_filter <- function(viz = as.viz("week_filter")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["aggregate_ga"]]
  
  range_text <- "-1 week"
  
  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)
  
  week_data <- viz.data %>%
    filter(date >= range_days[2]) 
  
  saveRDS(week_data, file=viz[["location"]], compress = FALSE)
  
}