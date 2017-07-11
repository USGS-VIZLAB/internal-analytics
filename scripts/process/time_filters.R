process.time_filter <- function(viz = as.viz("month_filter")){
  library(dplyr)

  viz.data <- readDepends(viz)[["year_filter"]]

  range_text <- viz[["range_text"]]

  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)

  month_data <- viz.data %>%
    filter(date >= range_days[2])

  saveRDS(month_data, file=viz[["location"]], compress = FALSE)

}



process.fill_missing <- function(viz = "fill_missing"){

}
