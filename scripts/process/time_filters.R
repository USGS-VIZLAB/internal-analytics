process.time_filter <- function(viz = as.viz("month_filter")){
  library(dplyr)

  viz.data <- readDepends(viz)[["year_filter"]]

  range_text <- viz[["range_text"]]

  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)

  month_data <- viz.data %>%
    filter(date >= range_days[2])

  saveRDS(month_data, file=viz[["location"]], compress = FALSE)

}

process.fill_missing_year <- function(viz = as.viz("fill_missing_year")){
  library(dplyr)
  library(tidyr)

  viz.data <- readDepends(viz)[["year_filter"]]

  # get a vector of all dates we'd like to include
  max_date <- Sys.Date() - 1
  range_text <- "-1 year"
  year_bounds <- rev(seq(max_date, length = 2, by = range_text))

  sub_data <- select(viz.data, date, sessions, newUsers, viewID) %>%
    filter(date >= year_bounds[1]) %>%
    # count sessions & newUsers per date
    group_by(date, viewID) %>%
    summarize(sessions = sum(sessions, na.rm = TRUE),
              newUsers = sum(newUsers, na.rm = TRUE)) %>%
    ungroup()

  # pad the data.frame with 0s in sessions and newUsers for dates when no
  # sessions or users were observed (but only pad after the GA starting date
  # for each viewID, which we infer from the first date with any GA record)
  min_dates <- sub_data %>%
    group_by(viewID) %>%
    summarise(min_date = max(year_bounds[1], min(date, na.rm=TRUE)))

  for(i in 1:nrow(min_dates)){

    pad_df <- data_frame(
      date = seq(min_dates$min_date[i], max_date, by=as.difftime(1, units='days')),
      viewID = min_dates$viewID[i])

    sub_data <- sub_data %>%
      full_join(pad_df, by=names(pad_df))
  }

  sub_data <- sub_data %>%
    mutate(n_possible = as.numeric(diff(year_bounds), units='days') + 1,
           n_actual = as.numeric(diff(range(date)), units='days') + 1) %>%
    ungroup() %>%
    replace_na(list(sessions=0, newUsers=0))

  saveRDS(sub_data, file=viz[["location"]], compress = FALSE)

}

process.fill_missing <- function(viz = as.viz("fill_missing_month")){
  library(dplyr)

  viz.data <- readDepends(viz)[["fill_missing_year"]]
  range_text <- viz[["range_text"]]

  range_days <- rev(seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text))

  month_data <- viz.data %>%
    filter(date >= range_days[1]) %>%
    group_by(viewID) %>%
    mutate(n_possible = as.numeric(diff(range_days), units='days') + 1,
           n_actual = as.numeric(diff(range(date)), units='days') + 1) %>%
    ungroup()

  saveRDS(month_data, file=viz[["location"]], compress = FALSE)
}
