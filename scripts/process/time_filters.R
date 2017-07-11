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

  max_date <- Sys.Date() - 1
  range_text <- "-1 year"
  year_days = seq(max_date, length = 2, by = range_text)

  full_dates <- seq(max_date, length = -as.numeric(diff(year_days)), by=-1)

  sub_data <- select(viz.data, date, sessions, newUsers, viewID) %>%
    group_by(date, viewID) %>%
    summarize(sessions = sum(sessions, na.rm = TRUE),
              newUsers = sum(newUsers, na.rm = TRUE)) %>%
    ungroup() %>%
    bind_rows(data_frame(viewID=NA, date=full_dates)) %>%
    unite(old_new, sessions,newUsers) %>%
    spread(date, old_new, fill="0_0") %>%
    gather(date, old_new, -viewID) %>%
    separate(old_new, into = c("sessions", "newUsers")) %>%
    filter(!is.na(viewID)) %>% # remove the dummy rows that were just to ensure every date got included
    mutate(date = as.Date(date),
           sessions = as.integer(sessions),
           newUsers = as.integer(newUsers)) %>% # restore date format that got lost when %>%
    filter(date >= min(full_dates))

  saveRDS(sub_data, file=viz[["location"]], compress = FALSE)

}

process.fill_missing <- function(viz = as.viz("fill_missing_month")){
  library(dplyr)

  viz.data <- readDepends(viz)[["fill_missing_year"]]
  range_text <- viz[["range_text"]]

  range_days = seq(max(viz.data$date, na.rm = TRUE), length = 2, by = range_text)

  month_data <- viz.data %>%
    filter(date >= range_days[2])

  saveRDS(month_data, file=viz[["location"]], compress = FALSE)
}
