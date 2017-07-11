process.trends_all <- function(viz=as.viz("trends_all")){
  library(dplyr)
  library(tidyr)
  library(rkt)
  library(lubridate)

  # load data
  deps <- readDepends(viz)
  sessions_all <- deps[["sessions_all"]]
  year_filter <- deps[["year_filter"]]
  project_table <- deps[['project_table']]

  # summarize the year's session records into daily session counts for each viewID
  counts_df <- year_filter %>%
    group_by(viewID, date) %>%
    summarize(sessions=sum(sessions, na.rm=TRUE)) %>%
    ungroup()

  # declare the time windows we want to analyze
  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("Year","Month","Week")
  latest_day <- max(counts_df$date, na.rm = TRUE)

  # pad counts with 0's on days when no sessions are reported
  year_range <- seq(latest_day, length = 2, by = range_text["Year"])
  seq_days <- seq(year_range[2], year_range[1], by=as.difftime(1, units="days"))
  counts_df_pad <- counts_df %>%
    bind_rows(data_frame(viewID=NA, date=seq_days)) %>%
    spread(date, sessions, fill=0) %>%
    gather(date, sessions, -viewID) %>%
    filter(!is.na(viewID)) %>% # remove the dummy rows that were just to ensure every date got included
    mutate(date = as.Date(date)) # restore date format that got lost when

  # augment counts with columns that will help us compute trends
  if(any(is.na(counts_df_pad$sessions))) {
    stop("oops, Alison thought we'd never see NAs. better add some na.rms in process.trends_all()")
  }
  counts_df_aug <- counts_df_pad %>%
    group_by(viewID) %>%
    mutate(
      sessions_mean = mean(sessions),
      sessions_norm = sessions / mean(sessions),
      dec_date = lubridate::decimal_date(date),
      day_of_week = lubridate::wday(date)
    ) %>%
    ungroup()

  # create 1-year, 1-month, and 1-week subsets
  counts <- list()
  for(i in seq_len(length(range_text))){
    range_days <- seq(latest_day, length = 2, by = range_text[i])
    j <- names(range_text)[i]
    counts[[j]] <- counts_df_aug %>%
      filter(date >= range_days[2]) %>%
      mutate(type = paste(j,"\n",paste0(range(range_days), collapse = " to ")))
  }

  # calculate trends at all 3 temporal scales, using different methods for each
  trends <- bind_rows(lapply(unique(counts_df_aug$viewID), function(vid) {
    # subset each of the counts data.frames for just this ID
    vcounts <- lapply(counts, function(cdf) { filter(cdf, viewID==vid) })

    # count the number of observations for each day of the week. if there are
    # fewer than 4 blocks (days of week) with >= 4 points, we'll use
    # Mann-Kendall (no blocking) rather than seasonal Kendall (with blocking on
    # day of week, never season)
    num_blocks <- lapply(vcounts, function(cdf) length(which(table(cdf$day_of_week) >= 4)) )

    # run trend tests, using seasonal Kendall if there are enough data and
    # Mann-Kendall otherwise. it's possible, but challenging, for 7 days to
    # produce a signifcant trend with a Mann-Kendall test
    vtrends <- bind_rows(lapply(names(vcounts), function(cscale) {
      cdf <- vcounts[[cscale]]
      if(num_blocks[[cscale]] >= 4) {
        vtrend <- rkt::rkt(date=cdf$dec_date, y=cdf$sessions_norm, block=cdf$day_of_week)
      } else {
        vtrend <- rkt::rkt(date=cdf$dec_date, y=cdf$sessions_norm)
      }
      data_frame(
        scale = cscale,
        slope = vtrend$B, # change per year as a fraction of the annual mean for this viewID
        pvalue = vtrend$sl) # two-sided p-value for significance of trend
    })) %>%
      mutate(viewID = vid)
  }))

  # augment trends with single columng for shape: is the trend up, down, or
  # non-significant?
  trends_aug <- trends %>%
    mutate(trend = ifelse(pvalue <= 0.05, ifelse(slope > 0, "up", "down"), "none"))

  # exploratory plot
  # ggplot(trends, aes(x=viewID, y=slope, shape=slope > 0, color=-abs(slope), alpha=pvalue < 0.05)) +
  #   geom_hline(yintercept=0) + geom_point() +
  #   facet_grid(scale ~ ., scales='free_y') +
  #   theme_classic()

  saveRDS(trends_aug, file = viz[["location"]])
}
