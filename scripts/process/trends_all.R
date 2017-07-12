process.trends_all <- function(viz=as.viz("trends_all")){
  library(dplyr)
  library(tidyr)
  library(rkt)
  library(lubridate)

  # load data
  deps <- readDepends(viz)

  sessions_all <- deps[["sessions_all"]]

  year_data <- deps[["year_data"]]

  counts_df_aug <- year_data %>%
    group_by(viewID) %>%
    mutate(
      sessions_mean = mean(sessions),
      sessions_norm = sessions / mean(sessions),
      dec_date = lubridate::decimal_date(date),
      day_of_week = lubridate::wday(date)
    ) %>%
    ungroup()

  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("Year","Month","Week")
  latest_day <- max(counts_df_aug$date, na.rm = TRUE)

  # create 1-year, 1-month, and 1-week subsets
  counts <- list()
  levels_text <- list()

  for(i in seq_len(length(range_text))){
    range_days <- seq(latest_day, length = 2, by = range_text[i])
    j <- names(range_text)[i]
    levels_text[[j]] <- paste(j,"\n",paste0(range(range_days), collapse = " to "))
    counts[[j]] <- counts_df_aug %>%
      filter(date >= range_days[2]) %>%
      mutate(type = levels_text[[i]])
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

  trends$scale[trends$scale == "Year"] <- levels_text$Year
  trends$scale[trends$scale == "Month"] <- levels_text$Month
  trends$scale[trends$scale == "Week"] <- levels_text$Week
  trends$type <- factor(trends$scale, levels = levels(sessions_all$type))

  # augment trends with single columng for shape: is the trend up, down, or
  # non-significant?
  trends_aug <- trends %>%
    mutate(trend = ifelse(pvalue <= 0.05, ifelse(slope > 0, viz[["trend_text"]]$up,
                                                 viz[["trend_text"]]$down),
                          viz[["trend_text"]]$none))

  sessions_total <- left_join(sessions_all, trends_aug,
                              by=c("viewID","type"))

  saveRDS(sessions_total, file = viz[["location"]])
}
