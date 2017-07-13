process.trends_all <- function(viz=as.viz("trends_all")){
  library(dplyr)
  library(tidyr)
  library(rkt)
  library(lubridate)

  # load data
  deps <- readDepends(viz)
  sessions_all <- deps[["sessions_all"]]
  counts <- deps[c("year_data","month_data","week_data")]

  # augment year_data with columns useful for trend detection & quantification
  counts_aug <- lapply(counts, function(cdf) {
    cdf %>%
      group_by(viewID) %>%
      mutate(
        sessions_mean = mean(sessions),
        sessions_norm = sessions / mean(sessions),
        dec_date = lubridate::decimal_date(date),
        day_of_week = lubridate::wday(date)
      ) %>%
      ungroup()
  })

  # create vector of interval types with names we can retrieve later
  types <- names(counts) %>%
    sapply(function(type) {
      # match this type to teh sessions_all factor by the first four characters (year/mont/week)
      levels(sessions_all$type)[grep(tolower(substring(type, 1, 4)), tolower(levels(sessions_all$type)))]
    }) %>%
    # add names according to the number of rows in the input dataset so we can
    # retrieve it later
    setNames(as.character(sapply(counts_aug, nrow)))

  # calculate trends at all 3 temporal scales, using different methods for each
  viewIDs <- unique(counts_aug[["year_data"]]$viewID) # if it's in 1-year, it's in 1-month and 1-week

  trends <- bind_rows(lapply(viewIDs, function(vid) {
    # run trend tests for all three scales (types) for this viewID. Use seasonal
    # Kendall (blocking by day of week) if there are enough data and
    # Mann-Kendall otherwise. it's possible, but challenging, for 7 days to
    # produce a signifcant trend with a Mann-Kendall test
    bind_rows(lapply(counts_aug, function(cdf_all) {
      # subset each of the counts data.frames for just this type and viewID
      cdf <- cdf_all %>% filter(viewID==vid)

      # count the number of observations for each day of the week. if there are
      # fewer than 4 blocks (days of week) with >= 4 points, we'll use
      # Mann-Kendall (no blocking) rather than seasonal Kendall

      num_blocks <- length(which(table(cdf$day_of_week) >= 4))

      # run the trend test, rkt, which is SK if block is specificied and MK
      # otherwise
      if(num_blocks >= 4) {
        vtrend <- rkt::rkt(date=cdf$dec_date, y=cdf$sessions_norm, block=cdf$day_of_week)
      } else if (num_blocks == 0){
        vtrend <- data.frame(B = NA, sl = NA)
      } else {
        vtrend <- rkt::rkt(date=cdf$dec_date, y=cdf$sessions_norm)
      }

      # create a 1-row summary of the trend results for this type & viewID
      data_frame(
        type = types[[as.character(nrow(cdf_all))]], # figure out which type this is based on the dims of cdf_all
        slope = vtrend$B, # change per year as a fraction of the annual mean for this viewID
        pvalue = vtrend$sl, # two-sided p-value for significance of trend
        n_possible = cdf$n_possible[1],
        n_actual = cdf$n_actual[1])
    })) %>%
      mutate(viewID = vid)
  }))

  # for consistency with other files, make year/month/week label into factor
  trends$type <- factor(trends$type, levels=levels(sessions_all$type))

  # augment trends with single columng for shape: is the trend up, down, or
  # non-significant?
  trends_aug <- trends %>%
    mutate(trend = ifelse(pvalue <= 0.05, ifelse(slope > 0, viz[["trend_text"]]$up,
                                                 viz[["trend_text"]]$down),
                          viz[["trend_text"]]$none),
           complete = (n_actual / n_possible) >= 0.9) # set threshold for whether a year/month/week counts as complete interval

  trends_aug$trend[is.na(trends_aug$trend)] <- "none"

  sessions_total <- left_join(sessions_all, trends_aug,
                              by=c("viewID","type"))

  saveRDS(sessions_total, file = viz[["location"]])
}
