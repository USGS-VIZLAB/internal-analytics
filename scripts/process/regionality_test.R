library(choroplethr)
library(dplyr)
library(yaml)
library(rvest)

# get population and percentage of total US population per state
# 2012 population data

data(df_pop_state)
us_pop <- df_pop_state
us_pop <- rename(us_pop, pop = value)
us_total <- sum(us_pop$pop)
us_pop <- mutate(us_pop, pop_pct = pop/us_total*100)

# get traffic data based on users by state
# this is data from one app
traffic_all <- readRDS('cache/process/lastest_year.rds')
traffic_all <- mutate(traffic_all, region = tolower(region))

# throw in app names
gaList <- yaml.load_file('data/gaTable.yaml')
gaTable <- data.frame(viewID = unlist(pluck(gaList, 'viewID')),
                      app_name = unlist(pluck(gaList, 'shortName')),
                      stringsAsFactors = FALSE)
traffic_all <- left_join(traffic_all, gaTable) %>% arrange(app_name)

# traffic summarized by region and app
traffic_by_region_and_app <- group_by(traffic_all, app_name, region) %>%
  summarize(traffic = sum(users)) %>%
  ungroup()

# merge app traffic w/ population
all_apps <- unique(traffic_all$app_name)
all_states <- unique(us_pop$region)
all_states_apps <- expand.grid(app_name = all_apps, region = all_states, stringsAsFactors = FALSE)
pad_us_pop <- left_join(all_states_apps, us_pop)
traffic_us <- left_join(pad_us_pop, traffic_by_region_and_app) %>%
  mutate(traffic = ifelse(is.na(traffic), 0, traffic)) # NAs --> 0

# add total app traffic to traffic by state
traffic_app_totals <- group_by(traffic_us, app_name) %>%
  summarize(traffic_total = sum(traffic))
traffic <- left_join(traffic_us, traffic_app_totals)

# calculate percentage of traffic for each state
traffic <- mutate(traffic, traffic_pct = traffic/traffic_total*100)

# determine which state deviates the most from what is expected for each app
most_deviated_states <- mutate(traffic, deviation = traffic_pct - pop_pct) %>%
  group_by(app_name) %>%
  slice(c(which.max(deviation),
          ifelse(all(is.na(deviation)), # include one instance for any app with all missing data
                 head(which(is.na(deviation)), 1),
                 integer(0)))) %>%
  mutate(string = ifelse(is.na(deviation),
                         paste0(" (", deviation, ")"),
                         paste0(region, " (", signif(deviation, 2), ")"))) %>%
  select(app_name, string) %>%
  arrange(app_name)

# now calculate the regionality index for each app...

# get r^2
metric_results <- sapply(unique(traffic$app_name), function(nm, traffic) {
  app_traffic <- filter(traffic, app_name == nm)
  sqr_resid <- (app_traffic$traffic_pct - app_traffic$pop_pct)^2
  sqr_total <- (app_traffic$traffic_pct - mean(app_traffic$traffic_pct))^2
  raw_result <- 1 - sum(sqr_resid)/sum(sqr_total)
  # truncate negative results to zeros
  result <- ifelse(raw_result < 0, 0, raw_result)
  return(result)
}, traffic)

# re-order metrics based on app total traffic
traffic_app_totals_ordered <- arrange(traffic_app_totals, traffic_total)
metric_results <- metric_results[match(traffic_app_totals_ordered$app_name, names(metric_results))]

# plot the results
par(mar = c(4, 12, 1, 3))
ymidpt <- barplot(metric_results, xlab = "", cex.names=0.8, horiz=TRUE, las=1, axes=FALSE, xlim=c(0,1))
axis(side = 1, at = seq(0,1,0.2))
box()
mtext("<-- regional", side=1, line=2, font=4, adj=0)
mtext("national -->", side=1, line=2, font=4, adj=1)
mtext("total traffic -->", side=2, line=11, font=4, padj=1)
# text that is close to the right edge should be positioned to the left of the coordinate
close2edge <- metric_results > par('usr')[2] - 0.20*diff(par('usr')[1:2])
text(x = metric_results[close2edge], y = ymidpt[close2edge], cex=0.7, pos=2,
     labels = most_deviated_states$string[close2edge])
text(x = metric_results[!close2edge], y = ymidpt[!close2edge], cex=0.7, pos=4,
     labels = most_deviated_states$string[!close2edge])
