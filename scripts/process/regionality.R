
process.state_app_traffic <- function(viz=as.viz("state_app_traffic")){
  deps <- readDepends(viz)
  traffic_all <- deps[["viz_data"]]
  ga_table <- deps[["project_table"]]
  sessions_all <- deps[['sessions_all']]

  # get population and percentage of total US population per state
  # 2015 population data
  us_pop <- rename(usmap::statepop, pop = pop_2015, region = full) %>%
    mutate(region = tolower(region))
  us_total <- sum(us_pop$pop)
  us_pop <- mutate(us_pop, pop_pct = pop/us_total*100)

  # get traffic data based on users by state
  ga_table <- select(ga_table, viewID, app_name = shortName)
  traffic_all <- traffic_all %>%
    filter(region %in% state.name | region == "District of Columbia") %>% #drop traffic from outside US
    left_join(ga_table) %>%
    arrange(app_name) %>%
    mutate(region = tolower(region))

  # traffic summarized by region and app
  traffic_by_region_and_app <- group_by(traffic_all, app_name, viewID, region) %>%
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
  traffic_app_totals <- group_by(traffic_us, app_name, viewID) %>%
    summarize(traffic_total = sum(traffic))
  traffic <- left_join(traffic_us, traffic_app_totals)

  # calculate percentage of traffic for each state
  traffic <- mutate(traffic, traffic_pct = traffic/traffic_total*100)
  saveRDS(traffic, file = viz[["location"]])
}

process.regionality_metric <- function(viz=as.viz("regionality_metric")){
  deps <- readDepends(viz)
  sessions_all <- deps[['sessions_all']]
  ga_table <- deps[["project_table"]]
  traffic <- deps[['state_app_traffic']]

  metric_type <- viz[['metric_type']]

  ga_table <- select(ga_table, viewID, app_name = shortName)

  # determine which state deviates the most from what is expected for each app
  most_deviated_states <- mutate(traffic, deviation = traffic_pct - pop_pct) %>%
    group_by(app_name) %>%
    slice(c(which.max(deviation),
            ifelse(all(is.na(deviation)), # include one instance for any app with all missing data
                   head(which(is.na(deviation)), 1),
                   integer(0)))) %>%
    mutate(most_deviated = ifelse(is.na(deviation),
                           paste0(" (", deviation, ")"),
                           paste0(region, " (", signif(deviation, 2), ")"))) %>%
    select(app_name, most_deviated) %>%
    arrange(app_name)

  # now calculate the regionality index for each app...

  # get r^2
  metric_results <- sapply(unique(traffic$app_name), function(nm, traffic, metric_type) {
    app_traffic <- filter(traffic, app_name == nm)

    if(metric_type == "r2"){
      ## r^2
      sqr_resid <- (app_traffic$traffic_pct - app_traffic$pop_pct)^2
      sqr_total <- (app_traffic$traffic_pct - mean(app_traffic$traffic_pct))^2
      raw_result <- 1 - sum(sqr_resid)/sum(sqr_total)
      result_scaled <- raw_result*10

      # truncate negative results to zeros
      result <- ifelse(result_scaled < 0, 0, result_scaled)
    }

    if(metric_type == "slope"){
      ## slopes
      regionality_lm <- lm(traffic_pct ~ pop_pct, data=app_traffic)
      raw_result <- coef(regionality_lm)[2]
      names(raw_result) <- NULL
      result_scaled <- raw_result*1
      result <- result_scaled
    }

    if(metric_type == "percapita"){
      ## per capita (would need to log these)
      result <- var(app_traffic$traffic/app_traffic$pop)
      result <- sd(app_traffic$traffic/app_traffic$pop)
    }

    if(metric_type == "normpercapita"){
      ## mean normalized per capita
      per_capita <- app_traffic$traffic_pct/app_traffic$pop_pct
      normalized_percapita <- per_capita/mean(per_capita)
      # result <- var(normalized_percapita) # weighted oppositely...NWISWeb was not national
      result <- log(sd(normalized_percapita))
    }

    if(metric_type == "shannon"){
      ## Species diversity (Shannon index)
      # p_i = proportion of individuals belonging to the ith species (pop_pct)
      pop_pct <- app_traffic$pop_pct
      shannon_index_pop <- -sum(pop_pct*log(pop_pct))
      result <- shannon_index_pop
    }

    if(metric_type == "gini"){
      ## Gini coefficient
      per_capita <- app_traffic$traffic_pct/app_traffic$pop_pct
      result <- ineq::Gini(per_capita) # normalized per capita made no difference
    }

    return(result)
  }, traffic, metric_type)

  # rescale so that index remains consistent through time (low == regional, high == national)
   new_scale <- c(1,10)
  if(metric_type %in% c("gini")){
    # gini has theoretical scale of 0-1, so don't need to scale for consistency through time
    # need to flip axis though
    scaled_metric <- 1-metric_results
  } else if(metric_type %in% c("normpercapita")){
    new_scale <- rev(new_scale)
    scaled_metric <- (((metric_results - min(metric_results)) * diff(new_scale)) /
                        diff(range(metric_results))) + new_scale[1]
  } else {
    scaled_metric <- (((metric_results - min(metric_results)) * diff(new_scale)) /
                        diff(range(metric_results))) + new_scale[1]
  }

  # format metric results into a nice data frame
  regionality_data <- data.frame(app_name = names(metric_results),
                                 regional_index = scaled_metric,
                                 stringsAsFactors = FALSE)
  regionality_data <- left_join(regionality_data, ga_table) %>%
    left_join(., most_deviated_states) %>%
    select(viewID, app_name, regional_index, most_deviated)
  rownames(regionality_data) <- NULL

  # combine regionality w/ sessions data
  needed_info <- select(sessions_all, viewID, longName, bin)
  regionality_data <- left_join(regionality_data, needed_info, by = "viewID")

  regionality_data <- regionality_data %>%
    mutate(trend = as.character(NA),
           trend_complete = as.character(NA),
           type = grep(pattern = "Year", x = unique(sessions_all$type), value = TRUE) %>%
             sub(pattern = "Year", replacement = "Geographic Reach*"), #use dates in year category
           trend = "geo", #just a placeholder since this field is used for color
           scaled_newUser = as.numeric(NA),
           session_text = as.character(round(regional_index, 2))) %>%
    rename(scaled_value = regional_index) %>%
    select(viewID, longName, scaled_value, scaled_newUser, bin, trend, trend_complete, type, session_text)

  sessions_all <- sessions_all %>%
    mutate(trend_complete = paste(trend, complete))

  result_df <- bind_rows(sessions_all, regionality_data)

  saveRDS(result_df, file = viz[["location"]])
}
