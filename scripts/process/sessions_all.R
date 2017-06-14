process.sessions_all <- function(viz=as.viz("sessions_all")){
  library(dplyr)
  library(tidyr)
  
  deps <- readDepends(viz)

  viz.data <- deps[["viz_data"]]
  
  viz.data <- select(viz.data, date,viewID,sessions,newUsers)
  
  ga_table <- deps[['project_table']]
  ga_table$viewID <- as.character(ga_table$viewID)
  
  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("Year","Month","Week")
  
  latest_day = max(viz.data$date, na.rm = TRUE)
  
  summary_data <- data.frame()
  level_text <- c()
  
  for(i in seq_len(length(range_text))){
    range_days = seq(latest_day, length = 2, by = range_text[i])
    
    j <- names(range_text)[i]
    
    summary_sessions <- viz.data %>%
      filter(date >= range_days[2]) %>%
      group_by(viewID) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE), 
                newUsers = sum(newUsers, na.rm = TRUE)) %>%
      arrange(sessions) %>%
      left_join(select(ga_table, viewID, longName, shortName), by="viewID") %>%
      mutate(type = paste(j,"\n",paste0(range(range_days), collapse = " to "))) %>%
      select(-viewID)
    
    level_text <- c(level_text, paste(j,"\n",paste0(range(range_days), collapse = " to ")))
    
    summary_data <- bind_rows(summary_data, summary_sessions)
    
  }
  
  max_char <- viz[["max_char"]]
  summary_data$longName[nchar(summary_data$longName) > max_char] <- summary_data$shortName[nchar(summary_data$longName) > max_char]
  
  summary_data <- summary_data %>% 
    arrange(desc(sessions)) %>%
    mutate(session_text = sapply(sessions, function(x) pretty_num(x)),
           type = factor(type, levels = level_text))
  
  break.by <- "Year"
  
  break_data <- filter(summary_data, grepl(break.by, type)) %>% 
    mutate(bin = cut(sessions, 
                     breaks = c(-Inf, viz[['breaks']], Inf),
                     labels = c("Low", "Moderate", "High", "Very High") )) %>% 
    arrange(desc(sessions))
  

  summary_data_full <- left_join(summary_data, 
                                 select(break_data, bin, longName), by="longName")%>%
    mutate(longName = factor(longName, levels = rev(break_data$longName)),
           bin = factor(bin, levels = c("Very High", "High", "Moderate", "Low")),
           scaler = 1)
  
  scale_to <- max(summary_data_full$sessions, na.rm = TRUE)
  
  scaler_df <- summary_data_full %>%
    group_by(bin) %>%
    summarize(max_bin = max(sessions, na.rm = TRUE)) %>%
    data.frame()
  
  summary_data_full <- summary_data_full %>%
    left_join(scaler_df, by = "bin") %>%
    mutate(scaler = scale_to / max_bin,
           scaled_value = sessions * scaler, 
           scaled_newUser = newUsers * scaler) %>%
    select(-max_bin, -scaler)
  
  saveRDS(summary_data_full, file = viz[["location"]])
}

pretty_num <- function(n){
  if (is.na(n)) n = 0
  if (n > 1e7){
    out <- sprintf('%1.0fM', n/1e6)
  } else if (n > 1e6){
    out <- sprintf('%1.1fM', n/1e6)
  } else if (n > 1e4){
    out <- sprintf('%1.0fK', n/1e3)
  } else if (n > 1000){
    out <- sprintf('%1.1fK', n/1e3)
  } else {
    out <- as.character(n)
  }
  return(out)
}
