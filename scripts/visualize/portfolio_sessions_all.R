visualize.portfolio_sessions_all <- function(viz){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- deps[["sessions_and_new_users"]]
  
  viz.data.daily <- deps[["sessions_and_new_users_daily"]]
  
  ga_table <- deps[["project_table"]] 
  ga_table$viewID <- as.character(ga_table$viewID)
  
  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("Year:\n","Month:\n","Week:\n")
  
  latest_day = max(viz.data$date, na.rm = TRUE)
  
  summary_data <- data.frame()
  level_text <- c()
  
  for(i in 1:3){
    range_days = seq(latest_day, length = 2, by = range_text[i])
    
    j <- paste(names(range_text)[i],paste0(range(range_days), collapse = " to "))
    level_text <- c(level_text, j)
    
    summary_sessions <- viz.data %>%
      filter(date >= range_days[2]) %>%
      group_by(viewID) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      arrange(sessions) %>%
      left_join(select(ga_table, viewID, shortName), by="viewID") %>%
      mutate(type = j) %>%
      select(-viewID)

    summary_data <- bind_rows(summary_data, summary_sessions)

  }
  
  range_days = seq(latest_day, length = 2, by = "-1 day")
  j <- paste("Day:\n",paste0(range(range_days), collapse = " to "))
  level_text <- c(level_text, j)
  
  summary_day <- viz.data.daily %>%
    group_by(viewID) %>%
    summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
    arrange(sessions) %>%
    left_join(select(ga_table, viewID, shortName), by="viewID") %>%
    mutate(type = j) %>%
    select(-viewID)
  
  summary_data <- bind_rows(summary_data, summary_day)
  
  summary_data <- summary_data[!is.na(summary_data$shortName),]
  
  shortName_ordered  <- filter(summary_data, type == level_text[1]) %>%
    arrange(sessions)
  
  shortName_ordered <- unique(shortName_ordered$shortName)
  
  summary_data$type <- factor(summary_data$type, levels = level_text)
  summary_data$shortName <- factor(summary_data$shortName, levels = shortName_ordered)
  
  port_graph <- ggplot(data = summary_data, aes(x = shortName, y = sessions)) +
    geom_bar(stat="identity", fill = "steelblue") +
    coord_flip() +
    facet_wrap(~ type, scales = "free_x", nrow = 1) +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    scale_y_log10(labels=fancyNumbers) 
  
  ggsave(port_graph, filename = viz[["location"]], 
         height = height, width = width)

}

fancyNumbers <- function(n){
  nNoNA <- n[!is.na(n)]
  x <-gsub(pattern = "1e",replacement = "10^",x = format(nNoNA, scientific = TRUE))
  exponents <- as.numeric(sapply(strsplit(x, "\\^"), function(j) j[2]))
  # browser()
  base <- ifelse(exponents == 0, "1", ifelse(exponents == 1, "10","10^"))
  exponents[base == "1" | base == "10"] <- ""
  textNums <- rep(NA, length(n))  
  textNums[!is.na(n)] <- paste0(base,exponents)
  
  textReturn <- parse(text=textNums)
  return(textReturn)
}
