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
      summarize(sessions = sum(sessions, na.rm = TRUE), newUsers = sum(newUsers, na.rm = TRUE)) %>%
      arrange(sessions) %>%
      left_join(select(ga_table, viewID, shortName), by="viewID") %>%
      mutate(type = j) %>%
      select(-viewID)

    summary_data <- bind_rows(summary_data, summary_sessions)

  }
  
  
  summary_data <- summary_data[!is.na(summary_data$shortName),]
  
  shortName_ordered  <- filter(summary_data, type == level_text[1]) %>%
    arrange(sessions)
  
  shortName_ordered <- unique(shortName_ordered$shortName)
  
  summary_data$type <- factor(summary_data$type, levels = level_text)
  summary_data$shortName <- factor(summary_data$shortName, levels = shortName_ordered)
  
  year_data <- filter(summary_data, grepl("Year", type)) %>% 
    mutate(bin = cut(sessions, breaks = c(-Inf, viz[['breaks']], Inf))) %>% arrange(desc(sessions))
  
  lm <- 1.7
  tm <- 0.15
  v.spc <- 0.05
  h.spc <- 0.035
  bm <- 0.05
  rect.buffer <- 0.13 # buffer between top (and bottom) of each category and the rectangle border
  cat.total <- height - tm - bm - v.spc*length(viz[['breaks']]) - rect.buffer*2*(length(viz[['breaks']]) + 1) 
    # total space for app info
  app.h <- cat.total / length(unique(year_data$shortName)) # what happens when a category bin is empty?
  
  png(filename = viz[["location"]], 
      height = height, width = width, units = 'in', res = 150)
  par(mar = c(0, 0, 0, 0), omi = c(0, lm, 0, 0), xpd = NA)
  plot(0, NA, ylim = c(0, height), xlim = c(0, 2), axes = FALSE, xlab="", ylab="", xaxs = 'i', yaxs = 'i') # 10% wider for text
  # xlim 0 to number of time facets
  y.0 <- height - tm
  bins <- unique(as.character(year_data$bin))
  cols <- sprintf('grey%1.0f', seq(from = 80, to = 95, length.out = length(bins)))
  bump <- seq(from = 1.2, to = 1.3, length.out = length(bins)) # makes the maximums of the smaller categories a little smaller
  for (cat.bin in bins) { # are already ranked w/ bins
    
    cat.data <- filter(year_data, bin == cat.bin)
    week.data <- filter(summary_data, shortName %in% cat.data$shortName, grepl("Week", type))
    cat.max <- cat.data %>% .$sessions %>% max() %>% "*"(bump[1])
    week.max <- week.data %>% .$sessions %>% max() %>% "*"(bump[1])
    rect(0, y.0-(nrow(cat.data) - 1)*app.h - 2 * rect.buffer, xright = 1.0-h.spc, ytop = y.0, col = cols[1], border = NA)
    rect(1, y.0-(nrow(cat.data) - 1)*app.h - 2 * rect.buffer, xright = 2-h.spc, ytop = y.0, col = cols[1], border = NA)
    y.0 <- y.0 - rect.buffer
    for (j in seq_len(nrow(cat.data))){
      
      segments(0,y.0, cat.data$sessions[j]/cat.max, lend = 1)
      segments(0,y.0, cat.data$newUsers[j]/cat.max, lwd = 3, lend = 1)
      week.x <- filter(week.data, shortName == cat.data$shortName[j])
      segments(1, y.0, 1 + week.x$sessions[1]/week.max, lend = 1)
      segments(1,y.0, 1 + week.x$newUsers[1]/week.max, lwd = 3, lend = 1)
      points(x = cat.data$sessions[j]/cat.max, y = y.0, pch = 20, col='black')
      points(x = 1 + week.x$sessions[1]/week.max, y = y.0, pch = 20, col='black')
      text(cat.data$sessions[j]/cat.max, y = y.0, labels = pretty_num(cat.data$sessions[j]), pos = 4)
      text(1 + week.x$sessions[1]/week.max, y = y.0, labels = pretty_num(week.x$sessions[1]), pos = 4)
      text(0, y = y.0, labels = cat.data$shortName[j], pos=2)
      
      y.0 <- y.0 - app.h
      
    }
    y.0 <- y.0 - (app.h - rect.buffer)
    cols <- tail(cols, -1L)
    bump <- tail(bump, -1L)
  }
  
  dev.off()
}

fancyNumbers <- function(n){
  nNoNA <- n[!is.na(n)]
  x <-gsub(pattern = "1e",replacement = "10^",x = format(nNoNA, scientific = TRUE))
  exponents <- as.numeric(sapply(strsplit(x, "\\^"), function(j) j[2]))
  base <- ifelse(exponents == 0, "1", ifelse(exponents == 1, "10","10^"))
  exponents[base == "1" | base == "10"] <- ""
  textNums <- rep(NA, length(n))  
  textNums[!is.na(n)] <- paste0(base,exponents)
  
  textReturn <- parse(text=textNums)
  return(textReturn)
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
