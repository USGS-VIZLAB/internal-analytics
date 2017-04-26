visualize.portfolio_sessions_all <- function(viz=as.viz("portfolio_sessions_all")){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- deps[["sessions_and_new_users"]]
  
  ga_table <- deps[["project_table"]] 
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
      left_join(select(ga_table, viewID, shortName), by="viewID") %>%
      mutate(type = paste(j,"\n",paste0(range(range_days), collapse = " to "))) %>%
      select(-viewID)
    
    level_text <- c(level_text, paste(j,"\n",paste0(range(range_days), collapse = " to ")))
    
    summary_data <- bind_rows(summary_data, summary_sessions)
    
  }
  
  summary_data <- summary_data %>% 
    filter(!is.na(shortName)) %>% 
    arrange(desc(sessions)) %>%
    mutate(session_text = sapply(sessions, function(x) pretty_num(x)),
           type = factor(type, levels = level_text))
  
  break.by <- "Year"
  
  break_data <- filter(summary_data, grepl(break.by, type)) %>% 
    mutate(bin = cut(sessions, 
                     breaks = c(-Inf, viz[['breaks']], Inf),
                     labels = c("low traffic", "moderate traffic", "high traffic", "very high traffic") )) %>% 
    arrange(desc(sessions))
  
  summary_data_full <- left_join(summary_data, 
                                 select(break_data, bin, shortName), by="shortName")%>%
    mutate(shortName = factor(shortName, levels = rev(break_data$shortName)),
           bin = factor(bin, levels = c("very high traffic","high traffic","moderate traffic","low traffic")),
           scaler = 1)
  
  scale_to <- max(summary_data_full$sessions, na.rm = TRUE)
  
  for(i in levels(summary_data_full$bin)){
    sub <- filter(summary_data_full, bin == i)
    max_sess <- max(sub$sessions, na.rm = TRUE)
    summary_data_full$scaler[summary_data_full$bin == i] <- (scale_to / max_sess)
  }
  
  min_app <- select(summary_data_full, bin, type, sessions, shortName) %>%
    filter(type == levels(summary_data_full$type)[1]) %>%
    group_by(bin) %>%
    slice(which.min(sessions))
  
  summary_data_full$scaled_value <- summary_data_full$sessions*summary_data_full$scaler
  summary_data_full$scaled_newUser <- summary_data_full$newUsers*summary_data_full$scaler

  mean_sessions <- summary_data_full %>%
    filter(type == levels(summary_data_full$type)[1]) 
  
  sessions_85 <- as.numeric(quantile(mean_sessions$scaled_value, probs = 0.85))
  
  mean_sessions <- summary_data_full %>%
    filter(type == levels(summary_data_full$type)[3]) 
  
  sessions_85_week <- as.numeric(quantile(mean_sessions$scaled_value, probs = 0.85))
  sessions_50_week <- as.numeric(quantile(mean_sessions$scaled_value, probs = 0.6))
  sessions_75_week <- as.numeric(quantile(mean_sessions$scaled_value, probs = 0.7))
  sessions_100 <- max(mean_sessions$scaled_value, na.rm = TRUE)
  
  text_df <- data.frame(label = c("very high traffic","high traffic","moderate traffic","low traffic"),
                        type = factor(levels(summary_data_full$type)[1], levels = levels(summary_data_full$type)),
                        bin = factor(levels(summary_data_full$bin), levels = levels(summary_data_full$bin)),
                        shortName = min_app$shortName,
                        y = sessions_85,
                        stringsAsFactors = FALSE)
  
  fake_legend <- data.frame(label = c("Total","New User"),
                        type = factor(levels(summary_data_full$type)[3], levels = levels(summary_data_full$type)),
                        bin = factor(levels(summary_data_full$bin)[4], levels = levels(summary_data_full$bin)),
                        shortName = rev(levels(summary_data_full$shortName)[1:2]),
                        y = sessions_85_week,
                        y_50 = sessions_50_week,
                        y_75 = sessions_75_week,
                        y_max = 1.35*sessions_100,
                        stringsAsFactors = FALSE)
  
  port_graph <- ggplot(data = summary_data_full, aes(x = shortName, y = scaled_value)) +
    geom_rect(aes(fill = bin),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.1) +
    geom_point() +
    geom_segment(aes(xend = shortName), yend=0) +
    geom_segment(aes(xend = shortName, y = scaled_newUser), yend=0, col="grey", size=2) + 
    geom_text(aes(label = session_text), size = 3, hjust = -0.25) + 
    geom_rect(data = fake_legend[1,], aes(y = 0),
              ymin = fake_legend$y_50[1]*0.95, 
              ymax = fake_legend$y_max[1], 
              xmin = .4,
              xmax = 2.6,
              color = "black", fill = "white") +
    geom_text(data = fake_legend, aes(x = shortName, y = y, label = label), size = 3) +
    geom_segment(data = fake_legend[2,],
                 aes(x = shortName, 
                     xend = shortName, 
                     y = y_50, yend=y_75), col="grey", size=2) + 
    geom_segment(data = fake_legend[1,], aes(xend = shortName, y=y_50, yend=y_75)) +
    geom_point(data = fake_legend[1,], aes(x = shortName, y=y_75)) +
    
    geom_text(data = text_df, aes(x = shortName, y = y, label = label), size = 3.5) +
    facet_grid(bin ~ type, scales = "free",
               space = "free_y", drop = TRUE) +
    coord_flip() +
    scale_fill_manual(values = rev(brewer.pal(4,"Blues"))) +
    scale_y_continuous(expand = c(.35,0) ) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x =  element_blank(),
          strip.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none"
          ) 

  ggsave(port_graph, file = viz[["location"]], height = height, width = width)
  
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


# visualize.portfolio_sessions_sparky <- function(viz=as.viz("portfolio_sessions_all")){
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   library(RColorBrewer)
#   
#   deps <- readDepends(viz)
#   
#   height = viz[["height"]]
#   width = viz[["width"]]
#   
#   viz.data <- deps[["sessions_and_new_users"]]
#   
#   ga_table <- deps[["project_table"]] 
#   ga_table$viewID <- as.character(ga_table$viewID)
#   
#   range_text <- c("-1 year")
#   names(range_text) <- c("Year")
#   
#   latest_day = max(viz.data$date, na.rm = TRUE)
#   
#   summary_data <- data.frame()
#   summary_sparky <- data.frame()
#   
#   level_text <- c()
#   
#   for(i in seq_len(length(range_text))){
#     range_days = seq(latest_day, length = 2, by = range_text[i])
#     
#     j <- names(range_text)[i]
#     level_text <- c(level_text, j)
#     
#     summary_sessions <- viz.data %>%
#       filter(date >= range_days[2]) %>%
#       group_by(viewID) %>%
#       summarize(sessions = sum(sessions, na.rm = TRUE), 
#                 newUsers = sum(newUsers, na.rm = TRUE)) %>%
#       arrange(sessions) %>%
#       left_join(select(ga_table, viewID, shortName), by="viewID") %>%
#       mutate(type = j) %>%
#       select(-viewID)
#     
#     summary_sparky_sub <- viz.data %>%
#       filter(date >= range_days[2]) %>%
#       group_by(viewID, date) %>%
#       summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
#       mutate(type = "Sparky") %>%
#       data.frame() %>%
#       left_join(select(ga_table, viewID, shortName), by="viewID") %>%
#       select(-viewID)
#     
#     summary_data <- bind_rows(summary_data, summary_sessions)
#     summary_sparky <- bind_rows(summary_sparky, summary_sparky_sub)
#     
#   }
#   
#   summary_data <- summary_data %>% 
#     filter(!is.na(shortName)) %>% 
#     arrange(desc(sessions)) %>%
#     mutate(type = factor(paste(type,"\n",paste0(range(range_days), collapse = " to ")),
#                          levels = c(paste(names(range_text),"\n",paste0(range(range_days), collapse = " to ")),"Sparky")),
#            session_text = sapply(sessions, function(x) pretty_num(x))
#     )
#   
#   break.by <- "Year"
#   
#   break_data <- filter(summary_data, grepl(break.by, type)) %>% 
#     mutate(bin = cut(sessions, 
#                      breaks = c(-Inf, viz[['breaks']], Inf),
#                      labels = c("low traffic", "moderate traffic", "high traffic", "very high traffic") )) %>% 
#     arrange(desc(sessions))
#   
#   summary_data_full <- left_join(summary_data, 
#                                  select(break_data, bin, shortName), by="shortName")%>%
#     mutate(shortName = factor(shortName, levels = rev(break_data$shortName)),
#            bin = factor(bin, levels = c("very high traffic","high traffic","moderate traffic","low traffic")),
#            scaler = 1)
#   
#   summary_sparky <- summary_sparky %>%
#     mutate(type = factor(type, levels = levels(break_data$type))) %>%
#     left_join(select(break_data, bin, shortName), by="shortName") %>%
#     mutate(shortName = factor(shortName, levels = levels(summary_data_full$shortName)),
#            bin = factor(bin, levels = c("very high traffic","high traffic","moderate traffic","low traffic")))
#   
#   
#   scale_to <- max(summary_data_full$sessions, na.rm = TRUE)
#   
#   for(i in levels(summary_data_full$bin)){
#     sub <- filter(summary_data_full, bin == i)
#     max_sess <- max(sub$sessions, na.rm = TRUE)
#     summary_data_full$scaler[summary_data_full$bin == i] <- (scale_to / max_sess)
#   }
#   
#   min_app <- select(summary_data_full, bin, type, sessions, shortName) %>%
#     filter(type == levels(summary_data_full$type)[1]) %>%
#     group_by(bin) %>%
#     slice(which.min(sessions))
#   
#   summary_data_full$scaled_value <- summary_data_full$sessions*summary_data_full$scaler
#   summary_data_full$scaled_newUser <- summary_data_full$newUsers*summary_data_full$scaler
#   
#   
#   mean_sessions <- summary_data_full %>%
#     filter(type == levels(summary_data_full$type)[1]) 
#   mean_sessions <- as.numeric(quantile(mean_sessions$scaled_value, probs = 0.85))
#   
#   text_df <- data.frame(label = c("very high traffic","high traffic","moderate traffic","low traffic"),
#                         type = factor(levels(summary_data_full$type)[1], levels = levels(summary_data_full$type)),
#                         bin = factor(levels(summary_data_full$bin), levels = levels(summary_data_full$bin)),
#                         shortNames = min_app$shortName,
#                         y = mean_sessions,
#                         stringsAsFactors = FALSE)
#   
#   ggplot(data = summary_data_full, aes(x = shortName, y = scaled_value)) +
#     geom_rect(aes(fill = bin),xmin = -Inf,xmax = Inf,
#               ymin = -Inf,ymax = Inf,alpha = 0.1) +
#     geom_segment(aes(xend = shortName), yend=0) +
#     geom_segment(aes(xend = shortName, y = scaled_newUser), yend=0, col="grey", size=2) + 
#     geom_point() +
#     geom_text(aes(label = session_text), size = 3, hjust = -0.25) + 
#     geom_text(data = text_df, aes(x = shortNames, y = y, label = label), size = 3.5) +
#     facet_grid(bin ~ ., scales = "free",
#                space = "free_y", drop = TRUE) +
#     coord_flip() +
#     scale_fill_manual(values = rev(brewer.pal(4,"Blues"))) +
#     scale_y_continuous(expand = c(.35,0) ) +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           axis.text.x =  element_blank(),
#           strip.text.y = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.ticks=element_blank(),
#           legend.position="none") 
#  
#   chunk_data <- data.frame()
#   
#   for(i in levels(summary_sparky$shortName) ){
#     sub <- filter(summary_sparky, shortName == i)
#     desampled <- data.frame(approx(sub$date,
#                       sub$sessions, 
#                       n = 48))
#     desampled$shortName <- factor(i, levels = levels(summary_data_full$shortName))
#     desampled$type <- factor("Sparky", levels = levels(summary_data_full$type))
#     desampled$bin <- sub$bin[1]
#     chunk_data <- bind_rows(chunk_data, desampled)
#   }
# 
#   ggplot(data = chunk_data, aes(x = x, y = y)) +
#     geom_line(aes(group = shortName) ) +
#     facet_grid(bin ~ ., scales = "fixed",
#                space = "free_y", drop = TRUE) +
#     theme_bw() 
#   
#   
#   +
#     theme(axis.title = element_blank(),
#           axis.text =  element_blank(),
#           strip.text.y = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.ticks=element_blank(),
#           legend.position="none",
#           panel.border = element_blank()) 
#   
#   ggsave(port_graph, file = viz[["location"]], height = height, width = width)
#   
# }

