visualize.portfolio_sessions_all <- function(viz=as.viz("portfolio_sessions_all")){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  library(grid)
  
  deps <- readDepends(viz)
  
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  text_col = viz[["text_col"]]
  
  viz.data <- deps[["sessions_and_new_users"]]
  
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
      left_join(select(ga_table, viewID, longName), by="viewID") %>%
      mutate(type = paste(j,"\n",paste0(range(range_days), collapse = " to "))) %>%
      select(-viewID)
    
    level_text <- c(level_text, paste(j,"\n",paste0(range(range_days), collapse = " to ")))
    
    summary_data <- bind_rows(summary_data, summary_sessions)
    
  }
  
  summary_data <- summary_data %>% 
    filter(!is.na(longName)) %>% 
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
  
  min_app <- select(summary_data_full, bin, type, sessions, longName) %>%
    filter(type == levels(summary_data_full$type)[1]) %>%
    group_by(bin) %>%
    slice(which.min(sessions))

  max_vals <- summary_data_full %>%
    group_by(type) %>%
    summarize(max_val = max(scaled_value, na.rm = TRUE)) 
  
  summary_data_full <- summary_data_full %>%  
    left_join(max_vals, by = "type") %>%
    mutate(text_placement = scaled_value + 0.15*max_val)

  # colfunc <- colorRampPalette(c("#E4B3C0","#FFF5F0"))
  # colfunc <- colorRampPalette(c("maroon","#FFF5F0"))
  colfunc <- colorRampPalette(c("grey75","grey95"))
  cols <- colfunc(4)
  
  port_graph <- ggplot(data = summary_data_full, 
                       aes(x = longName, y = scaled_value)) +
    geom_rect(aes(fill = bin),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,color = NA) +
    geom_point(color = bar_line_col) +
    geom_segment(aes(xend = longName), yend=0, size = 0.1, color = bar_line_col) +
    geom_segment(aes(xend = longName, y = scaled_newUser), yend=0, col=bar_line_col, size=1.5) + 
    geom_text(aes(label = session_text, y = text_placement), 
              size = 3, hjust = .75, color = text_col) + 
    facet_grid(bin ~ type, scales = "free",
               space = "free_y", drop = TRUE) +
    coord_flip() +
    scale_fill_manual(values = cols) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x =  element_blank(),
          strip.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          axis.ticks=element_blank(),
          legend.position = "none"
          ) 
  
  info_graph <- ggplot_build(port_graph)
  layout_stuff <- info_graph$layout
  lower_ranges <- layout_stuff$panel_ranges[[12]]$x.range
  mid_ranges <- layout_stuff$panel_ranges[[8]]$x.range
  
  ymin <- 0.45*(diff(lower_ranges))+lower_ranges[1]
  ymax <- 0.98*(diff(lower_ranges))+lower_ranges[1]
  
  ystart <- 0.50*(diff(lower_ranges))+lower_ranges[1]
  ymid <- 0.6*(diff(lower_ranges))+lower_ranges[1]
  yend <- 0.95*(diff(lower_ranges))+lower_ranges[1]
  
  bin_mid <- 0.95*(diff(mid_ranges))+mid_ranges[1]
  
  text_df <- data.frame(label = c("Very High Traffic","High Traffic","Moderate Traffic","Low Traffic"),
                        type = factor(levels(summary_data_full$type)[2], levels = levels(summary_data_full$type)),
                        bin = factor(levels(summary_data_full$bin), levels = levels(summary_data_full$bin)),
                        longName = 1.25,
                        y = bin_mid,
                        stringsAsFactors = FALSE)
  
  fake_legend <- data.frame(label = c("Total Users","New Users"),
                            type = factor(levels(summary_data_full$type)[3], levels = levels(summary_data_full$type)),
                            bin = factor(levels(summary_data_full$bin)[4], levels = levels(summary_data_full$bin)),
                            longName = rev(levels(summary_data_full$longName)[1:2]),
                            ymin = ymin,
                            ystart = ystart,
                            ymid = ymid,
                            yend = yend,
                            ymax = ymax,
                            stringsAsFactors = FALSE)
  
  port_graph <- port_graph +
    geom_label(data = text_df, 
               aes(x = longName, y = y, label = label), 
               size = 3.5,hjust = "right") +
    geom_rect(data = fake_legend[1,], aes(y = 0),
              ymin = fake_legend$ymin[1],
              ymax = fake_legend$ymax[1],
              xmin = .4,
              xmax = 2.6,
              color = "black", fill = "white") +
    geom_text(data = fake_legend,
              aes(x = longName, y = yend, label = label), 
              hjust = "right", col = "black") +
    geom_segment(data = fake_legend[2,],
                 aes(x = longName,
                     xend = longName,
                     y = ystart, yend=ymid), col=bar_line_col, size=1.5) +
    geom_segment(data = fake_legend[1,], aes(xend = longName, y=ystart, yend=ymid), size=0.1, col=bar_line_col) + 
    geom_point(data = fake_legend[1,], aes(x = longName, y=ymid), col=bar_line_col) 

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



