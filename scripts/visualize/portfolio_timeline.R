visualize.portfolio_timeline <- function(viz = as.viz("portfolio_timeline")){
  library(dplyr)
  library(ggplot2)
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  viz.data <- readDepends(viz)[["sessions_and_new_users"]]

  range_text <- viz[["rangetext"]]
  plot_type <- viz[["plottype"]]
  
  max_date <- max(viz.data$date, na.rm = TRUE)
  
  year_days = seq(max_date, length = 2, by = range_text)
  full_dates <- seq(max_date, length = -as.numeric(diff(year_days))+1, by=-1)
  empty_df <- data.frame(date = full_dates)
  
  sub_data_year <- select(viz.data, date, sessions) %>%
    filter(date >= year_days[2],
           date <= year_days[1]) %>%
    group_by(date) %>%
    summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
    data.frame() %>%
    right_join(empty_df, by="date") %>%
    arrange(desc(date))
  
  sub_data_year$sessions[is.na(sub_data_year$sessions)] <- 0
  

  png(viz[["location"]], height = height, width = width, res = 150)
  
  par(oma=c(0,0,0,0),
      mar=c(1.5,2.5,1,1.5),
      las=1, 
      mgp = c(1,0.3,0),
      tck=0.02)
  
  type <- ifelse(range_text == "-1 week", "b", "l")
  plot(x = sub_data_year$date, 
       sub_data_year$sessions, 
       type=type,xlab="",ylab="", yaxt='n', ylim = c(0, max(sub_data_year$sessions, na.rm = TRUE)),
       frame.plot = FALSE)
    
  last.tick <- tail(pretty(c(0, max(sub_data_year$sessions, na.rm = TRUE))),2)[1]  
  axis(1, at=c(par()$usr[1],par()$usr[2]), 
       labels = c("",""), lwd.tick=0)
  axis(2, at=c(-last.tick, 0, last.tick, last.tick*2), 
       labels = c("","0", pretty_num(last.tick), ""))
  par(xpd = NA)
  text(par('usr')[1], par('usr')[4]*1.04, 
       labels = paste0(range(sub_data_year$date), collapse = " to "), pos = 4)
  
  dev.off()


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
