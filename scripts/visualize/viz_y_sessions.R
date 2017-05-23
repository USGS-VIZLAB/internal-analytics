

visualize.viz_d_sessions <- function(viz){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["viz_data"]]
  
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)
  
  plot_type <- viz[["plottype"]]
  
  max_date <- max(viz.data$date, na.rm = TRUE)
  full_dates <- seq(max_date, length = 24, by=-60*60)
  empty_df <- data.frame(date=full_dates)
  
  for(i in unique(viz.data$viewID)){
    
    sub_data <- select(viz.data, date, viewID, sessions) %>%
      filter(viewID == i) %>%
      group_by(date) %>%
      summarize(sessions = sum(sessions, na.rm = TRUE)) %>%
      data.frame() %>%
      right_join(empty_df, by="date") %>%
      arrange(desc(date))
    
    sub_data$sessions[is.na(sub_data$sessions)] <- 0
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    png(location, width = width, height = height, res = 150)
    par(oma=c(0,0,0,0),
        mar=c(1.5,2.5,1,1.5),
        las=1, 
        mgp = c(1,0.3,0),
        tck=0.02)
    
    if(nrow(sub_data) > 0){
      plot(x = sub_data$dateTime, 
           sub_data$sessions, 
           type="b",xlab="",ylab="",yaxt='n',
           frame.plot = FALSE)
    } else { 
      plot(1, axes=FALSE, 
           type="n",xlab="",ylab="")
    }
    last.tick <- tail(pretty(c(0, max(sub_data$sessions, na.rm = TRUE))),2)[1]  
    axis(1, at=c(par()$usr[1],par()$usr[2]), 
         labels = c("",""), lwd.tick=0)
    axis(2, at=c(-last.tick, 0, last.tick, last.tick*2), 
         labels = c("","0", pretty_num(last.tick), ""))
    par(xpd = NA)
    text(par('usr')[1], par('usr')[4]*1.04, 
         labels = paste0(range(sub_data$date), collapse = " to "), pos = 4)
    dev.off()
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}

# couldn't remember how to get it to source another script, so re-using this form portfolio sessions
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
