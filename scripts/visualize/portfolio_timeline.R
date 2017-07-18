visualize.portfolio_timeline <- function(viz){
  library(dplyr)

  height = viz[["height"]]
  width = viz[["width"]]

  viz.data <- readDepends(viz)[["viz_data"]] %>%
    select(-viewID) %>%
    group_by(date) %>%
    summarise(sessions = sum(sessions, na.rm = TRUE))

  plot_type <- viz[["plottype"]]

  png(viz[["location"]], height = height, width = width, res = 150)

  plot_timeline(viz.data, plot_type)

  dev.off()

}

visualize.viz_y_sessions <- function(viz){
  library(dplyr)

  height = viz[["height"]]
  width = viz[["width"]]
  plot_type = viz[["plottype"]]

  viz.data <- readDepends(viz)[["viz_data"]]

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)

    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    png(location, height = height, width = width, res = 150)
      plot_timeline(sub_data, plot_type)
    dev.off()

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))

  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

plot_timeline <- function(viz.data, type){
  latest_day <- Sys.Date()-1
  range_text <- c("-1 year","-1 month","-1 week")
  names(range_text) <- c("year_line","month_line","week_line")

  format_style <- c("%b","%b %d","%b %d")
  names(format_style) <- c("year_line","month_line","week_line")

  day_breaks <- c("2 mon","1 week","2 day")
  names(day_breaks) <- c("year_line","month_line","week_line")


  range_days = seq(latest_day, length = 2, by = range_text[type])
  range_days <- range_days[order(range_days)]

  par(oma=c(0,0,0,0),
      mar=c(1.5,2.5,1,1.5),
      las=1,
      mgp = c(1,0.3,0),
      tck=0.02)

  line_type <- ifelse(type == "week_line", "b", "l")

  plot(x = viz.data$date,
       viz.data$sessions,
       type=line_type,
       xlab="",ylab="", yaxt='n',xaxt='n',
       xlim = range_days,
       ylim = c(0, max(viz.data$sessions, na.rm = TRUE)),
       frame.plot = FALSE)

  last.tick <- tail(pretty(c(0, max(viz.data$sessions, na.rm = TRUE))),2)[1]

  pretty_days <- pretty(range_days)
  axis.Date(1, at=seq(from = pretty_days[1],
                      to = pretty_days[length(pretty_days)],
                      by=day_breaks[type]),
            format=format_style[type])
  axis(1, at=c(par()$usr[1],par()$usr[2]),
       labels = c("",""), lwd.tick=0)
  axis(2, at=c(-last.tick, 0, last.tick, last.tick*2),
       labels = c("","0", pretty_num(last.tick), ""))
  par(xpd = NA)
  text(par('usr')[1], par('usr')[4]*1.04,
       labels = paste0(range(range_days), collapse = " to "), pos = 4)

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
