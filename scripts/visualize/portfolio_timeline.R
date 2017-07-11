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

  plot_timeline(viz.data)

  dev.off()

}

visualize.viz_y_sessions <- function(viz){
  library(dplyr)

  height = viz[["height"]]
  width = viz[["width"]]

  viz.data <- readDepends(viz)[["viz_data"]]

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)

    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    png(location, height = height, width = width, res = 150)
      plot_timeline(sub_data)
    dev.off()

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))

  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

plot_timeline <- function(viz.data){

  #TODO: add check for nrows == 0

  par(oma=c(0,0,0,0),
      mar=c(1.5,2.5,1,1.5),
      las=1,
      mgp = c(1,0.3,0),
      tck=0.02)

  type <- ifelse(nrow(viz.data) < 15, "b", "l")

  plot(x = viz.data$date,
       viz.data$sessions,
       type=type,
       xlab="",ylab="", yaxt='n',
       ylim = c(0, max(viz.data$sessions, na.rm = TRUE)),
       frame.plot = FALSE)

  last.tick <- tail(pretty(c(0, max(viz.data$sessions, na.rm = TRUE))),2)[1]
  axis(1, at=c(par()$usr[1],par()$usr[2]),
       labels = c("",""), lwd.tick=0)
  axis(2, at=c(-last.tick, 0, last.tick, last.tick*2),
       labels = c("","0", pretty_num(last.tick), ""))
  par(xpd = NA)
  text(par('usr')[1], par('usr')[4]*1.04,
       labels = paste0(range(viz.data$date), collapse = " to "), pos = 4)

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
