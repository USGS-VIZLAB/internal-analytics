visualize.viz_new_vs_returning <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)

  viz.data <- readDepends(viz)[["viz_data"]]

  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  plottype <- viz[["plottype"]]

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)

    port_source <- new_return_plot(sub_data, bar_line_col)

    location <- paste0("cache/visualize/",i,plottype,".png")

    ggsave(port_source, filename = location,
           height = height, width = width)

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plottype,
                                 stringsAsFactors = FALSE))

  }


  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

visualize.viz_new_vs_returning_portfolio <- function(viz = as.viz("portfolio_new_return_year")){
  library(dplyr)
  library(ggplot2)
  library(scales)

  viz.data <- readDepends(viz)[["viz_data"]]

  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]

  port_source <- new_return_plot(viz.data, bar_line_col)

  ggsave(port_source, filename = viz[["location"]],
         height = height, width = width)

}

new_return_plot <- function(viz.data, bar_line_col){
  library(ggplot2)
  library(scales)
  library(tidyr)

  x <- viz.data %>%
    mutate(returningUsers = users - newUsers) %>%
    select(New = newUsers, Returning = returningUsers) %>%
    gather(new.or.returning, value) %>%
    group_by(new.or.returning) %>%
    summarize(Sessions = sum(value)) %>%
    mutate(pos = Sessions/2)

  port_source <- ggplot() +
    geom_col(data = x, aes(x = new.or.returning, y = Sessions), fill = bar_line_col) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.title.x = element_text(size=14, color = "grey30"),
          panel.border = element_blank(),
          plot.margin=unit(c(0.1,1,0.1,0.1),"cm"))

  if(min(x$Sessions) < 0.35*max(x$Sessions)){
    offcenter_index <- which.min(x$Sessions)
    centered <- x[-offcenter_index,]
    offcenter <- x[offcenter_index,]
    offcenter$pos <- offcenter$Sessions

    port_source <- port_source +
      geom_text(data = centered, aes(x = new.or.returning, y = pos, label = comma(Sessions)),
                size = 5, color = "white") +
      geom_text(data = offcenter, aes(x = new.or.returning, y = pos, label = comma(Sessions)),
                size = 5, color = "grey30", hjust = 0,
                nudge_y = 0.05*offcenter$pos)
  } else {
    port_source <- port_source +
      geom_text(data = x, aes(x = new.or.returning, y = pos, label = comma(Sessions)),
                size = 5, color = "white")
  }

  return(port_source)
}
