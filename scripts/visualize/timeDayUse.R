#portfolio-wide
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

visualize.portfolio_timeDayUse <- function(viz=as.viz("portfolio_timeDayUse_year")) {

  dep_data <- readDepends(viz)

  viz.data <- dep_data[["viz_data"]]
  project.data <- dep_data[["project_table"]]

  height = viz[["height"]]
  width = viz[["width"]]
  plot_type <- viz[["plottype"]]
  bar_line_col = viz[["bar_line_col"]]

  port_device <- plot_tod(viz.data, bar_line_col, project.data)

  ggsave(port_device, filename = viz[["location"]],
         height = height, width = width)


}

visualize.timeDayUse_app <- function(viz=as.viz("timeDayUse_app_year")) {

  library(dplyr)
  library(ggplot2)
  library(scales)

  dep_data <- readDepends(viz)
  viz.data <- dep_data[["viz_data"]]
  project.data <- dep_data[["project_table"]]

  height = viz[["height"]]
  width = viz[["width"]]
  plot_type <- viz[["plottype"]]
  bar_line_col = viz[["bar_line_col"]]

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  for(i in unique(viz.data$viewID)) {

    location <- paste0("cache/visualize/", i,plot_type, ".png")

    sub.data <- filter(viz.data, viewID == i)

    port_device <- plot_tod(sub.data,bar_line_col, project.data)

    ggsave(port_device, filename = location,
           height = height, width = width)

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))
  }
  write.csv(x, file=viz[["location"]], row.names = FALSE)
}

plot_tod <- function(viz.data, bar_line_col, project.data){

  projTZ <- select(project.data, viewID, timezone)

  viz.data <- left_join(viz.data, projTZ, by = 'viewID')

  viz.data <- mutate(viz.data, hour = as.character(recode(timezone,
                                                          `America/New_York` = as.numeric(hour),
                                                          `America/Chicago` = as.numeric(hour) + 1,
                                                          `America/Denver` = as.numeric(hour) + 2,
                                                          `America/Los_Angeles` = as.numeric(hour) + 3)))

  hourSum <- viz.data %>%
    mutate(hour = as.numeric(hour)) %>%
    group_by(hour) %>%
    summarise(Sessions = n())

  # Because of central time conversions, there were 25's in there:
  hourSum$hour[hourSum$hour > 24] <- hourSum$hour[hourSum$hour > 24] - 24

  hourSum$hour <- factor(hourSum$hour, levels = as.character(0:24))

  hourSum <- rename(hourSum, `Hour of the Day` = hour)

  port_device <-   ggplot(data = hourSum) +
    geom_col(aes(x = `Hour of the Day`, y=Sessions),
             fill = bar_line_col) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size=14, color = "grey30"),
          axis.text = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(drop=FALSE)

  return(port_device)
}

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}
