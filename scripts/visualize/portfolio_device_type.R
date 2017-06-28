visualize.portfolio_device_type <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)

  viz.data <- readDepends(viz)[["viz_data"]]
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]

  viz.data <- select(viz.data, viewID,deviceCategory)

  port_device <- plot_device(viz.data, bar_line_col)

  ggsave(port_device, filename = viz[["location"]],
         height = height, width = width)

}

visualize.viz_device_type <- function(viz){
  library(dplyr)
  library(ggplot2)
  library(scales)

  viz.data <- readDepends(viz)[["viz_data"]]
  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]

  viz.data <- select(viz.data, viewID,deviceCategory)

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)

    location <- paste0("cache/visualize/",i,"_",plot_type,".png")

    port_device <- plot_device(sub_data, bar_line_col)

    ggsave(port_device, filename = location,
           height = height, width = width)

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))

  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

plot_device <- function(viz.data, bar_line_col){

  sub_data_range <- viz.data %>%
    group_by(deviceCategory) %>%
    summarize(Sessions = n()) %>%
    mutate(pos = Sessions/2)

  if(nrow(sub_data_range) == 0){
    sub_data_range <- data.frame(deviceCategory = c("desktop","mobile","tablet"),
                                 Sessions = c(NA,NA,NA),
                                 pos = c(NA, NA, NA),
                                 stringsAsFactors = FALSE)
  }

  port_device <- ggplot() +
    geom_col(data = sub_data_range,
             aes(x = reorder(deviceCategory, Sessions), y=Sessions), fill = bar_line_col) +
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


  offcenter_index <- which(sub_data_range$Sessions < 0.35*max(sub_data_range$Sessions,na.rm = TRUE))

  if(length(offcenter_index) > 0){
    centered <- sub_data_range[-offcenter_index,]
    offcenter <- sub_data_range[offcenter_index,]
    offcenter$pos <- offcenter$Sessions

    port_device <- port_device +
      geom_text(data = offcenter,
                aes(x = reorder(deviceCategory, Sessions), y = pos, label = comma(Sessions)),
                size = 5, color = "grey30",
                hjust = 0, nudge_y = 0.05*offcenter$pos) +
      geom_text(data = centered,
                aes(x = reorder(deviceCategory, Sessions), y = pos, label = comma(Sessions)),
                size = 5, color = "white")

  } else {
    port_device <- port_device +
      geom_text(data = sub_data_range,
                aes(x = reorder(deviceCategory, Sessions), y = pos, label = comma(Sessions)),
                size = 5, color = "white")
  }

  return(port_device)
}
