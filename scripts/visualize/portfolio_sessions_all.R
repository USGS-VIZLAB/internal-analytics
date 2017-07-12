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

  summary_data_full <- deps[["sessions_all"]]

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

  summary_data_full$text_placement[summary_data_full$sessions == 0] <- 0

  colfunc <- colorRampPalette(c("grey75","grey95"))
  cols <- colfunc(4)

  names(cols) <- levels(summary_data_full$bin)

  cols <- c(cols, "none" = viz[["trend_color"]]$none,
            "up" = viz[["trend_color"]]$up,
            "down" = viz[["trend_color"]]$down)

  port_graph <- ggplot(data = summary_data_full,
                       aes(x = longName, y = scaled_value)) +
    geom_rect(aes(fill = bin),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,color = NA) +
    scale_color_manual(values = c("none" = viz[["trend_color"]]$none,
                                  "up" = viz[["trend_color"]]$up,
                                  "down" = viz[["trend_color"]]$down)) +
    geom_segment(aes(xend = longName), yend=0, size = 0.65, color = bar_line_col) +
    geom_segment(aes(xend = longName, y = scaled_newUser),
                 yend=0, col=bar_line_col, size=1.15) +
    geom_text(aes(label = session_text, y = text_placement, color = trend),
              size = 3, hjust = .75,
              data = summary_data_full[summary_data_full$scaled_value != 0,]) +
    geom_text(aes(label = session_text, y = text_placement, color = trend),
              size = 3, hjust = 0,
              data = summary_data_full[summary_data_full$scaled_value == 0,]) +
    geom_point(aes(shape=trend, fill = trend), color = "black",
               data = summary_data_full[summary_data_full$scaled_value != 0,]) +
    scale_shape_manual(values = c("none"=21,
                                  "up"=24,
                                  "down"=25)) +
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

  if(packageVersion("ggplot2") >= "2.2.1.9000"){
    lower_ranges <- layout_stuff$panel_scales_y[[3]]$range$range
    high_ranges <- layout_stuff$panel_scales_y[[1]]$range$range
  } else {
    lower_ranges <- layout_stuff$panel_ranges[[12]]$x.range
    high_ranges <- layout_stuff$panel_ranges[[4]]$x.range
  }

  ymin <- 0.45*(diff(lower_ranges))+lower_ranges[1]
  ymax <- 0.98*(diff(lower_ranges))+lower_ranges[1]

  ystart <- 0.50*(diff(lower_ranges))+lower_ranges[1]
  ymid <- 0.6*(diff(lower_ranges))+lower_ranges[1]
  yend <- 0.95*(diff(lower_ranges))+lower_ranges[1]

  bin_mid <- 0.95*(diff(high_ranges))+high_ranges[1]

  text_df <- data.frame(label = c("Very High Traffic","High Traffic","Moderate Traffic","Low Traffic"),
                        type = factor(levels(summary_data_full$type)[1], levels = levels(summary_data_full$type)),
                        bin = factor(levels(summary_data_full$bin), levels = levels(summary_data_full$bin)),
                        longName = 1.25,
                        y = bin_mid,
                        stringsAsFactors = FALSE)

  fake_legend <- data.frame(label = c("Total Users","New Users","Trending Up","Trending Down"),
                            type = factor(levels(summary_data_full$type)[3], levels = levels(summary_data_full$type)),
                            bin = factor(levels(summary_data_full$bin)[4], levels = levels(summary_data_full$bin)),
                            longName = rev(levels(summary_data_full$longName)[1:4]),
                            ymin = ymin,
                            ystart = ystart,
                            ymid = ymid,
                            yend = yend,
                            ymax = ymax,
                            trend = c(NA, NA, "up", "down"),
                            stringsAsFactors = FALSE)

  fake_legend$mid_mid <- fake_legend$ymid - fake_legend$ystart

  port_graph <- port_graph +
    geom_label(data = text_df,
               aes(x = longName, y = y, label = label),
               size = 3.5,hjust = "right",label.r = unit(0, "lines")) +
    geom_rect(data = fake_legend[1,], aes(y = 0),
              ymin = fake_legend$ymin[1],
              ymax = fake_legend$ymax[1],
              xmin = .4,
              xmax = 4.6,
              color = "black", fill = "white") +
    geom_text(data = fake_legend,
              aes(x = longName, y = yend, label = label),
              hjust = "right", col = "black") +
    geom_segment(data = fake_legend[2,],
                 aes(x = longName,
                     xend = longName,
                     y = ystart, yend=ymid), col=bar_line_col, size=1.15) +
    geom_segment(data = fake_legend[1,], aes(xend = longName, y=ystart, yend=ymid), size=0.65, col=bar_line_col) +
    geom_point(data = fake_legend[1,], aes(x = longName, y=ymid), col=bar_line_col) +
    geom_point(data = fake_legend[3:4,], color = "black", aes(x = longName, y=mid_mid, shape=trend, fill = trend))

  ggsave(port_graph, file = viz[["location"]], height = height, width = width)

}

