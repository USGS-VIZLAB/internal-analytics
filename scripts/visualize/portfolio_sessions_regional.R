visualize.portfolio_sessions_regional <- function(viz=as.viz("app_regionality"),
                                                  metric_type =
                                                    c("r2", "slopes", "percapita",
                                                      "normpercapita", "shannon", "gini")){

  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)

  deps <- readDepends(viz)

  height = viz[["height"]]
  width = viz[["width"]]
  bar_line_col = viz[["bar_line_col"]]
  text_col = viz[["text_col"]]

  summary_data <- deps[["viz_data"]]

  # change color for lowest quarter
  only_regionality <- filter(summary_data, type == "Geographic Reach")
  lowest_qtr <- quantile(only_regionality$scaled_value, 0.25, na.rm = TRUE)
  summary_data <- summary_data %>%
    mutate(bar_color = bar_line_col) %>%
    rowwise %>%
    mutate(bar_color = ifelse(grepl("Geographic", type) &&
                                scaled_value <= lowest_qtr,
           yes = "blue", no = bar_color)) %>%
    ungroup

  # decide where to place text
  max_vals <- summary_data %>%
    group_by(type) %>%
    summarize(max_val = max(scaled_value, na.rm = TRUE))

  summary_data <- summary_data %>%
    left_join(max_vals, by = "type") %>%
    mutate(text_placement = scaled_value + 0.06*max_val)

  # prepare dimensions for each pane that won't truncate the text values
  dummy_for_ylims <- summary_data %>%
    select(bin, type, longName, scaled_value, max_val) %>%
    group_by(bin, type) %>%
    arrange(desc(scaled_value)) %>%
    slice(1) %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(scaled_value = max_val*1.15)

  # define scales for shape, color, and fill
  shps <- c("none"=21,
            "up"=24,
            "down"=25)
  cols <- c("none" = viz[["trend_color"]]$none,
            "up" = viz[["trend_color"]]$up,
            "down" = viz[["trend_color"]]$down)
  colfunc <- colorRampPalette(c("grey75","grey95"))
  fills <- colfunc(4)
  names(fills) <- levels(summary_data$bin)

  fills <- c(fills,
             setNames(cols, paste(names(cols), 'TRUE')),
             setNames(rep('white', 3), paste(names(cols), 'FALSE')))

  graph_body <- ggplot(data = summary_data,
                       aes(x = longName, y = scaled_value)) +
    geom_rect(aes(fill = bin),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,color = NA) +
    geom_segment(aes(xend = longName), yend=0, size = 0.65, col=bar_line_col) +
    geom_segment(aes(xend = longName, y = scaled_newUser),
                 yend=0, size=1.15, col=bar_line_col) +
    geom_text(aes(label = session_text, y = text_placement, color = trend),
              size = 3, hjust = 0,
              data = summary_data) +
    geom_point(aes(shape=trend, fill = trend_complete, color=trend),
               data = summary_data) +
    geom_blank(data=dummy_for_ylims) +
    facet_grid(bin ~ type, scales = "free",
               space = "free_y", drop = TRUE) +
    coord_flip() +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = fills) +
    scale_shape_manual(values = shps) +
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

  #info_graph <- ggplot_build(graph_body)
  #print(info_graph)
  grid.newpage()
  g <- arrangeGrob(graph_body,
                   bottom = textGrob(c("Regional", "National"),
                                     x = 0, y = 0,
                                     gp = gpar(fontsize = 2)))
  grid.draw(g)
  ggsave(g, file = viz[["location"]], height = height, width = width)
}
