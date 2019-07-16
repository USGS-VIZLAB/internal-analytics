visualize.app_state_pop_vs_traffic <- function(viz){
  height = viz[["height"]]
  width = viz[["width"]]
  plot_type = viz[["plottype"]]
  state_app_traffic <- readDepends(viz)[["state_app_traffic"]]

  output_df <- tibble(id = character(),
                  loc = character(),
                  type = character())

  for(app_id in na.omit(unique(state_app_traffic$viewID))){
    sub_data <- filter(state_app_traffic, viewID == app_id)

    location <- paste0("cache/visualize/",app_id,"_",plot_type,".png")
    app_plot <- plot_app_state_traffic_pop(sub_data)
    ggsave(filename = location, plot = app_plot,
           width = width, height = height, units = "in",
           dpi = 150)

    output_df <- bind_rows(output_df, tibble(id = app_id,
                                 loc = location,
                                 type = plot_type))

  }

  write.csv(output_df, file=viz[["location"]], row.names = FALSE)
}

plot_app_state_traffic_pop <- function(app_state_traffic) {
  app_state_traffic_pop_diff <- app_state_traffic %>%
    mutate(pct_traffic_minus_pop = traffic_pct - pop_pct) %>%
    arrange(pct_traffic_minus_pop) %>%
    mutate(abbr = factor(abbr, levels = abbr))
  #make two-panel plot of map and bar plot
  bars <- ggplot(app_state_traffic_pop_diff, aes(x = abbr, y = pct_traffic_minus_pop)) +
    geom_col() + coord_flip() +
    labs(x = "State", y = "Percent of traffic - percent of population")
  map <- plot_usmap(regions = "states", data = app_state_traffic_pop_diff,
                    values = "pct_traffic_minus_pop") +
    scale_fill_gradient2(name = "% traffic —\n % population") +
    theme(legend.position = c(0.25, 0.87), legend.direction = "horizontal",
          legend.background = element_blank())
  return(bars)
}




#build individual plot
