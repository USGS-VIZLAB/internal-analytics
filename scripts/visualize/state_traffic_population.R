visualize.app_state_pop_vs_traffic <- function(viz){
  height = viz[["height"]]
  width = viz[["width"]]
  plot_type = viz[["plottype"]]
  state_app_traffic <- readDepends(viz)[["state_app_traffic"]]

  output_df <- tibble(id = character(),
                  loc = character(),
                  type = character())
  plotting_function <- get(sub(pattern = ".*?_", replacement = "", x = plot_type))
  for(app_id in na.omit(unique(state_app_traffic$viewID))){
    sub_data <- filter(state_app_traffic, viewID == app_id)
    app_state_traffic_pop_diff <- sub_data %>%
      mutate(pct_traffic_minus_pop = traffic_pct - pop_pct,
             pct_traffic_divby_pop = traffic_pct/pop_pct) %>%
      arrange(pct_traffic_minus_pop) %>%
      mutate(abbr = factor(abbr, levels = abbr))
    location <- paste0("cache/visualize/",app_id,"_",plot_type,".png")
    app_plot <- plotting_function(app_state_traffic_pop_diff)
    ggsave(filename = location, plot = app_plot,
           width = width, height = height, units = "in",
           dpi = 150)

    output_df <- bind_rows(output_df, tibble(id = app_id,
                                 loc = location,
                                 type = plot_type))
  }
  write.csv(output_df, file=viz[["location"]], row.names = FALSE)
}

state_traffic_pop_bars <- function(app_state_traffic) {
  bars <- ggplot(app_state_traffic, aes(x = reorder(abbr, pct_traffic_divby_pop),
                                        y= pct_traffic_divby_pop, fill = pop_pct)) +
    geom_col() + coord_flip() +
    labs(x = "State", y = "Percent of traffic divided by percent of population", fill = "Percent of U.S. population") +
    geom_hline(yintercept = 1, col = "blue") +
    scale_fill_gradient(low = "white", high = "black") +
    scale_y_continuous(limits = c(0, NA))
  return(bars)
}

state_traffic_pop_map <- function(app_state_traffic) {
  map <- plot_usmap(regions = "states", data =  app_state_traffic,
                    values = "pct_traffic_divby_pop",
                    lines = "grey75") +
    scale_fill_gradient2(name = "% traffic over\n % population",
                         low = "red", high = "blue",
                         midpoint = 1) +
    theme(legend.position = c(0.35, 0.9), legend.direction = "horizontal",
          legend.background = element_blank())
  return(map)
}
