visualize.viz_geo_portfolio <- function(viz=as.viz("viz_geo_portfolio")){
  library(dplyr)
  library(ggplot2)
  library(maps)
  
  viz.data <- readDepends(viz)[["geo_apps"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  states <- map_data("state")
  
  region_summary <- data.frame(table(viz.data$region), stringsAsFactors = FALSE)  %>%
    arrange(desc(Freq)) %>%
    mutate(region = tolower(Var1)) %>%
    select(-Var1) 
  
  if(nrow(region_summary) > 0){
    map.df <- left_join(states,region_summary, by="region")
    
    map_plot <- ggplot(map.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=Freq))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map("albers", lat0=30, lat1=40) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
  } else {
    map_plot <- ggplot(states, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=1))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map("albers", lat0=30, lat1=40) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  }
  
  ggsave(map_plot, filename = viz[["location"]], height = height, width = width, units = "in")
  
}
