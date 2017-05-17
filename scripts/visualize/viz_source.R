visualize.viz_source <- function(viz = as.viz("viz_source")){
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]

  for(i in unique(viz.data$viewID)){
    sub_data <- filter(viz.data, viewID == i)
    
    source_sum <- data.frame(table(sub_data$source),
                             stringsAsFactors = FALSE) %>%
      arrange(desc(Freq)) %>%
      mutate(source = as.character(Var1)) %>%
      data.frame() 
    
    source_sum <- source_sum[1:min(c(5, nrow(source_sum))),]
    

    if(nrow(source_sum) == 0){
      source_sum <- data.frame(source=c("google","(direct)"),
                               Freq = c(NA,NA),
                               stringsAsFactors = FALSE) 
    } else if(nrow(source_sum) < 5){
      source_sum <- rbind(source_sum,data.frame(source=rep("",5-nrow(source_sum)),
                               Freq = rep(NA,5-nrow(source_sum)),
                               stringsAsFactors = FALSE) )
    }
    
    location <- paste0("cache/visualize/",i,"_",plot_type,".png")
    
    port_source <-   ggplot(data = source_sum) +
      geom_col(aes(x = reorder(source, Freq), y=Freq), fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      ylab("Total Sessions") +
      theme(axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            axis.text = element_text(size = 14),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      scale_y_continuous(labels = comma)
    
    ggsave(port_source, filename = location, 
           height = height, width = width)
    
    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))      
    
  }
  
  write.csv(x, file=viz[["location"]], row.names = FALSE)
  
}
