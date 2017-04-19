visualize.portfolio_source <- function(viz = as.viz("portfolio_source")){
  library(dplyr)
  
  viz.data <- readDepends(viz)[["source_counts"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  source_sum <- viz.data %>%
    group_by(source) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  source_sum <- source_sum[1:min(c(10, nrow(source_sum))),]
  
  png(viz[["location"]], height = height, width = width)  
  par(oma=c(0,0,0,0),las=1)
  if(nrow(source_sum) > 0){
    barplot(rev(source_sum$count), horiz=TRUE,
            names.arg=rev(source_sum$source))
  } else {
    barplot(c(0,0), horiz=TRUE,
            names.arg=c("google","(direct)"),
            xlim = c(0,10))
  }
  dev.off()      

}
