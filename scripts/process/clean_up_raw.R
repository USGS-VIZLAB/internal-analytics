process.clean_up_raw <- function(viz = as.viz("clean_up_raw")){
  library(dplyr)
  library(data.table)
  
  viz.data <- readDepends(viz)[["fetchGA_cida"]]
  
  viz.data <- viz.data %>%
    mutate(date = as.Date(paste(year,month,day,sep="-"), origin = "1970-01-01")) %>%
    mutate(dateTime = as.POSIXct(paste0(date," ",hour,":00:00")))
    
  fwrite(viz.data, file=viz[["location"]], quote = TRUE, row.names = FALSE)
}