library(yaml)
library(dplyr)
readData.projectTable <- function(viz){
  return(do.call(bind_rows, yaml.load_file(viz[['location']])))
}