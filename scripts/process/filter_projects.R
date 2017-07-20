process.filter_projects <- function(viz=as.viz("filter_projects")){
  library(dplyr)
  library(tidyr)

  deps <- readDepends(viz)

  viz.data <- deps[["year_filter"]]
  ga_table <- deps[['project_table']]


  ga_table <- ga_table %>%
    filter(viewID %in% unique(viz.data$viewID))

  saveRDS(ga_table, file = viz[["location"]])

}
