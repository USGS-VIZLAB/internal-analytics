#' create the urls of all the project pages
library(dplyr)
process.projectlinks <- function(viz = as.viz("project_links")) {
  deps <- readDepends(viz)
  table <- deps[['project_table']] %>%
    select(viewID, longName) %>%
    mutate(url = paste0(viewID, ".html")) %>%
    arrange(longName)
  links <- apply(table, 1, function(x){list(longName=x[[2]], url=x[[3]])})
  saveRDS(links, file = viz[['location']])
}