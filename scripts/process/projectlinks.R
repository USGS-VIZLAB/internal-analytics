#' create the urls of all the project pages
library(dplyr)
process.projectlinks <- function(viz = as.viz("project_links")) {
  deps <- readDepends(viz)
  masterTable <- deps[['project_table']]
  sessions_all <- deps[['order_projects']] %>%
    filter(grepl(pattern = "Year", x = type))
  table <- masterTable %>%
     left_join(sessions_all) %>%
    mutate(url = paste0(shortName, ".html")) %>%
    arrange(desc(sessions)) %>%
    select(shortName, longName, url)
  links <- apply(table, 1, function(x){list(longName=x[[2]], url=x[[3]])})
  saveRDS(links, file = viz[['location']])
}
