#' create the urls of all the project pages
library(dplyr)
process.projectlinks <- function(viz = as.viz("project_links")) {
  deps <- readDepends(viz)
  masterTable <- deps[['project_table']]
  sessions_all <- deps[['order_projects']] %>%
    filter(grepl(pattern = "Year", x = type)) %>%
    select(-longName) #GCMRC and NGWMN had longName changed to shortName so they fit
                      #on the main plot.  It messes up the join here
  table <- masterTable %>%
     left_join(sessions_all) %>%
    mutate(url = paste0(shortName, ".html")) %>%
    arrange(desc(sessions)) %>%
    select(shortName, longName, url, bin)
  links <- apply(table, 1, function(x){list(longName=x[[2]], url=x[[3]], bin=x[[4]])})
  saveRDS(links, file = viz[['location']])
}
