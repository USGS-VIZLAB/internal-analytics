#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {
  deps <- readDepends(viz)
  projects <- deps[['project_table']][['viewID']] # get projects from deps
  img.files <- list(
    month_sessions = deps[['viz_month_sessions']]
  )
  for (proj in projects) {
    # get relative paths for images
    proj.imgs <- sapply(img.files, function(x){
      row <- filter(x, id == proj)
      img <- list(
        location = row[['loc']],
        mimetype = "image/png",
        alttext = row[['type']],
        title = row[['type']]
      )
      img <- as.viz(img)
      img <- as.publisher(img)
      publish(img)
    })
    
    pub <- list(
      id = paste0(proj, "-page"),
      publisher = page,
      template = viz[['template']],
      depends = viz[['depends']],
      context = list(
        header = viz[['context']][['header']],
        footer = viz[['context']][['footer']],
        fig1 = proj.images[['fig1']],
        fig2 = proj.images[['fig2']]
      )
    )
    pub <- as.viz(pub)
    pub <- as.publisher(pub)
    publish(pub)
  }
}