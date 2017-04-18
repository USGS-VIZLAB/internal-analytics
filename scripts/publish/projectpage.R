#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {
  deps <- readDepends(viz)
  projects <- deps[['project_table']][['viewID']] # get projects from deps
  img.files <- list(
    month_sessions = deps[['viz_month_sessions']],
    year_line_sessions = deps[['viz_y_sessions']]
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
      publisher = "page",
      template = viz[['template']],
      depends = viz[['depends']],
      context = list(
        header = viz[['context']][['header']],
        footer = viz[['context']][['footer']],
        monthly_users_chart = proj.imgs[['month_sessions']],
        year_line_sessions = proj.imgs[['year_line_sessions']]
      )
    )
    pub <- as.viz(pub)
    pub <- as.publisher(pub)
    publish(pub)
  }
}