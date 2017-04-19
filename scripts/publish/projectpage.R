#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {
  
  deps <- readDepends(viz)
  
  projects <- deps[['project_table']][['viewID']] # get projects from deps
  
  img.files <- list(
    month_sessions = deps[['viz_month_sessions']],
    year_line_sessions = deps[['viz_y_sessions']],
    month_line_sessions = deps[['viz_m_sessions']],
    week_line_sessions = deps[['viz_w_sessions']],
    day_line_sessions = deps[['viz_d_sessions']],
    device_type = deps[['viz_device_type']]
  )
  
  for (proj in projects) {
    # get relative paths for images
    proj.imgs <- sapply(img.files, function(x){
      row <- filter(x, id == proj)
      img.out <- "missing"
      if (nrow(row) > 0) {
        img <- list(
          location = row[['loc']],
          mimetype = "image/png",
          alttext = row[['type']],
          title = row[['type']]
        )
        img <- as.viz(img)
        img <- as.publisher(img)
        img.out <- publish(img)
      }
      return(img.out)
    })
    
    sectionId <- paste0(proj, "-section")
    contents <- list(
      id = sectionId,
      publisher = "section",
      template = viz[['template']],
      context = list(
        monthly_users_chart = proj.imgs[['month_sessions']],
        year_line_sessions = proj.imgs[['year_line_sessions']],
        month_line_sessions = proj.imgs[['month_line_sessions']],
        week_line_sessions = proj.imgs[['week_line_sessions']],
        day_line_sessions = proj.imgs[['day_line_sessions']],
        device_type = proj.imgs[['device_type']]
      )
    )
    contents <- as.viz(contents)
    contents <- as.publisher(contents)
    viz[['depends']][[sectionId]] <- contents
    
    pub <- list(
      id = paste0(proj, "-page"),
      name = proj,
      publisher = "page",
      template = "fullpage",
      depends = viz[['depends']],
      context = list(
        header = viz[['context']][['header']],
        footer = viz[['context']][['footer']],
        resources = viz[['context']][['resources']],
        sections = sectionId
      )
    )
    
    
    pub <- as.viz(pub)
    pub <- as.publisher(pub)
    publish(pub)
  }
}