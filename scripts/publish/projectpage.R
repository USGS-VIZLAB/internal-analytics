#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {
  
  deps <- readDepends(viz)
  
  projects <- deps[['project_table']] # get projects from deps
  
  img.files <- list(
    new_vs_returning_year = deps[['new_vs_returning_year']],
    year_line_sessions = deps[['viz_y_sessions']],
    month_line_sessions = deps[['viz_m_sessions']],
    week_line_sessions = deps[['viz_w_sessions']],
    viz_device_type_year = deps[['viz_device_type_year']],
    viz_source_year = deps[['viz_source_year']],
    viz_geo_apps_year = deps[["viz_geo_apps_year"]],
    timeDayUse_app_year = deps[["timeDayUse_app_year"]]
  )
  
  table.files <- deps[["app_time"]]
  
  for (i in 1:nrow(projects)) {
    proj <- projects[i,]
    viewID <- proj$viewID
    # get relative paths for images
    
    table.data <- filter(table.files, id == viewID)
    table.html <- readChar(table.data$loc, file.info(table.data$loc)$size)
    
    proj.imgs <- sapply(img.files, function(x){
      img <- "missingImg"
      
      row <- filter(x, id == viewID)
      if (nrow(row) > 0) {
        img <- list(
          location = row[['loc']],
          mimetype = "image/png",
          alttext = row[['type']],
          title = row[['type']]
        )
      }
      img <- as.viz(img)
      img <- as.publisher(img)
      img.out <- publish(img)

      return(img.out)
    })
    
    sectionId <- paste0(viewID, "-section")
    contents <- list(
      id = sectionId,
      publisher = "section",
      template = viz[['template']],
      depends = viz[['depends']],
      context = c(
        viz[['context']],
        list(
          project_name = proj$longName,
          project_description = proj$description,
          project_URL = proj$websiteUrl,
          new_vs_returning_year = proj.imgs[['new_vs_returning_year']],
          year_line_sessions = proj.imgs[['year_line_sessions']],
          month_line_sessions = proj.imgs[['month_line_sessions']],
          week_line_sessions = proj.imgs[['week_line_sessions']],
          viz_device_type_year = proj.imgs[['viz_device_type_year']],
          viz_source_year = proj.imgs[['viz_source_year']],
          viz_geo_apps_year = proj.imgs[["viz_geo_apps_year"]],
          timeDayUse_app_year = proj.imgs[["timeDayUse_app_year"]],
          app_time = table.html
      ))
    )
    contents <- as.viz(contents)
    contents <- as.publisher(contents)
    
    depends <- viz[['depends']]
    depends[[sectionId]] <- contents
    
    pub <- list(
      id = paste0(viewID, "-page"),
      name = proj$shortName,
      publisher = "page",
      template = "fullpage",
      depends = depends,
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

publish.projectlist <- function(viz = as.viz("project_list")) {
  required <- c("template")
  checkRequired(viz, required)
  
  deps <- readDepends(viz)
  
  template <- template(viz[['template']])
  context <- list(projects = deps[['project_links']])
  viz[['output']] <- render(template, context)
  return(viz[['output']])
}
