#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {

  deps <- readDepends(viz)

  projects <- deps[['project_table']] # get projects from deps
  links <- deps[['project_links']] # ordered urls for projects

  img.files <- list(
    new_vs_returning_year = deps[['new_vs_returning_year']],
    new_vs_returning_month = deps[['new_vs_returning_month']],
    new_vs_returning_week = deps[['new_vs_returning_week']],
    year_line_sessions = deps[['viz_y_sessions']],
    month_line_sessions = deps[['viz_m_sessions']],
    week_line_sessions = deps[['viz_w_sessions']],
    viz_device_type_year = deps[['viz_device_type_year']],
    viz_device_type_month = deps[['viz_device_type_month']],
    viz_device_type_week = deps[['viz_device_type_week']],
    viz_source_year = deps[['viz_source_year']],
    viz_source_month = deps[['viz_source_month']],
    viz_source_week = deps[['viz_source_week']],
    viz_geo_apps_year = deps[["viz_geo_apps_year"]],
    viz_geo_apps_month = deps[["viz_geo_apps_month"]],
    viz_geo_apps_week = deps[["viz_geo_apps_week"]],
    timeDayUse_app_year = deps[["timeDayUse_app_year"]],
    timeDayUse_app_month = deps[["timeDayUse_app_month"]],
    timeDayUse_app_week = deps[["timeDayUse_app_week"]]
  )

  for (i in 1:nrow(projects)) {
    proj <- projects[i,]


    viewID <- proj$viewID
    # get relative paths for images

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

    flip.clock.files <- list(
      yearFlipClock = deps[['app_time_year']],
      monthFlipClock = deps[['app_time_month']],
      weekFlipClock = deps[['app_time_week']]
    )

    flip.clocks <- sapply(flip.clock.files, function(x){
      row <- filter(x, id == viewID)
      flip <- NULL
      if (nrow(row) > 0) {
        flip.data <- as.viz(list(
          location = row[['loc']],
          reader = "rds"
        ))
        flip <- list(
          publisher = "flipClock",
          depends = list(
            times = flip.data
          ),
          template = "flipClockTemplate"
        )
        flip <- as.viz(flip)
        flip <- as.publisher(flip)
        flip <- publish(flip)
      }
      return(flip)
    })

    j <- 1
    while (j <= length(links)) {
      if (links[[j]][["longName"]] == proj[["longName"]]) {
        break
      }
      j <- j + 1
    }
    prevLink <- NULL
    if (j>1) {
      prevLink <- links[[j-1]][['url']]
    }
    nextLink <- NULL
    if (j<nrow(projects)) {
      nextLink <- links[[j+1]][['url']]
    }

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
          project_contact = proj$projectContact,
          project_contact_email = proj$projectContactEmail,
          analytics_contact = proj$analyticsContact,
          analytics_contact_email = proj$analyticsContactEmail,
          new_vs_returning_year = proj.imgs[['new_vs_returning_year']],
          new_vs_returning_month = proj.imgs[['new_vs_returning_month']],
          new_vs_returning_week = proj.imgs[['new_vs_returning_week']],
          year_line_sessions = proj.imgs[['year_line_sessions']],
          month_line_sessions = proj.imgs[['month_line_sessions']],
          week_line_sessions = proj.imgs[['week_line_sessions']],
          viz_device_type_year = proj.imgs[['viz_device_type_year']],
          viz_device_type_month = proj.imgs[['viz_device_type_month']],
          viz_device_type_week = proj.imgs[['viz_device_type_week']],
          viz_source_year = proj.imgs[['viz_source_year']],
          viz_source_month = proj.imgs[['viz_source_month']],
          viz_source_week = proj.imgs[['viz_source_week']],
          viz_geo_apps_year = proj.imgs[["viz_geo_apps_year"]],
          viz_geo_apps_month = proj.imgs[["viz_geo_apps_month"]],
          viz_geo_apps_week = proj.imgs[["viz_geo_apps_week"]],
          timeDayUse_app_year = proj.imgs[["timeDayUse_app_year"]],
          timeDayUse_app_month = proj.imgs[["timeDayUse_app_month"]],
          timeDayUse_app_week = proj.imgs[["timeDayUse_app_week"]],
          yearFlipClock = flip.clocks[["yearFlipClock"]],
          monthFlipClock = flip.clocks[["monthFlipClock"]],
          weekFlipClock = flip.clocks[["weekFlipClock"]],
          previous_link = prevLink,
          next_link = nextLink
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
