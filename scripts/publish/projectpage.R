#' Creates templated html for each project
publish.projectpage <- function(viz) {
  deps <- readDepends(viz)
  projects <- c() # get projects from deps
  for (proj in projects) {
    # get relative paths for images
    proj.imgs <- list()
    for (type in proj[['images']]) {
      img <- list(
        location = proj[['images']][[type]][['imageLoc']],
        mimetype = "image/png"
      )
      img <- as.viz(img)
      img <- as.publisher(img)
      proj.imgs[[type]] <- publish(img)
    }
    
    pub <- list(
      id = paste0(proj, "-page"),
      publisher = page,
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