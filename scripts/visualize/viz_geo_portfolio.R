visualize.viz_geo_portfolio <- function(viz=as.viz("viz_geo_portfolio")){
  library(dplyr)
  library(maptools)
  library(maps)
  library(sp)
  library(ggplot2)
  
  viz.data <- readDepends(viz)[["geo_apps"]]
  height = viz[["height"]]
  width = viz[["width"]]
  
  proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  
  to_sp <- function(...){
    map <- maps::map(..., fill=TRUE, plot = FALSE)
    IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
    map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
    map.sp.t <- spTransform(map.sp, CRS(proj.string))
    return(map.sp.t)
  }
  
  shift_sp <- function(sp, scale, shift, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
    orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
    scale <- max(apply(bbox(ref), 1, diff)) * scale
    obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
    ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
    obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
    ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
    new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
    obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
    if (is.null(proj.string)){
      proj4string(obj) <- proj4string(sp)
    } else {
      proj4string(obj) <- proj.string
    }
    
    if (!is.null(row.names)){
      row.names(obj) <- row.names
    }
    return(obj)
  }
  
  
  conus <- to_sp('state')
  
  # thanks to Bob Rudis (hrbrmstr):
  # https://github.com/hrbrmstr/rd3albers
  
  # -- if moving any more states, do it here: --
  move_variables <- list(
    alaska = list(scale=0.33, shift = c(80,-450), rotate=-50),
    hawaii = list(scale=1, shift=c(520, -110), rotate=-35)
    # PR = list(scale=2.5, shift = c(-140, 90), rotate=20)
  )
  
  stuff_to_move <- list(
    alaska = to_sp("world", "USA:alaska"),
    hawaii = to_sp("world", "USA:hawaii")
    # PR = to_sp("world", "Puerto Rico")
  )
  
  states.out <- conus
  
  wgs84 <- "+init=epsg:4326"

  
  for(i in names(move_variables)){
    shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                   move_variables[[i]],  
                                   proj.string = proj4string(conus),
                                   row.names = i))
    states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
    
  }

  region_summary <- data.frame(table(viz.data$region), stringsAsFactors = FALSE)  %>%
    arrange(desc(Freq)) %>%
    mutate(region = tolower(Var1)) %>%
    select(-Var1) %>%
    right_join(data.frame(region = names(states.out), stringsAsFactors = FALSE), by="region") 
  
  if(nrow(region_summary) > 0){

    sp_fill <- SpatialPolygonsDataFrame(states.out,
                                        data=data.frame(row.names=region_summary$region,
                                                        fill_col = region_summary$Freq))
    sf.points <- fortify(states.out, region="region")
    sf.points <- left_join(sf.points, region_summary, by=c("id"="region"))

    gsMap <- ggplot(sf.points,aes(x=long, y=lat, fill=Freq)) + 
      coord_equal() +
      geom_polygon(colour="white", size=0.1, alpha = 0.75,
                   aes(group=group)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position="bottom",
            legend.title = element_blank(),
            legend.key.size = unit(0.25, "cm"),
            legend.key.width = unit(2, "cm")) +
      scale_fill_gradient(na.value = 'transparent',
                          low = "white", high = "steelblue")
    
    
  } else {
    gsMap <- ggplot(states.out,aes(x=long, y=lat)) + 
      coord_equal() +
      geom_polygon(colour="white", size=0.1, alpha = 0.75,
                   aes(group=group)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  }
  
  ggsave(gsMap, filename = viz[["location"]], height = height, width = width)
  
}
