pretty_time <- function(time){
  years <- floor(time/60/60/24/365)
  days <- floor(time/60/60/24) - 365*years
  hours <- floor(time/60/60) - 24*(365*years + days)
  minutes <- floor(time/60) - 60*(hours + 24*(365*years + days))
  seconds <- floor(time) - 60*(minutes + 60*(hours + 24*(365*years + days)))

  hours <- zeroPad(as.character(hours),2)
  minutes <- zeroPad(as.character(minutes),2)
  seconds <- zeroPad(as.character(seconds),2)
  years <- format(as.numeric(years),big.mark=",",scientific=FALSE)

  df <- data.frame(Years = years,
                   Days = as.character(days),
                   Hours = hours,
                   Minutes = minutes,
                   Seconds = seconds,
                   stringsAsFactors = FALSE)
  return(df)

}

zeroPad <- function(x,padTo){
  if(padTo <= 1) return(x)

  numDigits <- nchar(x, keepNA = TRUE)
  padding <- padTo-numDigits

  if(any(is.na(padding))){
    padding[is.na(padding)] <- 0
  }

  paddingZeros <- vapply(
    X = padding[padding > 0],
    FUN = function(y) paste0(rep("0",y),collapse=""),
    FUN.VALUE = ""
  )

  x[padding > 0] <- paste0(paddingZeros,x[padding > 0])
  return(x)
}

process.portfolio_time <- function(viz = as.viz("portfolio_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  viz.data <- deps[["viz_data"]]
  location <- viz[["location"]]

  ave_time_on_page <- mean(viz.data$avgSessionDuration, na.rm = TRUE)
  total_time <- sum(viz.data$avgSessionDuration, na.rm = TRUE)

  x <- pretty_time(c(ave_time_on_page, total_time))

  if(as.numeric(x$Days[1]) > 0){
    x$Hours[1] <- zeroPad(as.character(as.numeric(x$Hours[1]) +
                                         24*as.numeric(x$Days[1])))
  }

  ret_list <- list()
  ret_list[["total"]] <- x[2,]
  ret_list[["ave"]] <- x[1,]

  saveRDS(ret_list, file = location)
}


process.app_time <- function(viz = as.viz("app_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  app.times <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  viz.data <- deps[["viz_data"]]
  text_type <- viz[["text_type"]]

  dir.create(file.path("cache", "process"), showWarnings = FALSE)

  for(i in unique(viz.data$viewID)){

    location <- paste0("cache/process/",i,"_",text_type,".rds")

    sub_data <- filter(viz.data, viewID == i)

    ave_time_on_page <- mean(sub_data$avgSessionDuration, na.rm = TRUE)
    total_time <- sum(sub_data$avgSessionDuration, na.rm = TRUE)

    x <- pretty_time(c(ave_time_on_page, total_time))

    if(as.numeric(x$Days[1]) > 0){
      x$Hours[1] <- zeroPad(as.character(as.numeric(x$Hours[1]) +
                                           24*as.numeric(x$Days[1])))
    }

    ret_list <- list()
    ret_list[["total"]] <- x[2,]
    ret_list[["ave"]] <- x[1,]

    saveRDS(ret_list, file = location)

    app.times <- bind_rows(app.times, data.frame(id = i,
                                 loc = location,
                                 type = text_type,
                                 stringsAsFactors = FALSE))
  }

  write.csv(app.times, file=viz[["location"]], row.names = FALSE)

}

