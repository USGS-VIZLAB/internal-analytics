pretty_time <- function(time){
  days <- floor(time/60/60/24)
  hours <- floor(time/60/60) - 24*days
  minutes <- floor(time/60) - hours*60 - 24*days*60
  seconds <- floor(time) - hours*60*60 - minutes*60 - 24*days*60*60

  hours <- zeroPad(as.character(hours),2)
  minutes <- zeroPad(as.character(minutes),2)
  seconds <- zeroPad(as.character(seconds),2)

  df <- data.frame(Days = as.character(days),
                   Hours = hours,
                   Minutes = minutes,
                   Seconds = seconds,
                   stringsAsFactors = FALSE)
  return(df)

}

visualize.portfolio_ave_time <- function(viz = as.viz("portfolio_ave_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  viz.data <- deps[["viz_data"]]
  location <- viz[["location"]]
  ave_time_on_page <- mean(viz.data$avgSessionDuration, na.rm = TRUE)

  x <- pretty_time(ave_time_on_page)
  txt_return <- paste0(x[1,c("Hours","Minutes","Seconds")],collapse = ":")

  sink(location)
  cat(txt_return)
  sink()
  return(txt_return)
}

visualize.portfolio_total_time <- function(viz = as.viz("portfolio_total_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  viz.data <- deps[["viz_data"]]
  location <- viz[["location"]]

  total_time <- sum(viz.data$avgSessionDuration, na.rm = TRUE)

  x <- pretty_time(total_time)
  txt_return <- paste0(x[1,c("Hours","Minutes","Seconds")],collapse = ":")
  x$Days <- format(as.numeric(x$Days),big.mark=",",scientific=FALSE)

  txt_return <- paste(x$Days[1], "Days.", txt_return)

  sink(location)
  cat(txt_return)
  sink()

  return(txt_return)
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

visualize.app_ave_time <- function(viz = as.viz("app_ave_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  viz.data <- deps[["viz_data"]]
  text_type <- viz[["text_type"]]

  dir.create(file.path("cache", "publish"), showWarnings = FALSE)

  for(i in unique(viz.data$viewID)){

    location <- paste0("cache/publish/",i,"_",text_type,".txt")

    sub_data <- filter(viz.data, viewID == i)

    ave_time_on_page <- mean(sub_data$avgSessionDuration, na.rm = TRUE)

    df <- pretty_time(ave_time_on_page)
    row.names(df) <- c("Average")

    df$Days <- format(as.numeric(df$Days),big.mark=",",scientific=FALSE)
    txt_return <- paste0(df[1,c("Hours","Minutes","Seconds")],collapse = ":")

    sink(location)
      cat(txt_return)
    sink()

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = text_type,
                                 stringsAsFactors = FALSE))
  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}

visualize.app_total_time <- function(viz = as.viz("app_total_time_year")){
  library(dplyr)

  deps <- readDepends(viz)

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  viz.data <- deps[["viz_data"]]
  text_type <- viz[["text_type"]]

  dir.create(file.path("cache", "publish"), showWarnings = FALSE)

  for(i in unique(viz.data$viewID)){

    location <- paste0("cache/publish/",i,"_",text_type,".txt")

    sub_data <- filter(viz.data, viewID == i)

    total_time <- sum(sub_data$avgSessionDuration, na.rm = TRUE)

    df <- pretty_time(total_time)
    row.names(df) <- c("Total")

    df$Days <- format(as.numeric(df$Days),big.mark=",",scientific=FALSE)
    txt_return <- paste0(df[1,c("Hours","Minutes","Seconds")],collapse = ":")

    sink(location)
    cat(txt_return)
    sink()

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = text_type,
                                 stringsAsFactors = FALSE))
  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}
