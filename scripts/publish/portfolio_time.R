publish.portfolio_time <- function(viz = as.viz("portfolio_time_year")){
  library(dplyr)
  library(htmlTable)

  deps <- readDepends(viz)

  viz.data <- deps[["viz_data"]]
  type <- viz[["plottype"]]

  ave_time_on_page <- mean(viz.data$avgSessionDuration, na.rm = TRUE)
  total_time <- sum(viz.data$avgSessionDuration, na.rm = TRUE)

  x <- pretty_time(c(ave_time_on_page, total_time))
  row.names(x) <- c("Average", "Total")


  x$Days <- format(as.numeric(x$Days),big.mark=",",scientific=FALSE)

  return(htmlTable(x,
            rnames = row.names(x),
            col.rgroup = c("none", "#F7F7F7"),
            css.total = "border-top: 1px solid #BEBEBE; font-weight: 900; padding-right: 0.7em; padding-top: 0.7em; width=100%;",
            css.cell="padding-bottom: 0.5em; padding-right: 0.5em; padding-top: 0.5em;"))


}

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

visualize.app_time <- function(viz = as.viz("app_time_year")){
  library(dplyr)
  library(htmlTable)

  deps <- readDepends(viz)

  x <- data.frame(id = character(),
                  loc = character(),
                  type = character(),
                  stringsAsFactors = FALSE)

  plot_type <- viz[["plottype"]]

  viz.data <- deps[["viz_data"]]

  dir.create(file.path("cache", "publish"), showWarnings = FALSE)

  for(i in unique(viz.data$viewID)){

    location <- paste0("cache/publish/",i,"_",plot_type,".html")

    sub_data <- filter(viz.data, viewID == i)

    ave_time_on_page <- mean(sub_data$avgSessionDuration, na.rm = TRUE)

    total_time <- sum(sub_data$avgSessionDuration, na.rm = TRUE)

    df <- pretty_time(c(ave_time_on_page, total_time))
    row.names(df) <- c("Average", "Total")

    df$Days <- format(as.numeric(df$Days),big.mark=",",scientific=FALSE)

    sink(location)
      cat(htmlTable(df,
           rnames=row.names(df),
           col.rgroup = c("none", "#F7F7F7"),
           css.total = "border-top: 1px solid #BEBEBE; font-weight: 900; padding-right: 0.7em; padding-top: 0.7em; width=100%;",
           css.cell="padding-bottom: 0.5em; padding-right: 0.5em; padding-top: 0.5em;"))
    sink()

    x <- bind_rows(x, data.frame(id = i,
                                 loc = location,
                                 type = plot_type,
                                 stringsAsFactors = FALSE))
  }

  write.csv(x, file=viz[["location"]], row.names = FALSE)

}
