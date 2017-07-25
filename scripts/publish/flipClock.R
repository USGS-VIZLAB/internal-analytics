publish.flipClock <- function(viz) {

  times <- readDepends(viz)[['times']]
  # start with total
  average <- times[['ave']]
  total <- times[['total']]

  context <- getSignificantTime(average, 2)
  totalText <- getSignificantTime(total, 3)

  context[['totalTime']] <- paste(
    fixPlural(paste0(totalText[['flipGroup1']], collapse = ""),
    totalText[['timeLabel1']]),
    fixPlural(paste0(totalText[['flipGroup2']], collapse = ""),
    totalText[['timeLabel2']]),
    fixPlural(paste0(totalText[['flipGroup3']], collapse = ""),
    totalText[['timeLabel3']])
  )
  template <- template(viz[['template']])
  output <- render(template, context)
  return(output)
}

breakCharacter <- function(character) {
  lapply(character, function(x) {as.list(unlist(strsplit(x, "[.]?")))})
}

getSignificantTime <- function(times, num.groups) {
  context <- list()
  i <- 1
  while (i <= length(times) && as.numeric(times[i]) == 0) {
    i <- i + 1
  }
  offset <- min(i-1, (length(times)) - num.groups)
  for (j in 1:num.groups) {
    context[[paste0("timeLabel", j)]] <- names(times)[j+offset]
    context[[paste0("flipGroup", j)]] <- breakCharacter(unname(times[j+offset]))[[1]]
  }
  return(context)
}

fixPlural <- function(number, text) {
  if(as.numeric(number) == 1) {
    text <- sub("[s]$", "", text)
  }
  paste(number, text)
}
