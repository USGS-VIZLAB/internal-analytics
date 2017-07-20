publish.flipClock <- function(viz) {
  num.groups <- 3

  times <- readDepends(viz)[['times']]
  # start with total
  context <- list()
  total <- times[['total']]
  i <- 1
  while (i <= length(total) && as.numeric(total[i]) == 0) {
    i <- i + 1
  }
  offset <- min(i-1, (length(total)) - num.groups)
  for (j in 1:num.groups) {
    context[[paste0("timeLabel", j)]] <- names(total)[j+offset]
    context[[paste0("flipGroup", j)]] <- breakCharacter(unname(total[j+offset]))[[1]]
  }
  template <- template(viz[['template']])
  output <- render(template, context)
  return(output)
}

breakCharacter <- function(character) {
  lapply(character, function(x) {as.list(unlist(strsplit(x, "[.]?")))})
}
