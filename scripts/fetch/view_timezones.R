library(googleAnalyticsR)
library(dplyr)

allViews <- ga_account_list()
allViewInfo <- data.frame()
for(i in 1:nrow(allViews)){
  viewInfo <- ga_view(allViews$accountId[i], allViews$webPropertyId[i],
          allViews$viewId[i])
  allViewInfo <- bind_rows(allViewInfo, as.data.frame(viewInfo))
}