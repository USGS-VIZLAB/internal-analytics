library(yaml)
library(dplyr)
library(googleAnalyticsR)
library(googleAuthR)

#load yaml, convert to data frame
#commenting out for safety
df <- do.call(what = bind_rows, args = yaml.load_file('data/gaTable.yaml'))

gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')

#get new rows to add out of account list
new <- accSub
newViewDF <- data.frame()
for(i in 1:nrow(new)){
  row <- ga_view(accountId = new$accountId[i], webPropertyId = new$webPropertyId[i],
                 profileId = new$viewId[i])
  newViewDF <- bind_rows(newViewDF, data.frame(row, stringsAsFactors = FALSE)) 
}
newViewDF <- rename(newViewDF, viewId = id) %>% 
            mutate(botFilteringEnabled = as.character(botFilteringEnabled))
newViewDF <- left_join(newViewDF, select(new, viewId, accountName), by = "viewId")

#remove cols
newTable <- select(newViewDF, -kind, -selfLink, -currency, -effective,
                      -created, -updated, -eCommerceTracking, 
                   -matches('.href|Query|type')) %>% 
                rename(viewID = viewId, viewName = name) 
masterTable <- bind_rows(df, newTable)

#write dataframe to yaml, with newlines between sections
cat(gsub(x = as.yaml(masterTable, column.major = FALSE, indent = 2), 
         pattern = "- longName", replacement = "\n- longName"),
    file = "data/gaTable.yaml")
