library(yaml)
library(dplyr)


#load yaml, convert to data frame
#commenting out for safety
#df <- do.call(what = bind_rows, args = yaml.load_file('data/gaTable.yaml'))

#remove cols
newTable <- select(new, -kind, -selfLink, -currency, -effective,
                      -created, -updated, -eCommerceTracking, -contains(".href"),
                      -contains('Query'), -contains('type'), -defaultPage) %>% 
                rename(viewID = id, viewName = name)
masterTable <- bind_rows(df, newTable)

#write dataframe to yaml
cat(as.yaml(masterTable, column.major = FALSE), file = "data/gaTable.yaml")
