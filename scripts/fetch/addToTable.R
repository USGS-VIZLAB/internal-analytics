library(yaml)
library(dplyr)
#write dataframe to yaml
cat(as.yaml(masterTable, column.major = FALSE), file = "data/gaTable.yaml")

#load yaml, convert to data frame
df <- do.call(what = bind_rows, args = yaml.load_file('data/gaTable.yaml'))