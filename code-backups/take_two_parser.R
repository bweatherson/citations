require(XML)
require(data.table)
require(tidyverse)

doc <- xmlParse("1957-002.xml")


d = xmlRoot(doc)
size = xmlSize(d)

start_time <- Sys.time()

names = NULL
for(i in 1:size){
  v = getChildrenStrings(d[[i]])
  names = unique(c(names, names(v)))
}

mid_time <- Sys.time()

mid_time - start_time

for(i in 1:size){
  v = getChildrenStrings(d[[i]])
  cat(paste(v[names], collapse=","), "\n", file="a.csv", append=TRUE)
}

end_time <- Sys.time()

end_time - mid_time
end_time - start_time

read_csv("a.csv")


