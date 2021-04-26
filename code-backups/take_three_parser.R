require(xml2)
require(tidyverse)
xx=read_xml("1957-002.xml", options = "NSCLEAN")     

system.time(xxx <- xx %>% xml_ns_strip())

xxx <- xml_ns(xx)

xy=(xml_find_all(xx, "//d1:REC"))
system.time(
  xz <- xml_find_all(xy, ".//descendant::*[not(*)]"))
system.time(xa <- xml_text(xz))
head(data.frame(t(matrix(xa, 5))))