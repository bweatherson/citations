require(dplyr)
require(jsonlite)

the_json <- toJSON(category_dt)

save(the_json, file = "small_list.json")