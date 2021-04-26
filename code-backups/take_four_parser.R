require(tidyverse)
require(xml2)

rm(list = ls())

filelist <- list.files(pattern = "xml")
todolist <- filelist %>% str_replace(".xml","")


extract_info <- function (x){
  enframe(x) %>% pivot_wider(names_from = name, values_from = value, values_fn = list)
}


for (todoname in todolist){

time_01 <- Sys.time()

stage_01 <- read_xml(paste0(todoname,".xml"))

stage_02 <- as_list(stage_01)

#save(stage_02, file = "stage_02.RData")

stage_03 <- as_tibble(stage_02)
rm(stage_02)

#save(stage_03, file = "stage_03.RData")

stage_04 <- stage_03 %>%
  mutate(info = lapply(records, unlist))
rm(stage_03)

#save(stage_04, file = "stage_04.RData")

stage_05 <- as_tibble(stage_04)
rm(stage_04)

#save(stage_05, file = "stage_05.RData")

stage_06 <- lapply(stage_05$info, extract_info) %>% 
  bind_rows() %>%
  select(id = UID, 
         source = static_data.summary.titles.title, 
         author = static_data.summary.names.name.display_name,
         type = static_data.summary.doctypes.doctype,
         pages = static_data.summary.pub_info.page,
         refs = static_data.fullrecord_metadata.references.reference.uid,
         subject = static_data.fullrecord_metadata.category_info.subjects.subject,
         bib = static_data.item.bib_id)

#save(stage_06, file = "stage_06.RData")
rm(stage_05)

stage_07 <- stage_06 %>%
  rowwise() %>%
  filter("Philosophy" %in% subject | "History & Philosophy Of Science" %in% subject)
rm(stage_06)

authadjust <- function(x){
   paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
 }

stage_08 <- stage_07 %>%
  mutate(journal = source[1]) %>%
  mutate(art_title = source[length(source)]) %>%
  mutate(type = type[1]) %>%
  filter(type == "Article") %>%
  mutate(journal = str_to_title(journal),
         art_title = str_to_title(art_title)) %>%
  rowwise() %>%
  mutate(auth = case_when(length(author) == 1 ~ authadjust(author[1]),
                          length(author) == 2 ~ paste0(authadjust(author[1]), " and ", authadjust(author[2])),
                          TRUE ~ paste0(authadjust(author[1]), ", ", authadjust(author[2]), ", et al")
  )) %>%
  mutate(longcite = paste(
    auth, art_title, journal, bib, sep = ", "
  )) %>%
  mutate(year = as.numeric(str_sub(bib, -4))) %>%
  mutate(shortcite = paste(
    str_to_title(str_extract(author[1],".+(?=,)")),
    year,
    sep = " "
  ))

time_08 <- Sys.time()
print(paste(todoname, time_08 - time_01))
#save(stage_07, file = "stage_07.RData")

save(stage_08, file = paste0(todoname,".RData"))
}