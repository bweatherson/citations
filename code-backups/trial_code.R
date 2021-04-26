require(tidyverse)
require(xml2)
require(XML)
tt <- read_xml("1957-002.xml")

xml_find_all(tt, "/records/rec/static_data/summary/titles/title") %>% xml_text()

ttt <- read_xml("sample-article.xml")


tlist <-  as_list(ttt)

ttibble <- tibble::as_tibble(tlist) %>%
  # new tidyr function
  unnest_wider(article, names_repair = "universal") %>%
  unnest_wider(journal.meta, names_repair = "universal") %>%
  
  unnest_wider(journal.title.group, names_repair = "universal") %>%
  unnest_wider(journal.title, names_repair = "universal")

unnest_all <- function( .df )
{
  lc <- purrr::keep(.df, is.list) %>% names
  if( length(lc) == 0 ) return(.df)
  tidyr::unnest( .df, lc ) %>% unnest_all()
}

  
tibble::as_tibble(tlist) %>% unnest(cols = c(article))
tparse <- xmlParse(tt)


cite_list <- as_list(cites) # This is the really slow part
cite_tibble <- as_tibble(cite_list)
cite_tibble_wide <- unnest_wider(cite_tibble, records, names_repair = "universal")

temp_cites <- cite_tibble_wide %>% slice(1:1000)
cite_tibble_wider <- cite_tibble_wide %>%
  mutate(id = unlist(UID)) %>%
  select(-UID, -dynamic_data) 


df <- lapply(cite_tibble_wider, unnest_wider)


## From here is what works


start_time <- Sys.time()
cites <- read_xml("1957-002.xml")

cite_list <- as_list(cites) # This is the really slow part

mid_time <- Sys.time()

mid_time - start_time

cite_tibble <- as_tibble(cite_list)

end_time <- Sys.time()

end_time - mid_time
end_time - start_time

df <- cite_tibble %>%
  mutate(info = lapply(records, unlist))

df <- as_tibble(df)


extract_info <- function (x){
  enframe(x) %>% pivot_wider(names_from = name, values_from = value)
}

df_info <- lapply(df$info, extract_info) %>% 
  bind_rows() %>%
  select(id = UID, 
        source = static_data.summary.titles.title, 
        author = static_data.summary.names.name.display_name,
        type = static_data.summary.doctypes.doctype,
        pages = static_data.summary.pub_info.page,
        refs = static_data.fullrecord_metadata.references.reference.uid)

df_ethics <- df_info %>%
  rowwise() %>%
  filter("ETHICS" %in% source)

## From here is what works

cites_57 <- read_xml("1957-001.xml")

cite_list_57 <- as_list(cites_57) # This is the really slow part - takes about 20 minutes to analyse a 500MB file
cite_tibble_57 <- as_tibble(cite_list_57)

df_57 <- cite_tibble_57 %>%
  mutate(info = lapply(records, unlist))

df_57 <- as_tibble(df_57)


extract_info <- function (x){
  enframe(x) %>% pivot_wider(names_from = name, values_from = value)
}

# This step takes about 10 minutes

df_57_info <- lapply(df_57$info, extract_info) %>% 
  bind_rows() %>%
  select(id = UID, 
         source = static_data.summary.titles.title, 
         author = static_data.summary.names.name.display_name,
         type = static_data.summary.doctypes.doctype,
         pages = static_data.summary.pub_info.page,
         refs = static_data.fullrecord_metadata.references.reference.uid,
         subject = static_data.fullrecord_metadata.category_info.subjects.subject,
         bib = static_data.item.bib_id)

df_philosophy <- df_57_info %>%
  rowwise() %>%
  filter("Philosophy" %in% subject | "History & Philosophy Of Science" %in% subject)

philsci <- df_57_info %>%
  rowwise() %>%
  filter('PHILOSOPHY OF SCIENCE' %in% source)
View(philsci)

df_philosophy <- df_philosophy %>%
  rowwise() %>%
  mutate(journal = source[1]) %>%
  mutate(art_title = source[length(source)]) %>%
  filter(type == "Article")

journal_list <- df_57_info %>%
  rowwise() %>%
  mutate(journal = source[1]) %>%
  ungroup() %>%
  group_by(journal) %>%
  tally()