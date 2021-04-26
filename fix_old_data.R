authadjust <- function(x){
  paste0(str_extract(x, '\\b[^,]+$'), " ", str_to_title(str_extract(x,".+(?=,)")))
}

worklist <- list.files(pattern="RData")

for (f in worklist[1:55]){
  load(f)
  stage_08 <- stage_08 %>%
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
save(stage_08, file = f)
}