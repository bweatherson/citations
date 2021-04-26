require(tidyverse)

jphil_total <- c()

load("~/Documents/citations/j_phil_backups/jphil_7187-1619a.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_16-19b.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_11-15a.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_11-15b.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_8797.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_06-10.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_01-05.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

load("~/Documents/citations/j_phil_backups/jphil_98-00.RData")
jphil_total <- bind_rows(jphil_total, master_refs_fixed)

jphil_total <- jphil_total %>%
  select(1:5) %>%
  mutate(newid = str_extract(uid, ".+(?=\\.)")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, "\\. ","")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, "\\.","")) %>%
  mutate(citedAuthor = str_replace_all(citedAuthor, ",",""))

jphil_total <- jphil_total %>%
  mutate(thename = strsplit(citedAuthor, " ")) %>%
  mutate(citedAuthor = paste0(thename[2], " ", thename[1])) %>%
  select(1:6) %>%
  mutate(citedAuthor = str_to_title(citedAuthor))


name_corrected <- read_csv("jphilauthors.csv")

jphil_total <- full_join(jphil_total, name_corrected, by = "citedAuthor") 

# temp <- jphil_total %>%
#   mutate(shortcite = str_extract(authname, "(?=\\s).*")) %>%
#   mutate(shortcite = str_replace(shortcite, "\\s", "")) %>%
#   mutate(shortcite = paste0(shortcite, " ", year)) %>%
#   arrange(shortcite) %>%
#   ungroup() %>%
#   rowid_to_column("newcode") %>%
#   mutate(newcode = as.character(newcode)) %>%
#   mutate(newcode = str_pad(newcode, width = 4, side = "left", pad = "0")) %>%
#   mutate(newcode = paste0("jphil",newcode))
  

jphil_bib <- jphil_total %>%
  mutate(shortcite = str_extract(authname, "(?=\\s).*")) %>%
  mutate(shortcite = str_replace(shortcite, "\\s", "")) %>%
  mutate(shortcite = paste0(shortcite, " ", year)) %>%
  arrange(shortcite) %>%
  distinct(shortcite, .keep_all = TRUE) %>%
  select(shortcite, authname, year) %>%
  rowid_to_column("newcode") %>%
  mutate(newcode = as.character(newcode)) %>%
  mutate(newcode = str_pad(newcode, width = 4, side = "left", pad = "0")) %>%
  mutate(newcode = paste0("jphil",newcode))

jphil_cites <- jphil_total %>%
  mutate(shortcite = str_extract(authname, "(?=\\s).*")) %>%
  mutate(shortcite = str_replace(shortcite, "\\s", "")) %>%
  mutate(shortcite = paste0(shortcite, " ", year)) %>%
  left_join(jphil_bib, by = "shortcite") %>%
  select(id = newid, refs = newcode)

save(jphil_bib, file = "jphil_bib.RData")
save(jphil_cites, file = "jphil_cites.RData")

