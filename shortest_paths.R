require(tidyverse)
require(tidygraph)
require(ggraph)
require(igraph)

load("master_bib.RData")
load("master_cite.RData")

all_cites <- master_bib %>%
  select(id, firstauth, shortcite, longcite, journal, year) %>%
  filter(year > 1974)

cited_articles <- all_cites$id
citing_articles <- all_cites$id

good_cite <- master_cite %>%
  filter(id %in% citing_articles) %>%
  filter(refs %in% cited_articles) %>%
  left_join(all_cites, by = "id") %>%
  left_join(all_cites, by = c("refs" = "id"), suffix = c(".new", ".old")) %>%
  rename(new = id, old = refs)

cite_count <- good_cite %>%
  group_by(new) %>%
  tally() %>%
  arrange(-n)