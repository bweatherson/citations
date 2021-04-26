require(tidyverse)
require(tidygraph)
require(ggraph)

load("master_bib.RData")
load("master_cite.RData")


###Getting a list of journals
journal_list <- master_cite %>%
  left_join(master_bib, by = c("refs" = "id")) %>%
  group_by(journal) %>%
  tally() %>%
  arrange(-n)
###


cited_years <- 1975:2005
citing_years <- 1975:2005
citing_journals <- journal_list$journal
# citing_journals <- journal_list$journal[2:28]
# 
# x <- 2:22
# x <- x[!x %in% c(8, 10, 20)]
# citing_journals <- journal_list$journal[x]

  
# citing_journals <- c("Journal Of Philosophy",
#                      "Mind",
#                      "Philosophical Review",
#                      "British Journal For The Philosophy Of Science",
#                      "Nous",
#                      "Philosophy And Phenomenological Research",
#                      "Philosophical Quarterly",
#                      "Philosophy Of Science",
#                      "Analysis",
#                      "Ethics",
#                      "Proceedings Of The Aristotelian Society",
#                      "Philosophy & Public Affairs"
# )
label_min_pairs <- 1
label_min_cites <- 40

all_cites <- master_bib %>%
  select(id, firstauth, shortcite, longcite, journal, year) %>%
  filter(year %in% cited_years | year %in% citing_years)

cited_articles <- (all_cites %>% filter(year %in% cited_years))$id
citing_articles <- (all_cites %>% filter(year %in% citing_years))$id

good_cite <- master_cite %>%
  filter(id %in% citing_articles) %>%
  filter(refs %in% cited_articles) %>%
  left_join(all_cites, by = "id") %>%
  left_join(all_cites, by = c("refs" = "id"), suffix = c(".new", ".old")) %>%
  filter(journal.new %in% citing_journals) %>%
  rename(new = id, old = refs)

# This is to get rid of articles that only cite one article
# The find all pairs code is dumb and breaks if there are no pairs
cite_count <- good_cite %>%
  group_by(new) %>%
  tally() %>%
  filter(n > 1)

all_pairs <- good_cite %>%
  filter(new %in% cite_count$new) %>%
  group_by(new) %>%
  arrange(old) %>%
  do(data.frame(t(combn(.$old, 2))))

# Filter out the pairs that only occur once 
# And filter out pairs by same author, which are boring
top_pairs <- all_pairs %>%
  ungroup() %>%
  group_by(X1, X2) %>%
  tally() %>%
  arrange(-n) %>%
  filter(n > 5) %>%
  left_join(all_cites, by = c("X1" = "id")) %>%
  left_join(all_cites, by = c("X2" = "id")) %>%
  filter(firstauth.x != firstauth.y) %>%
  select(-firstauth.x, -firstauth.y)

top_cites <- good_cite %>%
  group_by(old, longcite.old) %>%
  tally() %>%
  filter(n > 1) %>%
  arrange(-n) %>%
  rename(cites = n)

# Now create list of nodes that we're going to use
article_list <- c(top_pairs$X1, top_pairs$X2) %>%
  as_tibble() %>%
  rename(code = value) %>%
  group_by(code) %>%
  tally() 

# Now create list of things we're going to ignore
# We aren't deleting them yet, but if both members of a pair are ignorable, then delete
ignore_articles <- article_list %>%
  filter(n == 1)

# Your label is your shortcite if you have enough cites and enough pairs
article_labels <- article_list %>%
  left_join(master_bib, by = c("code" = "id")) %>%
  select(code, n, shortcite, longcite) %>%
  left_join(top_cites, by = c("code" = "old")) %>%
# This is the code for having a minimum number of pairs
# Trying for now to label things with lots of cites or terminal nodes
#  mutate(shortcite = case_when(
#    (n < label_min_pairs) | (cites < label_min_cites) | (is.na(cites)) ~ "",
#    TRUE ~ shortcite
#  )) %>%
# This is the code for labelling high cited articles and terminal nodes
  mutate(shortcite = case_when(
    (n == 1 ) | (cites >= label_min_cites) ~ shortcite,
    TRUE ~ ""
  )) %>%
  select(code, shortcite)

# Start on edge list
# Filter out where both X1 and X2 are bad

edges <- top_pairs %>%
  filter(!X1 %in% ignore_articles$code | !X2 %in% ignore_articles$code) 

# Third create nodes list. 
# Filter out articles we won't use 
# Should have each id number, plus an id column. 
# Note we've changed the WOS numbers to 'code', and called the new column 'id'
# Trying to follow https://www.jessesadler.com/post/network-analysis-with-r/ as closely as possible for now

nodes <- all_cites %>%
  filter(id %in% edges$X1 | id %in% edges$X2) %>%
  select(code=id) %>%
  rowid_to_column("id") %>%
  left_join(article_labels, by = "code") %>%
  select(id, code, shortcite)

# Now create edges
# Take the pair list, replace the WOS codes with ids, and delete guff

edges <- edges %>%
  left_join(nodes, by = c("X1" = "code")) %>%
  select(from = id, everything()) %>%
  left_join(nodes, by = c("X2" = "code")) %>%
  ungroup() %>%
  select(from, to = id, weight = n)
  
cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

g <- ggraph(cites_graph, layout = "nicely") + 
  geom_edge_link(width = 0.6,
                 aes(color = as.factor(weight))) + 
  geom_node_point(size = 0.6, alpha = 0.5) + 
  geom_node_text(aes(label = shortcite), 
                 repel = TRUE,
                 size = 1.5) +
  labs(edge_color = "Number of co-citations") +
  theme_graph()

g + labs(caption = "Co-citations between 1975 and 1995 of articles written between 1975 and 1995")

ggsave("all_1975_2005.png", scale = 5, limitsize = FALSE)