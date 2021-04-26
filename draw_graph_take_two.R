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


cited_years <- 1975:1996
citing_years <- 1975:1996
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
#label_min_pairs <- 1
#label_min_cites <- 20

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
  rename(new = id, old = refs) %>%
  filter(firstauth.old != firstauth.new) # Delete self-cites

top_articles <- good_cite %>%
  group_by(old, shortcite.old, longcite.old) %>%
  tally() %>%
  arrange(-n)

cutoff <- top_articles$n[100]

top_articles <- top_articles %>%
  filter(n >= cutoff)

good_cite <- good_cite %>%
  filter(old %in% top_articles$old)

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

# Not doing much here; ungrouping but leaving this in case we want more edits later
top_pairs <- all_pairs %>%
  ungroup() %>%
  group_by(X1, X2) %>%
  tally() %>%
  arrange(-n) 

# Now create list of nodes that we're going to use
article_list <- top_articles$old %>%
  as_tibble() %>%
  rename(code = value) %>%
  group_by(code) %>%
  tally() 

# Now create list of things we're going to ignore
# We aren't deleting them yet, but if both members of a pair are ignorable, then delete
# Commenting this out because I'm leaving this in for this particular graph
#ignore_articles <- article_list %>%
#  filter(n == 1)

# Your label is your shortcite if you have enough cites and enough pairs
article_labels <- article_list %>%
  left_join(master_bib, by = c("code" = "id")) %>%
  select(code, shortcite)

# Start an edge list
# No filters yet; think about it later

edges <- top_pairs

# Third create nodes list. 
# Filter out articles we won't use 
# Should have each id number, plus an id column. 
# Note we've changed the WOS numbers to 'code', and called the new column 'id'
# Trying to follow https://www.jessesadler.com/post/network-analysis-with-r/ as closely as possible for now

nodes <- article_list %>%
  select(code) %>%
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
  select(from, to = id, weight = n) %>%
  mutate(linethick = case_when(
    weight == 1 ~ "1",
    weight == 2 ~ "2",
    weight < 6 ~ "3-5",
    weight < 11 ~ "6-10",
    TRUE ~ "11+"
  )) %>%
  mutate(linedense = case_when(
    weight == 1 ~ 0.1,
    weight == 2 ~ 0.2,
    weight < 6 ~ 0.3,
    weight < 11 ~ 0.4,
    TRUE ~ 0.5
  ))

thick_order <- c("1", "2", "3-5", "6-10", "11+")
edges$linethick <- factor(edges$linethick, levels = thick_order)

# To be done - rearrange the order of the labels
# I did this somewhere in LDA book, just got to figure out where and how

cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

g <- ggraph(cites_graph, layout = "nicely") + 
  geom_edge_link0(aes(color = as.factor(linedense),
                     width = linedense,
                     alpha = linedense)) + 
  geom_node_point(size = 0.6, alpha = 0.5) + 
  geom_node_text(aes(label = shortcite), 
                 repel = TRUE,
                 size = 8) +
  labs(edge_color = "Number of co-citations") +
  theme_graph()

g + labs(caption = "Co-citations between 1975 and 1996 of articles written between 1975 and 1996") +
  scale_edge_width_continuous(range = c(0.2,1)) +
  scale_edge_alpha_continuous(range = c(0.2,0.8)) +
  scale_edge_color_discrete(
    h = c(0, 360) + 15,
    c = 100,
    l = 65,
    h.start = 0,
    direction = 1,
    na.value = "grey50"
  )
  

ggsave("all_1975_1996.png", scale = 5, limitsize = FALSE)