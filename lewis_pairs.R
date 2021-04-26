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

lewis_cited <- master_bib %>%
  filter(str_detect(longcite, "^D Lewis")) %>%
  filter(year > 1974) %>%
  arrange(year, id) %>%
  ungroup() %>%
  slice(-4, -6, -16, -21, -26, -27, -28)

lewis_titles <- c("Time Travel", 
                  "Conditional Probability", 
                  "Reply to Jamieson", 
                  "Truth in Fiction", 
                  "PD Newcomb", 
                  "Scorekeeping", 
                  "Lucas Mechanism 2", 
                  "Time's Arrow", 
                  "De Dicto and De Se", 
                  "Veridical Hallucination", 
                  "Ordering Semantics", 
                  "Causal Decision Theory", 
                  "Puzzling Pierre", 
                  "Logic for Equivocators", 
                  "Individuation by Acquaintance", 
                  "Extrinsic Properties", 
                  "Levi against U", 
                  "New Work", 
                  "Putnam's Paradox", 
                  "Structural Universals", 
                  "Conditional Probability 2", 
                  "Ayer", 
                  "Rearrangement of Particles", 
                  "Vague Identity", 
                  "Desire as Belief", 
                  "Trap's Dilemma", 
                  "Mill and Milquetoast", 
                  "Punishment Chance", 
                  "Noneism or Allism", 
                  "Extra Argument Places",
                  "Humean Supervenience Debugged",
                  "Desire as Belief 2",
                  "Elusive Knowledge"
                  )

lewis_cited$label <- lewis_titles

good_cite <- master_cite %>%
  filter(refs %in% lewis_cited$id) %>%
  left_join(all_cites, by = "id") %>%
  left_join(all_cites, by = c("refs" = "id"), suffix = c(".new", ".old")) %>%
#  filter(journal.new %in% citing_journals) %>%
  rename(new = id, old = refs)

# This doesn't look relevant
# But need to filter out uncited articles or the later code breaks
# Not quite sure why this is needed, but seems important, and useful tibble to have anyway
cite_count <- good_cite %>%
  group_by(new) %>%
  tally() %>%
  filter(n > 1)

all_pairs <- good_cite %>%
  filter(new %in% cite_count$new) %>%
  group_by(new) %>%
  arrange(old) %>%
  do(data.frame(t(combn(.$old, 2))))

top_pairs <- all_pairs %>%
  ungroup() %>%
  group_by(X1, X2) %>%
  tally() %>%
  arrange(-n) %>%
#  filter(n > 2) %>%
  left_join(all_cites, by = c("X1" = "id")) %>%
  left_join(all_cites, by = c("X2" = "id")) %>%
#  filter(firstauth.x != firstauth.y) %>%
  select(-firstauth.x, -firstauth.y)

top_cites <- good_cite %>%
  group_by(old, longcite.old) %>%
  tally() %>%
#  filter(n > 1) %>%
  arrange(-n) %>%
  rename(cites = n)

article_list <- c(top_pairs$X1, top_pairs$X2) %>%
  as_tibble() %>%
  rename(code = value) %>%
  group_by(code) %>%
  tally() 

article_labels <- article_list %>%
  left_join(lewis_cited, by = c("code" = "id")) %>%
  select(code, label)

edges <- top_pairs

nodes <- lewis_cited %>%
  filter(id %in% edges$X1 | id %in% edges$X2) %>%
  select(code=id, label) %>%
  rowid_to_column("id")

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
  geom_node_text(aes(label = label), 
                 repel = TRUE,
                 size = 2) +
  labs(edge_color = "Number of co-citations") +
  theme_graph()

g + labs(caption = "Co-citations of Lewis Articles between 1975 and 1995")

ggsave("lewis_1975-1995.png")

edges_reverse <- edges %>%
  rename(from = to, to = from)

edges_double <- bind_rows(edges, edges_reverse)

cites_graph_double <- tbl_graph(nodes = nodes, edges = edges_double, directed = FALSE)


s <- distances(cites_graph_double)