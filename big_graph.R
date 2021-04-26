require(tidyverse)
require(tidygraph)
require(ggraph)

load("philo_cite.RData")
load("philo_bib.RData")
load("philo_cocite.RData")

top_cites <- philo_cite %>%
  distinct(refs, id) %>%
  group_by(refs) %>%
  tally() %>%
  arrange(-n) %>%
  left_join(philo_bib, by = c("refs" = "id")) %>%
  select(refs, shortcite, longcite, year, n)

top_cites_by_year <- top_cites %>%
  group_by(year) %>%
  filter(year < 2016) %>%
  top_n(10, n) %>%
  arrange(year, -n)

#write.csv(top_cites_by_year, file = "the_500.csv")

top_cites_labelled <- read_csv("the_500_labelled.csv")

categories <- tribble(
  ~category, ~cat_name, ~cat_number,
  "AE", "Aesthetics", 1,
  "EP", "Epistemology", 2,
  "ET", "Ethics", 3,
  "HP", "History of Philosophy", 4,
  "LM", "Logic and Mathematics", 6,
  "ME", "Metaphysics", 7,
  "PL", "Philosophy of Language", 8,
  "PM", "Philosophy of Mind", 9,
  "PP", "Social and Political", 12,
  "PS", "Philosophy of Science", 11
) %>%
  arrange(cat_number)

#load("philo_cocite.RData")

# Maybe go back and delete all nodes without a 5+ link
# But leave in the 3-5 links because I like them
# This will be complicated

nodes <- left_join(top_cites_labelled, top_cites_by_year, by = c("shortcite", "n", "longcite", "year")) %>%
  select(refs, label, n, year, category) %>%
  left_join(categories, by = "category") %>%
  mutate(cite_category = case_when(
    n < 51 ~ 1,
    n < 76 ~ 2,
    n < 101 ~ 3,
    n < 151 ~ 4,
    n < 201 ~ 5,
    TRUE ~ 6
  ))

# This was for testing
# Still can't figure out how to manually do colors
#nodes <- nodes %>%
#  group_by(cat_number) %>%
#  top_n(3, n)

edges <- philo_cocite %>%
  filter(X1 %in% nodes$refs) %>%
  filter(X2 %in% nodes$refs) %>%
  filter(X1 != X2) %>%
  group_by(X1, X2) %>%
  tally(name = "weight") %>%
  ungroup() %>%
  filter(weight > 5)

big_edges <- edges %>%
  filter(weight > 5)

nodes <- nodes %>%
  filter(refs %in% big_edges$X1 | refs %in% big_edges$X2) %>%
  rowid_to_column(var = "id")

edges <- edges %>%
  inner_join(nodes, by = c("X1" = "refs")) %>%
  select(from = id, X2, weight) %>%
  inner_join(nodes, by = c("X2" = "refs")) %>%
  select(from, to = id, weight) %>%
  mutate(weight_category = case_when(
    weight < 6 ~ 2,
    weight < 11 ~ 3,
    weight < 16 ~ 4,
    weight < 21 ~ 5,
    TRUE ~ 6
  ))

#nodes <- nodes %>%
#  filter(id %in% edges$from | id %in% edges$to) %>%
#  rename(nodes = id)

########
# Bug here
# Gotta do edges first I think
#######

node_color <- nodes %>%
  group_by(cat_number) %>%
  tally() %>%
  select(cat_number) %>%
  mutate(cat_color = hcl(h = cat_number * 30 - 15,
                         c = 100,
                         l = 65)) %>%
  arrange(cat_color)


cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

g1 <- ggraph(cites_graph, layout = "graphopt")

g2 <- g1 + 
  geom_edge_link0(aes(width = weight_category,
                      alpha = weight_category),
                  edge_color = "grey66") + 
  geom_node_point(aes(fill = cat_name,
                  size = cite_category),
#                  labels = cat_name,
                  shape = 21) + 
  geom_node_text(aes(label = label), 
                 repel = TRUE,
                 size = 5) +
  labs(edge_width = "Number of co-citations",
       edge_alpha = "Number of co-citations",
       node_fill = "Topic",
       node_size = "Number of citations") +
  theme_graph()

g2 + labs(caption = paste(
  "Data from Web of Science.",
  "Only citations in philosophy journals are counted.",
  "Each node is one of the 10 most cited of its year, from 1966-2015.",
  "Each edge means that the nodes were co-cited 6 or more times.",
  "Nodes without links to elsewhere in graph are not shown.",
  "Co-citations are counted through 2019.",
  "Graph by Brian Weatherson.",
  sep = " "
)) +
  scale_edge_width_continuous(range = c(0.2,1),
                              name = "Number of Co-Citations",
                              labels = c(
                                    #"1-5", 
                                    "6-10",
                                    "11-15",
                                    "16-20",
                                    "21+"
                                  ) # This works; maybe have to do same for others
                                ) +
  scale_edge_alpha_continuous(range = c(0.2,0.5), guide = FALSE) +
  scale_fill_discrete(name = "Subjects"#,
                      #labels = categories$cat_name
                      ) +
  scale_size(name = "Citations",
             labels = c(
               "0-50",
               "51-75",
               "76-100",
               "101-150",
               "151-200",
               "201+"
             )
             )


ggsave("temp.png", scale = 7, limitsize = FALSE)