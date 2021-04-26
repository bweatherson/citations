require(tidyverse)
require(tidygraph)
require(ggraph)

load("philo_cite.RData")
load("philo_bib.RData")
load("philo_cocite.RData")
load("philo_cocite_double.RData")

philo_cocite <- philo_cocite %>%
  filter(X1 != X2)

philo_cocite_double <- philo_cocite_double %>%
  filter(X1 != X2)

centre_code <- "WOS:A1983RR51600001"
centre_title <- "Freedom of the Will and the Concept of a Person"
centre_author <- "Harry Frankfurt"
centre_year <- 1971

cocites <- philo_cocite_double %>%
  filter(X1 == centre_code) %>%
  group_by(X2) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:24)

nodes <- bind_rows(
  filter(philo_bib, id == centre_code),
  left_join(cocites, philo_bib, by = c ("X2" = "id"))
  ) %>%
  mutate(code = ifelse(
    is.na(X2), id, X2
  )) %>%
  select(code, shortcite)

cite_count <- philo_cite %>%
  filter(refs %in% nodes$code) %>%
  group_by(refs) %>%
  tally() %>%
  mutate(cite_category = case_when(
    n < 51 ~ 1,
    n < 76 ~ 2,
    n < 101 ~ 3,
    n < 151 ~ 4,
    n < 201 ~ 5,
    TRUE ~ 6
  )) %>%
  select(code = refs, cite_category)

nodes <- left_join(nodes, cite_count, by = "code")

edges <- philo_cocite %>%
  ungroup() %>%
  filter(X1 %in% nodes$code, X2 %in% nodes$code) %>%
  select(from = X1, to = X2) %>%
  group_by(from, to) %>%
  tally(name = "weight") %>%
  mutate(weight_category = case_when(
    weight < 6 ~ 2,
    weight < 11 ~ 3,
    weight < 16 ~ 4,
    weight < 21 ~ 5,
    TRUE ~ 6
  ))

cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

g1 <- ggraph(cites_graph, layout = "star")

g2 <- g1 + 
  geom_edge_link0(aes(width = weight_category,
                      alpha = weight_category),
                  edge_color = "grey66") + 
  geom_node_point(aes(fill = "red",
                      size = cite_category),
                  #                  labels = cat_name,
                  shape = 21) + 
  geom_node_text(aes(label = shortcite), 
                 repel = TRUE,
                 size = 3) +
  labs(edge_width = "Number of co-citations",
       edge_alpha = "Number of co-citations",
       node_size = "Number of citations") +
  theme_graph()

g2 + labs(caption = paste(
  "Data from Web of Science.",
  # "Only citations in philosophy journals are counted.",
  # "Each node is one of the 10 most cited of its year, from 1966-2015.",
  # "Each edge means that the nodes were co-cited 6 or more times.",
  # "Nodes without links to elsewhere in graph are not shown.",
  # "Co-citations are counted through 2019.",
  # "Graph by Brian Weatherson.",
  sep = " "
)) +
  scale_edge_width_continuous(range = c(0.2,1),
                              name = "Number of Co-Citations",
                              labels = c(
                                "1-5", 
                                "6-10",
                                "11-15",
                                "16-20",
                                "21+"
                              ) # This works; maybe have to do same for others
  ) +
  scale_edge_alpha_continuous(range = c(0.2,0.5), guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
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


#ggsave("temp.png", scale = 7, limitsize = FALSE)