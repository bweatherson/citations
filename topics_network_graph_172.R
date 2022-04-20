require(tidyverse)
require(tidygraph)
require(ggraph)

nodes_172 <- category_years %>% 
  rowid_to_column(var = "id")


edges_172 <- topic_connection %>% 
  filter(score > 0.05) %>% 
  left_join(nodes_172, by = c("cite_from_cat_real_name" = "cat_real_name")) %>% 
  rename(from = id) %>% 
  select(-cat_field) %>% 
  left_join(nodes_172, by = c("cite_to_cat_real_name" = "cat_real_name")) %>% 
  rename(to = id) %>% 
  select(-cat_field) %>% 
  mutate(weight = round(score, 1)) %>% 
  select(from, to, weight, from_text = cite_from_cat_real_name, to_text = cite_to_cat_real_name, cat_from_n, cat_to_n)

temp <- edges_172 %>% 
  group_by(from) %>% 
  summarise(s = sum(weight)) %>% 
  filter(s >= 1)

nodes_172 <- nodes_172 %>% 
  filter(id %in% temp$from)

edges_172 <- edges_172 %>% 
  filter(from %in% temp$from, to %in% temp$from)


cites_graph <- tbl_graph(nodes = nodes_172, edges = edges_172, directed = FALSE)

g1 <- ggraph(cites_graph, layout = "mds")

g2 <- g1 + 
  geom_edge_link0(aes(width = weight,
                      alpha = weight),
                  edge_color = "grey66") + 
  geom_node_point(aes(fill = cat_field,
                      size = n),
                  #                  labels = cat_name,
                  shape = 21) + 
  geom_node_text(aes(label = cat_real_name, size = 500 * round(log(n))), 
                 repel = TRUE
                 ) +
  labs(edge_width = "Rate of cross citations",
       edge_alpha = "Rate of cross citations",
       node_fill = "Topic",
       node_size = "Number of articles") +
  theme_graph()

#g2

#ggsave("temp.png", scale = 4, limitsize = FALSE)

g2 + labs(caption = paste(
  "Data from Web of Science.",
  "Only citations in philosophy journals are counted.",
  "Each node is one of the 172 biggest topics, as found by a clustering algorithm, in philosophy journals from 2000-2019.",
  "Each edge means that there was a substantial number of citations between the two topics.",
  "Citations are counted through 2019.",
  "Graph by Brian Weatherson.",
  sep = " "
)) +
  scale_edge_width_continuous(range = c(0.2,1),
                              name = "Rate of cross citations"#,
                              # labels = c(
                              #   #"1-5", 
                              #   "6-10",
                              #   "11-15",
                              #   "16-20",
                              #   "21+"
                              # ) # This works; maybe have to do same for others
  ) +
  scale_edge_alpha_continuous(range = c(0.2,0.5), guide = FALSE) +
  scale_fill_discrete(name = "Subjects"#,
                      #labels = categories$cat_name
  ) +
  scale_size(name = "Topic Size"#,
             # labels = c(
             #   "0-50",
             #   "51-75",
             #   "76-100",
             #   "101-150",
             #   "151-200",
             #   "201+"
             # )
  )


ggsave("temp.png", scale = 5, limitsize = FALSE)

temp <- edges_172 %>% 
  group_by(from) %>% 
  summarise(s = sum(weight))