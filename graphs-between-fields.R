require(tidyverse)
require(tidygraph)
require(ggraph)

temp5 <-  citations_from_class %>% 
  select(from = cite_from_cat_real_name, to = cite_to_cat_real_name, cc = n) %>% 
  left_join(category_years, by = c("from" = "cat_real_name")) %>% 
  select(from, from_field = cat_field, to, cc) %>% 
  left_join(category_years, by = c("to" = "cat_real_name")) %>% 
  select(from, from_field, to, to_field = cat_field, cc) %>% 
  group_by(from_field, to_field) %>% 
  summarise(weight = sum(cc)) %>% 
  filter(from_field != to_field)

temp5 <- temp5 %>% 
  bind_rows(temp5 %>% 
              select(to_field = from_field, from_field = to_field, weight)) %>% 
  ungroup() %>% 
  group_by(from_field, to_field) %>% 
  summarise(w = sum(weight))
  

temp6 <- c("Political",
           "Ethics",
           "Epistemology",
           "Language",
           "Mind",
           "Metaphysics",
           "Science",
           "Logic")

nodes <- enframe(temp6, name = "id", value = "from_field")

edges <- temp5 %>% 
  filter(from_field %in% temp6, to_field %in% temp6) %>% 
  rename(weight = w) %>% 
  left_join(nodes, by = "from_field") %>% 
  select(from = id, everything()) %>% 
  left_join(nodes, by = c("to_field" = "from_field")) %>% 
  select(from, to = id, everything())

cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(cites_graph, layout = 'linear', circular = TRUE) + 
  theme_graph() +
  geom_edge_arc2(aes(width = weight,
                     alpha = weight),
                 edge_color = "thistle2") +
  #  scale_edge_alpha('Edge direction', guide = 'edge_direction') +
  geom_node_point(shape = 21) +
  geom_node_text(aes(label = from_field), 
                 repel = TRUE)
