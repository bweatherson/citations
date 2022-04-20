require(tidyverse)
require(tidygraph)
require(ggraph)

# temp <- citations_from_class %>% 
#   filter(cite_from_cat_class == 1, cite_to_cat_class == 1) %>% 
#   select(from_text = cite_from_cat_real_name, to_text = cite_to_cat_real_name, weight = n)
# 
# nodes <- temp %>% 
#   group_by(from_text) %>% 
#   summarise(s = sum(weight)) %>% 
#   arrange(-s) %>% 
#   select(-s) %>% 
#   rowid_to_column(var = "id")

temp3 <- c("Debunking Arguments",
           "Reasons", 
           "Disagreement",
           "Contextualism",
           "Virtue Epistemology",
           "Relativism",
           "Justification",
           "Perceptual Content",
           "Physicalism",
           "Models",
           "Mechanisms",
           "Exclusion Problem",
           "Grounding",
           "Parthood",
           "Dispositions",
           "Egalitarianism")

nodes <- enframe(temp3, name = "id", value = "from_text")

edges <- citations_from_class %>% 
  filter(cite_from_cat_real_name %in% temp3, cite_to_cat_real_name %in% temp3) %>% 
  select(from_text = cite_from_cat_real_name, to_text = cite_to_cat_real_name, weight = n) %>% 
  left_join(nodes, by = "from_text") %>% 
  select(from = id, everything()) %>% 
  left_join(nodes, by = c("to_text" = "from_text")) %>% 
  select(from, to = id, everything())

cites_graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(cites_graph, layout = 'linear', circular = TRUE) + 
  theme_graph() +
  geom_edge_arc2(aes(width = weight,
                      alpha = weight),
                  edge_color = "thistle2") +
#  scale_edge_alpha('Edge direction', guide = 'edge_direction') +
  geom_node_point(shape = 21) +
  geom_node_text(aes(label = from_text), 
                 repel = TRUE)


# temp2 <- edges %>% 
#   complete(from_text, to_text, fill = list(weight = NA)) %>% 
#   arrange(from_text, to_text) %>% 
#   pivot_wider(id_cols = from_text, names_from = to_text, values_from = weight)
# 
# temp3 <- c("Egalitarianism",
#            "Reasons", 
#            "Disagreement",
#            "Justification",
#            "Perceptual Content",
#            "Contextualism",
#            "Relativism",
#            "Debunking Arguments",
#            "Models",
#            "Mechanisms",
#            "Exclusion Problem",
#            "Physicalism",
#            "Grounding",
#            "Parthood",
#            "Dispositions")
# 
# nodes <- enframe(temp3, name = "id", value = "from_text")