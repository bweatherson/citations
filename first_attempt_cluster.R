require(tidyverse)
require(igraph)

load("philo_bib.RData")
load("philo_cite.RData")

modern_philo_bib <- philo_bib %>% 
  filter(year >= 2000)

modern_philo_cite <- philo_cite %>% 
  filter(id %in% modern_philo_bib$id) %>% 
  filter(refs %in% modern_philo_bib$id)

the_graph <- graph_from_data_frame(modern_philo_cite, directed = FALSE)

# bet_list <-  betweenness(the_graph) %>%
#   enframe() %>%
#   top_n(100, value) %>%
#   left_join(modern_philo_bib, by = c("name" = "id"))
  
the_clusters <- cluster_edge_betweenness(the_graph)

temp <- membership(the_clusters) %>%
  enframe() %>%
  left_join(modern_philo_bib, by = c("name" = "id"))

temp$value <- as.numeric(temp$value)

View(temp)

temp_sum <- temp %>%
  group_by(value) %>% 
  tally() %>%
  arrange(-n)

temp <- temp %>% 
  left_join(temp_sum, by = "value")

View(temp_sum)