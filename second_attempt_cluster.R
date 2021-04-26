require(tidyverse)
require(igraph)

load("philo_bib.RData")
load("philo_cite.RData")

set.seed(14071789)

modern_philo_bib <- philo_bib %>% 
  filter(year >= 1980, year < 2000)

extended_philo_bib <- philo_bib %>% 
  filter(year >= 1980, year < 2000)

modern_philo_cite <- philo_cite %>% 
  filter(id %in% modern_philo_bib$id) %>% 
  filter(refs %in% extended_philo_bib$id)

cite_counts <- modern_philo_cite %>%
  group_by(refs) %>%
  tally() 

extended_philo_bib <- extended_philo_bib %>% 
  left_join(cite_counts, by = c("id" = "refs")) %>%
  mutate_at("n", ~replace(., is.na(.), 0))

high_cited <- cite_counts %>% 
  filter(n >= 0) %>%
  arrange(-n) %>% 
  rowid_to_column(var = "myid")

mycodes <- high_cited %>% 
  select(myid, refs)

extended_philo_bib <- extended_philo_bib %>% 
  left_join(mycodes, by = c("id" = "refs"))

has_two_refs <- modern_philo_cite %>%
  filter(refs %in% high_cited$refs) %>% 
  group_by(id) %>% 
  tally() %>% 
  filter(n > 1)

modern_philo_cocite <- modern_philo_cite %>%
  filter(refs %in% high_cited$refs) %>%
  filter(id %in% has_two_refs$id) %>% 
  group_by(id) %>%
  arrange(id, refs) %>%
  do(data.frame(t(combn(.$refs, 2))))

graph_input <- modern_philo_cocite %>%
  group_by(X1, X2) %>%
  tally() %>%
  rename(weight = n) %>% 
  ungroup() %>%
  left_join(mycodes, by = c("X1" = "refs")) %>% 
  select(X1 = myid, X2, weight) %>% 
  left_join(mycodes, by = c("X2" = "refs")) %>% 
  select(X1, X2 = myid, weight)

graph_input_short <- graph_input %>% filter(weight >= 3)

the_graph <- graph_from_data_frame(graph_input_short, directed = FALSE)

# bet_list <-  betweenness(the_graph) %>%
#   enframe() %>%
#   top_n(100, value) %>%
#   left_join(modern_philo_bib, by = c("name" = "id"))
  
the_clusters <- cluster_walktrap(the_graph)

temp <- membership(the_clusters) %>%
  enframe() %>%
  mutate(name = as.numeric(name)) %>% 
  left_join(extended_philo_bib, by = c("name" = "myid")) %>%
  arrange(-n, value)

temp$value <- as.numeric(temp$value)

temp_sum <- temp %>%
  group_by(value) %>% 
  tally() %>%
  arrange(-n) %>%
  filter(n >= 3) # Change this if you want more categories; currently have 67

main_cites <- temp %>%
  filter(value %in% temp_sum$value)

main_cites_short <- main_cites %>% 
  select(value, id)

category_count <- main_cites_short %>% 
  group_by(value) %>% 
  tally()

main_cites_short <- main_cites_short %>% 
  left_join(category_count, by = "value")

featured_cites <- modern_philo_cite %>%
  filter(refs %in% main_cites_short$id) %>%
  left_join(main_cites_short, by = c("refs" = "id")) %>%
  mutate(v = 1/n)

total_cite_value <- featured_cites %>% 
  group_by(id) %>%
  mutate(s = sum(v)) %>%
  select(id, refs, s)

scoreboard <- featured_cites %>% 
  right_join(total_cite_value, by = c("id", "refs")) %>% 
  mutate(t = v / s) %>% 
  group_by(id, value) %>%
  summarise(v = sum(t), .groups = 'drop') %>%
  left_join(modern_philo_bib, by = "id")



epi_articles <- filter(the_categories, value < 4)$myid