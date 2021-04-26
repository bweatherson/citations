top_cites <- round_three %>% 
  group_by(year) %>% 
  arrange(year, -cite_count) %>% 
  mutate(rank = rank(-cite_count, ties.method = "first")) %>% 
  slice(1:9) 

ggplot(top_cites, aes(x = year, y = cite_count)) +
  geom_point() +
  facet_wrap(vars(rank))