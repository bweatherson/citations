post_philo_cite <- philo_cite %>% 
  filter(id %in% modern_philo_bib$id) %>% 
  filter(refs %in% modern_philo_bib$id) %>% 
  left_join(modern_philo_bib %>% select(id, year), by = "id") %>% 
  left_join(modern_philo_bib %>% select(refs = id, ref_year = year), by = "refs") %>% 
  filter(year < 2020, ref_year < 2020) 

year_count <- modern_philo_bib %>% 
  ungroup() %>% 
  group_by(year) %>% 
  tally(name="total_articles")

cite_gap_summary <- post_philo_cite %>% 
  group_by(year, ref_year) %>% 
  tally() %>% 
  mutate(gap = year - ref_year) %>% 
  left_join(year_count, by = "year") %>% 
  mutate(ratio = n/total_articles)
  
summary_by_gap <- cite_gap_summary %>% 
  ungroup() %>% 
  group_by(gap) %>% 
  summarise(cites = sum(n))

ggplot(cite_gap_summary, aes(x = year, y = ref_year, size = n, fill = n, color = n)) + geom_point()

ggplot(cite_gap_summary, aes(x = year, y = ratio)) +
  geom_point() +
  facet_wrap(vars(ref_year))