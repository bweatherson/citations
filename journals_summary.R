# Summary of Journals

journal_year_summary <-  extended_philo_bib %>% 
  filter(year >= 2000, year < 2020) %>% 
  group_by(journal, year) %>% 
  summarise(papers = n()) %>% 
  arrange(journal, year) 

ggplot(journal_year_summary, aes(x = year, y = papers)) +
  geom_point(size = 0.15) + 
  facet_wrap(~journal, 
             ncol=5,
             scales = "free_y") 
  