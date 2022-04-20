# Find files that are left out

# First find articles that are in the set
networked_articles <- c(modern_philo_cite$id, modern_philo_cite$refs) %>% 
  as_tibble() %>% 
  rename(id = value) %>% 
  distinct(id)
  
# Now remove them from main bib

isolated_articles <- modern_philo_bib %>% 
  anti_join(networked_articles, by = "id")

missed_articles <- modern_philo_bib %>% 
  anti_join(isolated_articles, by = "id") %>% 
  anti_join(round_three, by = "id") %>% 
  left_join(cite_counts, by = c("id" = "refs"))

year_isolated <- isolated_articles %>% 
  group_by(year) %>% 
  tally()

year_missed <- missed_articles %>% 
  group_by(year) %>% 
  tally()

modern_philo_cite %>% 
  left_join(modern_philo_bib, by = "id") %>% 
  group_by(year) %>% 
  tally() %>% 
  ggplot(aes(x = year, y = n)) + geom_line()

modern_philo_cite %>% 
  left_join(modern_philo_bib, by = "id") %>% 
  group_by(year) %>% 
  summarise(cites_per = n() / n_distinct(id)) %>% 
  ggplot(aes(x = year, y = cites_per)) + geom_line()

journal_isolated <- isolated_articles %>% 
  group_by(journal) %>% 
  tally()

journal_missed <- missed_articles %>% 
  group_by(journal) %>% 
  tally()