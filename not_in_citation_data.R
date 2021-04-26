temp <- modern_philo_bib %>% 
  filter(!id %in% c(modern_philo_cite$id, modern_philo_cite$refs))

View(temp)

save(temp, file = "not-in-citation-record.RData")
