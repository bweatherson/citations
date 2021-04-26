round_three_short <- round_three %>% 
  select(myid, cat_num, s)

cross_cite <- inner_join(double_cocitation, round_three_short, by = c("X1" = "myid")) %>% 
  rename(cat1 = cat_num, s1 = s) %>% 
  inner_join(round_three_short, by = c("X2" = "myid")) %>% 
  rename(cat2 = cat_num, s2 = s) %>% 
  mutate(s = s1 * s2) %>% 
  left_join(select(category_years, cat_num, cat_class, cat_real_name), by = c("cat2" = "cat_num"))

max_cross_cite <- cross_cite %>% 
  filter(cat1 != cat2) %>% 
  group_by(cat1, cat2, cat_class) %>% 
  summarise(cross = sum(s)) %>% 
  group_by(cat1, cat_class) %>% 
  arrange(-cross) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(select(category_years, cat_num, cat_class, cat_real_name), by = c("cat2" = "cat_num"))

temp <- cross_cite %>% 
  filter(cat1 == 3, cat2 == 9) %>% 
  group_by(article) %>% 
  tally(wt = s) %>% 
  left_join(extended_philo_bib, by = c("article" = "myid"))
