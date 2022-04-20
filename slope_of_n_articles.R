category_year_quantity <- round_three %>% 
  group_by(cat_num, year) %>% 
  summarise(m = sum(s), .groups = "drop")

temp <- category_year_quantity %>% 
  split(.$cat_num) %>% 
  map(~lm(m ~ year, data = .x)) %>% 
  map_df(broom::tidy, .id = 'cat_num') %>% 
  filter(term == "year")
  
View(temp)

category_year_frequency <- round_three %>% 
  group_by(year, cat_num) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup()

temp <- category_year_frequency %>% 
  split(.$cat_num) %>% 
  map(~lm(freq ~ year, data = .x)) %>% 
  map_df(broom::tidy, .id = 'cat_num') %>% 
  filter(term == "year") %>% 
  mutate(slope = estimate - std.error) %>% 
  mutate(cat_num = as.numeric(cat_num)) %>% 
  select(cat_num, slope) %>% 
  left_join(category_years, by = "cat_num") %>% 
  mutate(growth = round(1000000000 * slope / sqrt(n)^2)) %>% 
  arrange(-growth)

View(temp)