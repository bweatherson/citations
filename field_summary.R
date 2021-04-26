cat_names_types <- read_csv("cat_names_types.csv")

category_years <- left_join(category_years,cat_names_types, by = "cat_num")

field_summary <- category_years %>% 
  group_by(cat_field) %>% 
  summarise(nt = sum(n), ct = sum(c), yt = weighted.mean(y,n)) %>% 
  mutate(mt = ct / nt)

category_big_table <- category_years %>% 
  arrange(cat_field,cat_real_name) %>% 
  rowid_to_column(var="alpha_id") %>% 
  mutate(n = round(n, 1)) %>% 
  select(cat_real_name,cat_field,cat_class,n)

# Instructions on how to include links in DT
# https://community.rstudio.com/t/create-interactive-links-in-gt-table-in-rmarkdown/70266
# Ideally would include these as auto-generated, but may be tricky
# Maybe here for more info https://blogs.oregonstate.edu/cgrb/2019/08/06/r-tips-a-table-makeover-with-dt/