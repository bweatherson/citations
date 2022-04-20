cat(" \n\n")
cat("**Citations Across Categories**")
# Just find the three rows that list the three categories most commonly cited
# Then manually alter that table to produce a nice visual output
# This could be done by pivot wider, but it's actually easier by hand
cf_column <- max_cite_from  %>% 
  ungroup() %>% 
  filter(cite_from_cat_real_name == category_properties$cat_real_name[1]) %>% 
  arrange(cite_to_cat_class) %>% 
  mutate(`Most Commonly Cited By` =   paste0(cite_to_cat_real_name, " (", round(citations, 1), ")")) %>% 
  select(`Most Commonly Cited By`)

ct_column <- max_cite_to  %>% 
  ungroup() %>% 
  filter(cite_to_cat_real_name == category_properties$cat_real_name[1]) %>% 
  arrange(cite_from_cat_class) %>%
  mutate(`Most Commonly Cites` =   paste0(cite_from_cat_real_name, " (", round(citations, 1), ")")) %>% 
  select(`Most Commonly Cites`)

cc_column <- max_cat_co_cite  %>% 
  ungroup() %>% 
  filter(cat1 == the_cat) %>% 
  arrange(cat_class) %>% 
  mutate(`Most Commonly Co-Cited With` =   paste0(cat_real_name, " (", round(cross, 1), ")")) %>% 
  select(`Most Commonly Co-Cited With`)

g_column <- tribble(
  ~` `,
  "Group 1",
  "Group 2",
  "Group 3"
)

category_citation_table <- tibble(
  g_column,
  cf_column,
  ct_column,
  cc_column
)


category_cite_from <- tribble(
  ~`Group 1`, ~`Group 2`, ~`Group 3`,
  paste0(temp$cite_to_cat_real_name[1], " (", round(temp$citations[1], 1), ")"),
  paste0(temp$cite_to_cat_real_name[2], " (", round(temp$citations[2], 1), ")"),
  paste0(temp$cite_to_cat_real_name[3], " (", round(temp$citations[3], 1), ")")
)

print(kable(category_cite_from, align=rep('c', 3)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
        row_spec(1, background = "#f8f8f8"))

cat(" \n\n")
cat("**Most Commonly Cited By**")
# Really the same as the above table, just reverse the order
temp <- max_cite_to  %>% 
  filter(cite_to_cat_real_name == category_properties$cat_real_name[1]) %>% 
  arrange(cite_from_cat_class)

category_cite_to <- tribble(
  ~`Group 1`, ~`Group 2`, ~`Group 3`,
  paste0(temp$cite_from_cat_real_name[1], " (", round(temp$citations[1], 1), ")"),
  paste0(temp$cite_from_cat_real_name[2], " (", round(temp$citations[2], 1), ")"),
  paste0(temp$cite_from_cat_real_name[3], " (", round(temp$citations[3], 1), ")")
)

print(kable(category_cite_to, align=rep('c', 3)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
        row_spec(1, background = "#f8f8f8"))

cat(" \n\n")
cat("**Most Commonly Co-Cited With**")
# For now this one uses numbers
# Ugh, better fix this eventually
temp <- max_cat_co_cite  %>% 
  filter(cat1 == the_cat) %>% 
  arrange(cat_class)

category_co_cite <- tribble(
  ~`Group 1`, ~`Group 2`, ~`Group 3`,
  paste0(temp$cat_real_name[1], " (", round(temp$cross[1], 1), ")"),
  paste0(temp$cat_real_name[2], " (", round(temp$cross[2], 1), ")"),
  paste0(temp$cat_real_name[3], " (", round(temp$cross[3], 1), ")")
)

print(kable(category_co_cite, align=rep('c', 3)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
        row_spec(1, background = "#f8f8f8"))