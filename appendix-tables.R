the_cat <- topics_big_table$cat_num[jjj]
target_articles <- round_three %>% filter(cat_num == the_cat)

journal_distribution <- target_articles %>%
  group_by(journal) %>%
  tally(wt = s) %>%
  arrange(-n) %>%
  mutate(n = round(n, 2)) %>%
  slice(1:10)

cat("**Most Frequent Journals**")

print(kable(journal_distribution, align = c('l', 'r'),
            col.names = c("Journal", "Number of Articles")))

journals_count <- modern_philo_bib %>% 
  group_by(journal) %>% 
  tally(name = "t")

journals_ratio <- journal_distribution %>% 
  left_join(journals_count, by = "journal") %>% 
  mutate(r = round(n/t, 3)) %>% 
  arrange(-r) %>% 
  slice(1:10) %>% 
  select(journal, r) %>% 
  mutate(r = scales::percent(r, accuracy = 0.1))

cat(" \n\n")

cat("**Highest Ratio in Journals**")

print(kable(journals_ratio, align = c('l', 'r'),
            col.names = c("Journal", "Percentage of Articles")))

cat(" \n\n")
cat("**Number of Citations**")

category_size <- tribble(
  ~Citations, ~`Mean Citations`, ~Rank,
  round(category_properties$c[1], 1), 
  round(category_properties$mean_cite[1], 2),
  category_properties$mean_cite_rank
)

print(kable(category_size, align=rep('c', 4)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
        row_spec(1, background = "#f8f8f8"))

cat(" \n\n")
cat("**Articles by Year**")

print(
  kable(
    year_distribution %>% 
      select(year, n, r) %>% 
      mutate(n = round(n, 1)) %>% 
      mutate(r = scales::percent(r, accuracy = 0.001)) %>%  
      rename(Year = year, Articles = n, Proportion = r),
    align = c('l', 'c', 'c')
  ) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
)

cat(" \n\n")
cat("**Articles by Journal Type**")

journal_types <- pie_chart_data %>% 
  filter(cat_num == the_cat) %>% 
  mutate(r = round(cat_type, 1)) %>% 
  select(Type = type, Articles = r)

journal_types$Type[1] <- "General"
journal_types$Type[2] <- "Philosophy of Science"
journal_types$Type[3] <- "Specialist"
journal_types$Type[4] <- "Moral and Political"

print(kable(journal_types, align=c('l', 'r')) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")))

cat(" \n\n")
cat("**Topic Age**")

category_age <- tribble(
  ~`Mean Year`, ~`Relative Age`, ~`Relative Age in Group`,
  round(category_properties$y[1], 2),
  ordinal(category_properties$y_rank[1]),
  ordinal(category_properties$class_y_rank[1])
)

print(kable(category_age, align=rep('c', 3)) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
        row_spec(1, background = "#f8f8f8"))

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

print(kable(category_citation_table, align=c('l',rep('c', 3))) %>% 
        kable_styling(bootstrap_options = c("hover", "condensed", "responsive")) %>% 
        row_spec(c(1,3), background = "#f8f8f8") %>% 
        row_spec(2, background = "#ffffff"))

# cat("**Highly Cited Articles** \n \n")
# cat("::: apa-reference \n\n")  
# for (jj in 1:20){
#   cat(target_articles$longcite[jj], " \n\n", sep="")
# }
# cat(":::\n\n")

cat("<br>")
cat("**Precursors** \n \n")
cat("::: apa-reference \n\n")  
for (jj in 1:nrow(active_precursors)){
  cat(active_precursors$longcite[jj], " \n\n", sep="")
}
cat(":::\n\n")