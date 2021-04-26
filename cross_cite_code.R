interactions <- modern_philo_cite %>% 
  right_join(round_three, by = "id") %>% 
  select(id, refs, cite_from_article = longcite, cite_from_s = s, cite_from_cat_num = cat_num) %>% 
  right_join(round_three, by = c("refs" = "id")) %>% 
  select(id, refs, cite_from_article, cite_from_s, cite_from_cat_num, cite_to_article = longcite, cite_to_s = s, cite_to_cat_num = cat_num) %>% 
  mutate(s = cite_from_s * cite_to_s) %>% 
  left_join(topics_big_table %>% select(cat_num, cat_class, cat_real_name, n), by = c("cite_from_cat_num" = "cat_num")) %>% 
  rename(cite_from_cat_class = cat_class, cite_from_cat_real_name = cat_real_name, cat_from_n = n) %>% 
  left_join(topics_big_table %>% select(cat_num, cat_class, cat_real_name, n), by = c("cite_to_cat_num" = "cat_num")) %>% 
  rename(cite_to_cat_class = cat_class, cite_to_cat_real_name = cat_real_name, cat_to_n = n)
  
citations_from_class <- interactions %>% 
  group_by(cite_from_cat_real_name, cite_from_cat_class, cite_to_cat_real_name, cite_to_cat_class, cat_from_n, cat_to_n) %>% 
  tally(wt = s) %>% 
  filter(cite_from_cat_real_name != cite_to_cat_real_name) %>% 
  ungroup() %>% 
  complete(nesting(
            cite_from_cat_real_name, cite_from_cat_class, cat_from_n
            ),
           nesting(
            cite_to_cat_real_name, cite_to_cat_class, cat_to_n
           ),
           fill=list(n = 0)
            ) %>% 
  filter(cite_from_cat_real_name != cite_to_cat_real_name) %>% 
  mutate(nn = cat_from_n * cat_to_n,
         r = n/(nn^0.5)
        )

# mean_citations_from <- citations_from_class %>% 
#   group_by(cite_from_cat_real_name) %>% 
#   summarise(avg = mean(r), stdev = sd(r))
# 
# mean_citations_to <- citations_from_class %>% 
#   group_by(cite_to_cat_real_name) %>% 
#   summarise(avg = mean(r), stdev = sd(r))
# 
# 
# cite_from_z_score <- left_join(citations_from_class, mean_citations_from, by = "cite_from_cat_real_name") %>% 
# #  mutate(from_score = (r - avg)/ stdev) %>% 
#   mutate(from_score = r) %>% 
#   arrange(-from_score)
# 
# cite_to_z_score <- left_join(citations_from_class, mean_citations_to, by = "cite_to_cat_real_name") %>% 
#   mutate(to_score = (r - avg)/ stdev) %>% 
#   arrange(-to_score)

topic_connection <- citations_from_class %>% 
  left_join(select(citations_from_class, cite_to_cat_real_name, cite_from_cat_real_name, r),
                   by = c("cite_from_cat_real_name" = "cite_to_cat_real_name", 
                                         "cite_to_cat_real_name" = "cite_from_cat_real_name")) %>% 
  mutate(score = r.x + r.y) %>% 
  arrange(-score) %>% 
  select(cite_from_cat_real_name, cite_to_cat_real_name, score, everything()) %>% 
  filter(r.x < r.y)

# 2: Presentism - Temporal Experience
# 3: Just War Theory - Self Defence
# 6 : Perceptual Content - Justification
# 13: Mechanisms - Exclusion Problem
# 19: Extended Mind - Predictive Processing
# 39: Grounding - Presentism (note direction)
# 47: Promises - Silencing (important example)
# 54: Constructive Empiricism - The Empirical Stance (surprised it isn't higher)
# 93: Proof Theory - Generics (WTF?!)
# 116: Chance - Fitness
# 160: Pasadena Problem - Ethics and Risk (theory to applied very quickly)
# 171: Color-Smells (look how asymmetric!)
# 219: Indexicals - Silencing