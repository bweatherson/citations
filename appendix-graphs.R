the_cat <- topics_big_table$cat_num[jjj]
target_articles <- round_three %>% filter(cat_num == the_cat)

year_distribution <- target_articles %>% 
  group_by(year) %>% 
  tally(wt = s) %>% 
  arrange(year) %>% 
  left_join(year_totals, by = "year") %>% 
  mutate(r = n / total)

journal_distribution <- pie_chart_data %>% 
  filter(cat_num == the_cat) %>% 
  mutate(fraction = cat_type / sum(cat_type),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = case_when(fraction < 0.05 ~ "",
                           TRUE ~ paste0(type, "\n", round(cat_type,0))),
         lab_size = case_when(fraction < 0.05 ~ 0,
                              TRUE ~ 0))

p1 <- ggplot(year_distribution, aes(x = year, y = r)) + 
  geom_point(color = graph_color(category_properties$cat_field[1])) + 
  scale_y_continuous(labels = scales::label_percent(accuracy =0.5), limits = c(0, NA)) +
  theme_minimal() +
  ggtitle("Topic Frequency by Year") +
  theme(text = element_text(family="Lato"),
        plot.title = element_text(size = rel(1),
                                  family = "Lato",
                                  face = "bold",
                                  margin = margin(0, 0, 10, 0)),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(-0.05, "lines"),
        panel.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.major.y = element_line(color = "grey85", size = 0.07),
        panel.grid.minor.y = element_line(color = "grey85", size = 0.03),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="none",
        plot.margin = margin(l = 20))

p2 <- ggplot(journal_distribution, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  ggtitle("Distribution across Journal Types") +
  geom_text(x=3.5, aes(y=labelPosition, label=label, size = 0)) +
  scale_fill_brewer(palette="Set2") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1),
                                  family = "Lato",
                                  face = "bold",
                                  margin = margin(0, 0, 10, 0)))

cap_text <- paste0(
  "Distribution of articles in ",
  category_properties$cat_real_name[1],
  " across time and across journals."
)

alt_text <- paste0(
  "Two graphs showing distribution of articles in the topic ",
  category_properties$cat_real_name[1],
  ". On the left is a scatterplot showing which percentage of each year's articles are in that topic. ",
  "The mean value of this graph is ",
  percent(
    mean(
      year_distribution$r
    ),
    accuracy = 0.1
  ),
  " and the median is ",
  percent(
    median(
      year_distribution$r
    ),
    accuracy = 0.1
  ),
  ". The graph reaches a peak value of ",
  percent(
    max(
      year_distribution$r
    ),
    accuracy = 0.1
  ),
  " in ",
  slice_max(year_distribution, r, n = 1)$year[1],
  " and has a minimum value of ",
  percent(
    min(
      year_distribution$r
    ),
    accuracy = 0.1
  ),
  " in ",
  slice_min(year_distribution, r, n = 1)$year[1],
  ".",
  " On the right is a pie chart showing the distribution of articles across types of journals. ",
  round(journal_distribution$cat_type[1],1),
  " articles appeared in Generalist journals, ",
  round(journal_distribution$cat_type[2],1),
  " articles appeared in Science journals, ",
  round(journal_distribution$cat_type[3],1),
  " articles appeared in Specialist journals, and ",
  round(journal_distribution$cat_type[4],1),
  " articles appeared in Value Theory journals."
  
)

