# load library
#library(ggplot2)

# Create test data.
data <- pie_chart_data %>% 
  filter(cat_num == 125) %>% 
  mutate(fraction = cat_type / sum(cat_type),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = case_when(fraction < 0.05 ~ "",
                            TRUE ~ paste0(type, "\n", round(cat_type,0))),
         lab_size = case_when(fraction < 0.05 ~ 0,
                              TRUE ~ 6))

# Compute percentages
#data$fraction <- data$cat_type / sum(data$cat_type)

# Compute the cumulative percentages (top of each rectangle)
#data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
#data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
#data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$type, "\n", round(data$cat_type,0))

# Adjust for too small

#data <- data %>% 
#  mutate(label = case_when(fraction < 0.05 ~ "",
#                           TRUE ~ label),
#         lab_size = case_when(fraction < 0.05 ~ 0,
#                              TRUE ~ 6))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  geom_text( x=3.5, aes(y=labelPosition, label=label, size = lab_size)) +
  scale_fill_brewer(palette="Set2") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
