require(tidyverse)
require(igraph)

small_tibble <- tribble(
  ~A, ~B, ~weight,
  1, 2, 100,
  1, 3, 100,
  2, 3, 100,
  3, 4, 1,
  2, 5, 1,
  1, 6, 1,
  4, 5, 100,
  4, 6, 100,
  5, 6, 100
)

small_graph <- graph_from_data_frame(small_tibble, directed = FALSE)

print(small_graph, e = TRUE, v = TRUE)

is_weighted(small_graph)

comm <- cluster_walktrap(small_graph)

membership(comm)

showmepaper <- function(x){
  t <- philo_bib %>% filter(id == x)
  t$longcite[1]
}