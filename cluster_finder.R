require(tidyverse)
require(igraph)

# Next few lines are just setup and are slow, so are commented for now
load("~/Documents/citations/philo_cite.RData")
load("~/Documents/citations/philo_bib.RData")
philo_bib <- philo_bib |> as_tibble()
philo_cite <- philo_cite |> as_tibble()

philo_cite_years <- philo_cite |>
  select(cited_article = refs, citing_article = id) |>
  left_join(philo_bib, by = c("cited_article" = "id")) |>
  select(cited_article, citing_article, cited_year = year) |>
  left_join(philo_bib, by = c("citing_article" = "id")) |>
  select(cited_article, citing_article, cited_year, citing_year = year) |>
  filter(cited_year >= 1975, cited_year <= 1999, citing_year >= 2000, citing_year <= 2019) |>
  mutate(cited_group = floor(cited_year/5) - 394)
  
cite_count_by_group <- philo_cite_years |>
  group_by(cited_group) |>
  tally()

cite_count_by_year <- philo_cite_years |>
  group_by(citing_year) |>
  tally()

cite_count_by_article <- philo_cite_years |>
  group_by(cited_article) |>
  tally() |>
  filter(n >= 25)

philo_high_cite <- philo_cite_years |>
  filter(cited_article %in% cite_count_by_article$cited_article)

citing_count_by_article <- philo_high_cite |>
  group_by(citing_article) |>
  tally() |>
  arrange(-n) |>
  filter(n >= 6)

philo_graph_generator <- philo_high_cite |>
  filter(citing_article %in% citing_count_by_article$citing_article) |>
  select(cited_article, citing_article) |>
  pivot_longer(cols = everything())

philo_graph <- graph(philo_graph_generator$value)

cite_cluster <- cluster_walktrap(philo_graph, steps = 10)

cluster_membership <- membership(cite_cluster) %>%
  enframe() %>%
  as_tibble() |>
  select(cited_article = name, cluster = value) |>
  filter(cited_article %in% philo_high_cite$cited_article) |>
  left_join(select(philo_bib, id, longcite), by = c("cited_article" = "id")) |>
  left_join(cite_count_by_article, by = "cited_article") |>
  group_by(cluster) |>
  mutate(cluster_cites = sum(n))

top_clusters <- cluster_membership |>
  ungroup() |>
  group_by(cluster) |>
  summarise(cites = sum(n)) |>
  arrange(-cites)

cluster_count <- c

#cite_graph <- graph(cite_list$value)

#cite_cluster <- cluster_infomap(cite_graph, nb.trials = 2)
#length(cite_cluster)


#for (early_year in (395:395)*5){
#  for (late_year in 2000:2000){
#    early_articles <- philo_bib |>
#      filter(year >= early_year, year <= early_year+4)
#    late_articles <- philo_bib |>
#      filter(year == late_year)
#    citings <- philo_cite |>
#      filter(id %in% late_articles$id, refs %in% early_articles$id) |>
#      group_by(refs) |>
#      tally() |>
#      select(id = refs, n=n) |>
#      left_join(philo_bib, by = "id") |>
#      arrange(-n)
#  }
#}