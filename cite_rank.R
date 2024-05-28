require(tidyverse)
require(igraph)

set.seed(14071789)

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
  filter(cited_year >= 1975, cited_year <= 2019, citing_year >= 2000, citing_year <= 2019) |>
  mutate(cited_group = floor(cited_year/5) - 394)

cite_count_by_article <- philo_cite_years |>
  group_by(cited_article) |>
  tally() |>
  filter(n >= 30)

cite_rank <- philo_cite_years |>
  filter(cited_article %in% cite_count_by_article$cited_article) |>
  group_by(cited_article, citing_year, cited_group) |>
  tally(name = "annual_cites") |>
  ungroup() |>
  complete(nesting(cited_article, cited_group), citing_year, fill = list(annual_cites = 0)) |>
  ungroup() |>
  group_by(cited_group, citing_year) |>
  mutate(annual_rank = min_rank(desc(annual_cites)))

early_nineties <- cite_rank |>
  filter(cited_group == 4) |>
  ungroup() |>
  group_by(cited_article) |>
  mutate(best_rank = min(annual_rank)) |>
  filter(best_rank <= 2) |>
  left_join(philo_bib, by = c("cited_article" = "id"))

ggplot(early_nineties, aes(x = citing_year, y = annual_rank)) +
  geom_point() +
  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)

ggplot(early_nineties, aes(x = citing_year, y = annual_cites)) +
  geom_point() +
#  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)

late_nineties <- cite_rank |>
  filter(cited_group == 5) |>
  ungroup() |>
  group_by(cited_article) |>
  mutate(best_rank = min(annual_rank)) |>
  filter(best_rank <= 2) |>
  left_join(philo_bib, by = c("cited_article" = "id"))

ggplot(late_nineties, aes(x = citing_year, y = annual_rank)) +
  geom_point() +
  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)

ggplot(late_nineties, aes(x = citing_year, y = annual_cites)) +
  geom_point() +
  #  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)

early_aughts <- cite_rank |>
  filter(cited_group == 6) |>
  filter(citing_year >= 2002) |>
  ungroup() |>
  group_by(cited_article) |>
  mutate(best_rank = min(annual_rank)) |>
  filter(best_rank <= 3) |>
  left_join(philo_bib, by = c("cited_article" = "id"))

ggplot(early_aughts, aes(x = citing_year, y = annual_rank)) +
  geom_point() +
  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)

ggplot(early_aughts, aes(x = citing_year, y = annual_cites)) +
  geom_point() +
  #  scale_y_continuous(trans = c("log10", "reverse")) +
  facet_wrap(~shortcite, ncol = 3)