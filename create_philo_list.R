journal_list <- master_bib %>%
  group_by(journal) %>%
  tally() %>%
  arrange(-n) %>%
  slice(2, 3, 6, 7, 9,
        13,14,16:37,
        40:43,
        45,46,48,52,53,56,57,59,
        63:67,69,70,
        75,77,79,84,89,91,96,99,102,105:107,
        111:112,
        115,125,127,129,131,133,137,138,139,145,155,158,159,176,
        178,179,183,186,187,
        195,198,203,215,216,217,220,223,225,229,230,233,242,248,249,259,260,280,282,
        296,297,298,317,321,322)

philo_bib <- master_bib %>%
  filter(journal %in% journal_list$journal)

philo_cite <- master_cite %>%
  filter(id %in% philo_bib$id) %>%
  filter(refs %in% philo_bib$id)

temp <- jphil_bib %>%
  select(id = newcode, shortcite, author = authname, year) %>%
  mutate(journal = "Journal Of Philosophy") %>%
  mutate(author = as.list(author)) %>%
  mutate(year = as.numeric(year))

philo_bib <- bind_rows(philo_bib, temp)

philo_cite <- bind_rows(philo_cite, jphil_cites)

save(philo_bib, file = "philo_bib.RData")
save(philo_cite, file = "philo_cite.RData")

temp <- philo_cite %>%
  group_by(id) %>%
  tally() %>%
  filter(n > 1)

philo_cocite <- philo_cite %>%
  filter(id %in% temp$id) %>%
  group_by(id) %>%
  arrange(refs) %>%
  do(data.frame(t(combn(.$refs, 2))))

philo_cocite_reverse <- philo_cocite %>%
  rename(X2 = X1, X1 = X2)

philo_cocite_double <- bind_rows(philo_cocite, philo_cocite_reverse)

save(philo_cocite, file = "philo_cocite.RData")
save(philo_cocite_double, file = "philo_cocite_double.RData")
