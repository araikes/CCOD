teson.tests <- c("ImPACT", "SAC", "SCAT", "ANAM", "Cogsport", "CogSport")
teson.articles <- ccod.included %>%
  select(record_id:pub_doi, cognitive_test_1:cognitive_test_16) %>%
  gather(Outcome, Test, -record_id:-pub_doi) %>%
  filter(grepl(paste(teson.tests, collapse = "|"), Test)) %>%
  ungroup() %>%
  separate(Outcome, c("Type", "dummy", "Number"), "_") %>%
  group_by(record_id) %>%
  mutate(num2 = seq(1:n())) %>%
  unite(Outcome, c("Type", "dummy", "num2")) %>%
  select(-Number) %>%
  spread(Outcome, Test) %>%
  arrange(record_id) %>%
  write_csv(., path = "./Tables/Article lists for meta-analyses/Emily.csv")

bishop.articles <- teson.articles %>%
  filter_at(vars(cognitive_test_1:cognitive_test_3), any_vars(. == "ImPACT")) %>%
  write_csv(., path = "./Tables/Article lists for meta-analyses/Carley.csv")
  
