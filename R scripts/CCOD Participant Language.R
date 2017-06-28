#### Header Information ####
# This script file is intended to provide some working code to summarise the
# various languages of individuals within the CCOD database. This provides one
# part of the backbone of the langating systematic review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Filter included articles to retain those articles which report some aspect
#### of language. ####

ccod.lang <- ccod.included %>%
  filter(lang_reported == "Yes") %>%
  select(record_id:pub_title, n_nontbi:n_total, lang_analysis, 
                n_lang_nontbi_perc:total_sample_language_complete) %>%
  select(-contains("complete"))

#### Convert to long format for manipulation ####
ccod.lang.long <- ccod.lang %>%
  gather(var, count, c(-record_id:-lang_analysis, -n_lang_nontbi_perc,
                                 -n_lang_mtbi_perc, -n_lang_modtbi_perc, -n_lang_stbi_perc,
                                 -n_lang_totsamp_perc)) %>%
  arrange(desc(pub_author))

#### Add group indicator ####
ccod.lang.long <- ccod.lang.long %>%
  left_join(labels) %>%
  rename(Language = aesthetic.label) %>%
  mutate(Group = ifelse(grepl("nontbi", var), "Non-TBI",
                               ifelse(grepl("mtbi", var), "mTBI",
                                      ifelse(grepl("modtbi", var), "modTBI",
                                             ifelse(grepl("stbi", var), "sTBI", "Total")))))

#### Compute numbers of participants from various backgrounds per severity/grouping ####
ccod.lang.nontbi <- compute_lang_by_group(ccod.lang.long, "Non-TBI")
ccod.lang.mtbi <- compute_lang_by_group(ccod.lang.long, "mTBI")
ccod.lang.modtbi <- compute_lang_by_group(ccod.lang.long, "modTBI")
ccod.lang.stbi <- compute_lang_by_group(ccod.lang.long, "sTBI")
ccod.lang.total <- compute_lang_by_total(ccod.lang.long)

#### Recombine dataframe ####
ccod.lang.complete <- bind_rows(ccod.lang.nontbi, ccod.lang.mtbi, 
                                         ccod.lang.modtbi, ccod.lang.stbi, 
                                         ccod.lang.total) %>%
  ungroup() %>%
  select(record_id, n_nontbi:n_total, Group, Language, participants) %>%
  spread(Language, participants)

#### Summarise language numbers ####
ccod.lang.samplesizes <- ccod.lang.complete %>%
  select(record_id:n_total) %>%
  mutate(total_n = ifelse(!is.na(n_total), n_total, rowSums(.[2:6], na.rm = TRUE))) %>%
  select(record_id, total_n) %>%
  distinct()

ccod.lang.summary1a <- ccod.lang.complete %>%
  group_by(record_id) %>%
  select(-n_nontbi:-Group) %>%
  summarise_all(sum) %>%
  left_join(ccod.lang.samplesizes) %>%
  select(record_id, total_n, everything())

ccod.lang.summary1b <- ccod.lang.summary1a %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum) %>%
  gather(lang, n) %>%
  arrange(desc(n)) %>%
  mutate(proportion = round(n/sum(ccod.lang.samplesizes$total_n)*100, 3))

ccod.lang.summary2 <- ccod.lang.summary1a %>%
  ungroup() %>%
  mutate_at(vars(`Australian English`:Swedish), funs(round((./total_n)*100, 3)))

ccod.lang.summary2b <- ccod.lang.summary2  %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(n >= 75.000)

ccod.lang.summary2c <- ccod.lang.summary2b %>%
  filter(n == 100.000) %>%
  group_by(Language) %>%
  summarise(count = n())

ccod.lang.summary2d <- ccod.lang.summary2 %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(Language == "English") %>%
  filter(!is.na(n)) %>%
  arrange(desc(n))

ccod.lang.summary3 <- ccod.lang.summary2 %>%
  select(-record_id, -total_n) %>%
  gather(Language, n) %>%
  filter(!is.na(n)) %>%
  group_by(Language) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ccod.lang.summary4 <- ccod.lang.summary1a %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(!is.na(n)) %>%
  group_by(record_id) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

  
