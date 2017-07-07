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
  summarise_at(vars(`Australian English`:`Swedish`), funs(sum(., na.rm = TRUE))) %>%
  left_join(ccod.lang.samplesizes) %>%
  select(record_id, total_n, everything())

ccod.lang.summary1b <- ccod.lang.summary1a %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum) %>%
  gather(Language, n) %>%
  arrange(desc(n)) %>%
  filter(n != 0.000) %>%
  mutate(proportion = round(n/sum(ccod.lang.samplesizes$total_n)*100, 3))

ccod.lang.summary2 <- ccod.lang.summary1a %>%
  ungroup() %>%
  mutate_at(vars(`Australian English`:Swedish), funs(round((./total_n)*100, 3)))

# Majority language
ccod.lang.summary2b <- ccod.lang.summary2  %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(n >= 50.000) %>%
  group_by(Language) %>%
  summarise(count = n())

# Only language
ccod.lang.summary2c <- ccod.lang.summary2  %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(n > 99.999) %>%
  group_by(Language) %>%
  summarise(count = n())

# English as Majority/Minority
ccod.lang.summary2d <- ccod.lang.summary2 %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(Language == "English") %>%
  filter(n != 0.000) %>%
  mutate(over50 = ifelse(n > 50, "Majority", "Minority")) %>%
  group_by(over50) %>%
  summarise(count = n())

# Total number of articles reporting the language
ccod.lang.summary3 <- ccod.lang.summary2 %>%
  select(-record_id, -total_n) %>%
  gather(Language, n) %>%
  filter(n != 0.000) %>%
  group_by(Language) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Articles reporting multiple languages
ccod.lang.summary4 <- ccod.lang.summary2 %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(n != 0.000) %>%
  group_by(record_id) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

# Articles reporting single language
ccod.lang.summary5 <- ccod.lang.summary1a %>%
  select(-total_n) %>%
  gather(Language, n, -record_id) %>%
  filter(n != 0.000) %>%
  group_by(record_id) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  arrange(desc(count))

#### Write tables ####
lang.table <- ccod.lang.summary2 %>%
  ungroup() %>%
  left_join(ccod.articleinfo) %>%
  select(-pub_author:-pub_journal, -pub_doi) %>%
  select(record_id, pub_apa, pub_year, pop_country, everything()) %>%
  gather(Lang, n, -record_id, -pub_apa, -pub_year, -pop_country, -total_n) %>%
  filter(n != 0.000) %>%
  arrange(desc(n)) %>%
  mutate(n = round(n, 2)) %>% 
  unite(Language, Lang, n, sep = ", ") %>%
  arrange(pub_apa)

lang.table.countryyear <- lang.table %>%
  select(record_id:pop_country) %>%
  distinct()

lang.distribution.table <- left_join(ccod.lang.summary1b, ccod.lang.summary3) %>%
  left_join(ccod.lang.summary2b, by = c("Language")) %>%
  left_join(ccod.lang.summary2c, by = c("Language"))

write_csv(lang.table, 
          path = "C:/Users/adamraikes/Documents/GitHub/CCOD/Tables/Language.csv")

write_csv(lang.distribution.table, 
          path = "C:/Users/adamraikes/Documents/GitHub/CCOD/Tables/Language Distribution.csv")

write_csv(lang.table.countryyear,
          path = "./Tables/CSVs from R/Language Country and Year.csv")

  
