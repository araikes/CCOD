#### Header Information ####
# This script file is intended to provide some working code to summarise the
# various origins of individuals within the CCOD database. This provides one
# part of the backbone of the originating systematic review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Filter included articles to retain those articles which report some aspect
#### of race, ethnicity or country of origin. ####

ccod.origin <- ccod.included %>%
  filter(origin_reported == "Yes") %>%
  select(record_id:pub_title, n_nontbi:n_total, origin_analysis, 
         n_orig_nontbi_perc:n_orig_totsamp_zimb) %>%
  select(-contains("complete"))

#### Convert to long format for manipulation ####
ccod.origin.long <- ccod.origin %>%
  gather(var, count, c(-record_id:-origin_analysis, -n_orig_nontbi_perc,
                                 -n_orig_mtbi_perc, -n_orig_modtbi_perc, -n_orig_stbi_perc,
                                 -n_orig_totsamp_perc)) %>%
  arrange(desc(pub_author))

#### Add group indicator ####
ccod.origin.long <- ccod.origin.long %>%
  left_join(labels) %>%
  rename(Origin = aesthetic.label) %>%
  mutate(Group = ifelse(grepl("nontbi", var), "Non-TBI",
                               ifelse(grepl("mtbi", var), "mTBI",
                                      ifelse(grepl("modtbi", var), "modTBI",
                                             ifelse(grepl("stbi", var), "sTBI", "Total")))))

#### Compute numbers of participants from various backgrounds per severity/grouping ####
ccod.origin.nontbi <- compute_origin_by_group(ccod.origin.long, "Non-TBI")
ccod.origin.mtbi <- compute_origin_by_group(ccod.origin.long, "mTBI")
ccod.origin.modtbi <- compute_origin_by_group(ccod.origin.long, "modTBI")
ccod.origin.stbi <- compute_origin_by_group(ccod.origin.long, "sTBI")
ccod.origin.total <- compute_origin_by_total(ccod.origin.long)

#### Recombine dataframes ####
ccod.origin.complete <- bind_rows(ccod.origin.nontbi, ccod.origin.mtbi, 
                                  ccod.origin.modtbi, ccod.origin.stbi, 
                                  ccod.origin.total) %>%
  ungroup() %>%
  select(record_id, n_nontbi:n_total, Group, Origin, participants) %>%
  spread(Origin, participants)

#### Summarise race/ethnicity/country of origin numbers ####
# Get total sample size for each study
ccod.origin.samplesizes <- ccod.origin.complete %>%
  select(record_id:n_total) %>%
  mutate(total_n = ifelse(!is.na(n_total), n_total, rowSums(.[2:6], na.rm = TRUE))) %>%
  select(record_id, total_n) %>%
  distinct()

# Compute total number of participants in each race/ethnicity/country group per study
ccod.origin.summary1a <- ccod.origin.complete %>%
  group_by(record_id) %>% 
  select(-n_nontbi:-Group) %>%
  summarise_at(vars(Aboriginal:Zimbabwean), funs(sum(., na.rm = TRUE))) %>%
  left_join(ccod.origin.samplesizes) %>%
  select(record_id, total_n, everything())

# Compute race/ethnicity/country groups as percentage of all participants
ccod.origin.summary1b <- ccod.origin.summary1a %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum) %>%
  gather(Origin, n) %>%
  arrange(desc(n)) %>%
  filter(n != 0.000) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes$total_n)*100, 3))

# Compute race/ethncity/country groups as percentage of participants per study
ccod.origin.summary1c <- ccod.origin.summary1a %>%
  group_by(record_id) %>%
  mutate_at(vars(Aboriginal:Zimbabwean), funs(round((./total_n)*100, 3)))



#### Assess Race/Ethnicity ####
# Separate race/ethnicity from country of origin
col.labels <- colnames(ccod.origin.summary1a)
coo.labels <- c("Australia or New Zealand", "Brazil", "Canada", "Cuba", "Dominican Republic",
                "Egypt", "India", "Indonesia", "Lebanon", "Mexico", "Netherlands", "Nicaragua", "Norway",
                "Puerto Rico", "Singapore", "Spain", "South America", "United Kingdom", "United States", 
                "US - English as primary\nlanguage", "US - Spanish as primary\nlanguage",
                "US (English or Spanish\nSpeaking)", "US or Canada", "Venezuela")

origin.labels <- setdiff(col.labels, coo.labels)
coo.indices <- match(coo.labels, col.labels)

# Summarise just race/ethnicity
ccod.origin.summary2 <- ccod.origin.summary1a %>%
  select(origin.labels) %>%
  group_by(record_id) %>%
  summarise_all(sum)

# Compute the number of articles reporting any given label
ccod.origin.summary2a <- ccod.origin.summary2 %>%
  select(-record_id, -total_n) %>%
  gather(Origin, n) %>%
  filter(n != 0.000) %>%
  group_by(Origin) %>%
  summarise(count = n())

ccod.origin.summary2b <- ccod.origin.summary2 %>%
  group_by(record_id) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n != 0.000) %>%
  distinct(record_id)

ccod.origin.samplesizes.race <- ccod.origin.samplesizes %>%
  filter(record_id %in% ccod.origin.summary2b$record_id)

# Compute race/ethnicity as percentage of all participants **without** country of origin
ccod.origin.summary2c <- ccod.origin.summary2 %>%
  ungroup() %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather(Origin, n) %>%
  arrange(desc(n)) %>%
  filter(n != 0.000) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes.race$total_n)*100, 3))

# Compute race/ethnicity as percentage per study **without** country of origin
# Filter for studies with at least 50% in one group
ccod.origin.summary2d <- ccod.origin.summary2 %>%
  group_by(record_id, total_n) %>%
  summarise_all(sum) %>%
  mutate_at(vars(Aboriginal:Zimbabwean), funs(round((./total_n)*100, 3))) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n >= 50) %>%
  group_by(Origin) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



#### Assess country of origin ####
# Separate out only countries of origin
ccod.origin.summary3 <- ccod.origin.summary1a %>%
  select(record_id, total_n, coo.labels) %>%
  group_by(record_id) %>%
  summarise_all(sum)

# Compute the number of articles reporting any given label
ccod.origin.summary3a <- ccod.origin.summary3 %>%
  select(-record_id, -total_n) %>%
  gather(Origin, n) %>%
  filter(n != 0.000) %>%
  group_by(Origin) %>%
  summarise(count = n())

ccod.origin.summary3b <- ccod.origin.summary3 %>%
  group_by(record_id) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n != 0.000) %>%
  distinct(record_id)

ccod.origin.summary3b %>% filter(record_id %in% ccod.origin.summary2b$record_id) %>% View()

ccod.origin.samplesizes.coo <- ccod.origin.samplesizes %>%
  filter(record_id %in% ccod.origin.summary3b$record_id)

# Compute participants per country as percentage of all participants **without
# respect to race/ethnicity**
ccod.origin.summary3c <- ccod.origin.summary3 %>%
  ungroup() %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather(Origin, n) %>%
  filter(n != 0.000) %>%
  arrange(desc(n)) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes.coo$total_n)*100, 3))

# Compute participants in each country for each study as a percentage **without
# respect to race/ethnicity**. Filter for studies with at least 50% of
# participants haling from one country.
ccod.origin.summary3d <- ccod.origin.summary3 %>%
  group_by(record_id, total_n) %>%
  summarise_all(sum) %>%
  mutate_at(vars(`Australia or New Zealand`:Venezuela), funs(round((./total_n)*100, 3))) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n >= 50) %>%
  group_by(Origin) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#### Write tables ####
origin.table <- ccod.origin.summary1c %>%
  ungroup() %>%
  left_join(ccod.articleinfo) %>%
  select(-pub_author:-pub_journal, -pub_doi) %>%
  select(record_id, pub_apa, pub_year, pop_country, everything()) %>%
  gather(Orig, n, -record_id, -pub_apa, -pub_year, -pop_country, -total_n) %>%
  filter(n != 0.000) %>%
  arrange(desc(n)) %>%
  mutate(n = round(n, 2)) %>% 
  unite(Origin, Orig, n, sep = ", ") %>%
  arrange(pub_apa, record_id)

origin.table.countryyear <- origin.table %>%
  select(record_id:pop_country) %>%
  distinct()

origin.distribution.table <- left_join(ccod.origin.summary2c, ccod.origin.summary2a) %>%
  left_join(ccod.origin.summary2d, by = "Origin")

coo.distribution.table <- left_join(ccod.origin.summary3c, ccod.origin.summary3a) %>%
  left_join(ccod.origin.summary3d, by = "Origin")
  

write_csv(origin.table, 
          path = "C:/Users/adamraikes/Documents/GitHub/CCOD/Tables/Origin.csv")

write_csv(origin.distribution.table,
          path = "C:/Users/adamraikes/Documents/GitHub/CCOD/Tables/Origin Distribution.csv")

write_csv(coo.distribution.table,
          path = "C:/Users/adamraikes/Documents/GitHub/CCOD/Tables/Country Distribution.csv")

write_csv(origin.table.countryyear,
          path = "./Tables/Origin Country and Year.csv")


  
