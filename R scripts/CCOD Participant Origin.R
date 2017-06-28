#### Header Information ####
# This script file is intended to provide some working code to summarise the
# various origins of individuals within the CCOD database. This provides one
# part of the backbone of the originating systematic review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Filter included articles to retain those articles which report some aspect
#### of race, ethnicity or country of origin. ####

ccod.origin <- ccod.included %>%
  filter(origin_reported == "Yes")

#### Remove extraneous columns ####
ccod.origin.limited <- ccod.origin %>%
  select(record_id:pub_title, n_nontbi:n_total, origin_analysis, n_orig_nontbi_perc:n_orig_totsamp_zimb) %>%
  select(-contains("complete"))

#### Convert to long format for manipulation ####
ccod.origin.long <- ccod.origin.limited %>%
  gather(Origin, count, c(-record_id:-origin_analysis, -n_orig_nontbi_perc,
                                 -n_orig_mtbi_perc, -n_orig_modtbi_perc, -n_orig_stbi_perc,
                                 -n_orig_totsamp_perc)) %>%
  arrange(desc(pub_author))

#### Add group indicator ####
ccod.origin.long <- ccod.origin.long %>%
  mutate(Group = ifelse(grepl("nontbi", Origin), "Non-TBI",
                               ifelse(grepl("mtbi", Origin), "mTBI",
                                      ifelse(grepl("modtbi", Origin), "modTBI",
                                             ifelse(grepl("stbi", Origin), "sTBI", "Total")))))

#### Convert origin names to more usable names. ####
ccod.origin.long$Origin <- gsub("^[^_]*_[^_]*_[^_]*_", "", ccod.origin.long$Origin)

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
ccod.origin.samplesizes <- ccod.origin.complete %>%
  select(record_id:n_total) %>%
  mutate(total_n = ifelse(!is.na(n_total), n_total, rowSums(.[2:6], na.rm = TRUE))) %>%
  select(record_id, total_n) %>%
  distinct()

ccod.origin.summary1a <- ccod.origin.complete %>%
  group_by(record_id) %>% 
  select(-n_nontbi:-Group) %>%
  summarise_at(vars(aboriginal:zimb), funs(sum)) %>%
  left_join(ccod.origin.samplesizes) %>%
  select(record_id, total_n, everything())

ccod.origin.summary1b <- ccod.origin.summary1a %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum) %>%
  gather(Origin, n) %>%
  arrange(desc(n)) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes$total_n)*100, 3))

# Separate race/ethnicity from country of origin
ccod.origin.summary2 <- ccod.origin.summary1a %>%
  select(-ausnz, -brazil, -canada, -cuba, -domrep, -egypt, -india,
                -indonesia, -lebanon, -mexico, -netherlands, -norway, -puertorico, -singapore, -spain, -uk, 
                -unitedstates, -uscanada, -usengl, -usenglspan, -usspanish, -venezuela) %>%
  group_by(record_id) %>%
  summarise_all(sum)

ccod.origin.summary2b <- ccod.origin.summary2 %>%
  group_by(record_id) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(!is.na(n)) %>%
  distinct(record_id)

ccod.origin.samplesizes.race <- ccod.origin.samplesizes %>%
  filter(record_id %in% ccod.origin.summary2b$record_id)

ccod.origin.summary2c <- ccod.origin.summary2 %>%
  ungroup() %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather(Origin, n) %>%
  arrange(desc(n)) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes.race$total_n)*100, 3))

ccod.origin.summary2d <- ccod.origin.summary2 %>%
  group_by(record_id, total_n) %>%
  summarise_all(sum) %>%
  mutate_at(vars(aboriginal:zimb), funs(round((./total_n)*100, 3))) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n >= 70) %>%
  group_by(Origin) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#### Assess country of origin ####
ccod.origin.summary3 <- ccod.origin.summary1a %>%
  select(record_id, total_n, ausnz, brazil, canada, cuba, domrep, egypt, india,
                indonesia, lebanon, mexico, netherlands, norway, puertorico, singapore, spain, uk, 
                unitedstates, uscanada, usengl, usenglspan, usspanish, venezuela) %>%
  group_by(record_id) %>%
  summarise_all(sum)

ccod.origin.summary3b <- ccod.origin.summary3 %>%
  group_by(record_id) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(!is.na(n)) %>%
  distinct(record_id)

ccod.origin.summary3b %>% filter(record_id %in% ccod.origin.summary2b$record_id) %>% View()

ccod.origin.samplesizes.coo <- ccod.origin.samplesizes %>%
  filter(record_id %in% ccod.origin.summary3b$record_id)

ccod.origin.summary3c <- ccod.origin.summary3 %>%
  ungroup() %>%
  select(-record_id, -total_n) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  gather(Origin, n) %>%
  arrange(desc(n)) %>%
  mutate(proportion = round(n/sum(ccod.origin.samplesizes.coo$total_n)*100, 3))

ccod.origin.summary3d <- ccod.origin.summary3 %>%
  group_by(record_id, total_n) %>%
  summarise_all(sum) %>%
  mutate_at(vars(ausnz:venezuela), funs(round((./total_n)*100, 3))) %>%
  select(-total_n) %>%
  gather(Origin, n, -record_id) %>%
  filter(n >= 50) %>%
  group_by(Origin) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



  
