#### Header Information #### This script file is intended to provide some
#working code to summarise the outcomes from CCOD Participant Origin.R and CCOD
#Participant Language.R

# Author: Adam Raikes
# Initial Date: 06/15/2017

# Identify the number of sources reporting race/ethnicity/CoO or language
bind_rows(ccod.lang, ccod.origin) %>% 
  select(record_id) %>% 
  distinct(record_id) %>% 
  summarise(count = n()) %>%
  View()

# Identify the number of sources reporting both race/ethncitiy/CoO and language
bind_rows(ccod.lang, ccod.origin) %>%
  select(record_id) %>%
  group_by(record_id) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() %>%
  left_join(ccod.articleinfo) %>%
  View()

#### Race/Language analyses ####
ccod.demographic.analyses <- bind_rows(ccod.lang, ccod.origin) %>%
  select(record_id, origin_analysis, lang_analysis) %>%
  distinct() %>%
  filter(origin_analysis == "Yes" | lang_analysis == "Yes") 

ccod.origin.analyses <- bind_rows(ccod.lang, ccod.origin) %>%
  select(record_id, origin_analysis, lang_analysis) %>%
  distinct() %>%
  filter(origin_analysis == "Yes") %>%
  left_join(ccod.articleinfo) %>%
  select(record_id, pub_year:pub_doi, origin_analysis)

ccod.lang.analyses <- bind_rows(ccod.lang, ccod.origin) %>%
  select(record_id, origin_analysis, lang_analysis) %>%
  distinct() %>%
  filter(lang_analysis == "Yes") %>%
  left_join(ccod.articleinfo) %>%
  select(record_id, pub_year:pub_doi, lang_analysis)



