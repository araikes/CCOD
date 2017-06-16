#### Header Information #### This script file is intended to provide some
#working code to summarise the outcomes from CCOD Participant Origin.R and CCOD
#Participant Language.R

# Author: Adam Raikes
# Initial Date: 06/15/2017

# Identify the number of sources reporting race/ethnicity/CoO or language
dplyr::bind_rows(ccod.lang, ccod.origin) %>% 
  dplyr::select(record_id) %>% 
  dplyr::distinct(record_id) %>% 
  dplyr::summarise(count = n()) %>%
  View()

# Identify the number of sources reporting both race/ethncitiy/CoO and language
dplyr::bind_rows(ccod.lang, ccod.origin) %>%
  dplyr::select(record_id) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count > 1) %>%
  dplyr::ungroup() %>%
  View()

#### Race/Language analyses ####
ccod.demographic.analyses <- dplyr::bind_rows(ccod.lang, ccod.origin) %>%
  dplyr::select(record_id, origin_analysis, lang_analysis) %>%
  dplyr::distinct() %>%
  dplyr::filter(origin_analysis == "Yes" | lang_analysis == "Yes") 

ccod.origin.analyses <- dplyr::bind_rows(ccod.lang, ccod.origin) %>%
  dplyr::select(record_id, origin_analysis, lang_analysis) %>%
  dplyr::distinct() %>%
  dplyr::filter(origin_analysis == "Yes") %>%
  dplyr::left_join(ccod.articleinfo) %>%
  dplyr::select(record_id, pub_year:pub_doi, origin_analysis)

ccod.lang.analyses <- dplyr::bind_rows(ccod.lang, ccod.origin) %>%
  dplyr::select(record_id, origin_analysis, lang_analysis) %>%
  dplyr::distinct() %>%
  dplyr::filter(lang_analysis == "Yes") %>%
  dplyr::left_join(ccod.articleinfo) %>%
  dplyr::select(record_id, pub_year:pub_doi, lang_analysis)



