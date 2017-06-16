#### Header Information ####
# This script file is intended to provide some working code to summarise the
# various languages of individuals within the CCOD database. This provides one
# part of the backbone of the langating systematic review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Filter included articles to retain those articles which report some aspect
#### of language. ####

ccod.lang <- ccod.included %>%
  dplyr::filter(lang_reported == "Yes")

#### Retain necessary columns ####
ccod.lang.limited <- ccod.lang %>%
  dplyr::select(record_id:pub_title, n_nontbi:n_total, lang_analysis, 
                n_lang_nontbi_perc:total_sample_language_complete) %>%
  dplyr::select(-dplyr::contains("complete"))

#### Convert to long format for manipulation ####
ccod.lang.long <- ccod.lang.limited %>%
  tidyr::gather(Language, count, c(-record_id:-lang_analysis, -n_lang_nontbi_perc,
                                 -n_lang_mtbi_perc, -n_lang_modtbi_perc, -n_lang_stbi_perc,
                                 -n_lang_totsamp_perc)) %>%
  dplyr::arrange(desc(pub_author))

#### Add group indicator ####
ccod.lang.long <- ccod.lang.long %>%
  dplyr::mutate(Group = ifelse(grepl("nontbi", Language), "Non-TBI",
                               ifelse(grepl("mtbi", Language), "mTBI",
                                      ifelse(grepl("modtbi", Language), "modTBI",
                                             ifelse(grepl("stbi", Language), "sTBI", "Total")))))

#### Convert lang names to more usable names. ####
ccod.lang.long$Language <- gsub("^[^_]*_[^_]*_[^_]*_", "", ccod.lang.long$Language)

#### Compute numbers of participants from various backgrounds per severity/grouping ####
ccod.lang.nontbi <- compute_lang_by_group(ccod.lang.long, "Non-TBI")
ccod.lang.mtbi <- compute_lang_by_group(ccod.lang.long, "mTBI")
ccod.lang.modtbi <- compute_lang_by_group(ccod.lang.long, "modTBI")
ccod.lang.stbi <- compute_lang_by_group(ccod.lang.long, "sTBI")
ccod.lang.total <- compute_lang_by_total(ccod.lang.long)

#### Recombine dataframe ####
ccod.lang.complete <- dplyr::bind_rows(ccod.lang.nontbi, ccod.lang.mtbi, 
                                         ccod.lang.modtbi, ccod.lang.stbi, 
                                         ccod.lang.total) %>%
  dplyr::ungroup() %>%
  dplyr::select(record_id, n_nontbi:n_total, Group, Language, participants) %>%
  tidyr::spread(Language, participants)

#### Summarise language numbers ####
ccod.lang.samplesizes <- ccod.lang.complete %>%
  dplyr::select(record_id:n_total) %>%
  dplyr::mutate(total_n = ifelse(!is.na(n_total), n_total, rowSums(.[2:6], na.rm = TRUE))) %>%
  dplyr::select(record_id, total_n) %>%
  dplyr::distinct()

ccod.lang.summary1a <- ccod.lang.complete %>%
  dplyr::group_by(record_id) %>%
  dplyr::select(-n_nontbi:-Group) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::left_join(ccod.lang.samplesizes) %>%
  dplyr::select(record_id, total_n, dplyr::everything())

ccod.lang.summary1b <- ccod.lang.summary1a %>%
  dplyr::ungroup() %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-record_id, -total_n) %>%
  dplyr::summarise_all(sum) %>%
  tidyr::gather(lang, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(ccod.lang.samplesizes$total_n)*100, 3))

ccod.lang.summary2 <- ccod.lang.summary1a %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(both:swedish), dplyr::funs(round((./total_n)*100, 3)))

ccod.lang.summary2b <- ccod.lang.summary2  %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Language, n, -record_id) %>%
  dplyr::filter(n >= 75.000)

ccod.lang.summary2c <- ccod.lang.summary2b %>%
  dplyr::filter(n == 100.000) %>%
  dplyr::group_by(Language) %>%
  dplyr::summarise(count = n())

ccod.lang.summary2d <- ccod.lang.summary2 %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Language, n, -record_id) %>%
  dplyr::filter(Language == "english") %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::arrange(desc(n))

ccod.lang.summary3 <- ccod.lang.summary2 %>%
  dplyr::select(-record_id, -total_n) %>%
  tidyr::gather(Language, n) %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::group_by(Language) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))

ccod.lang.summary4 <- ccod.lang.summary1a %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Language, n, -record_id) %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))

  
