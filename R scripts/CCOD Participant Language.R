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
  dplyr::select(record_id, Group, Language, participants) %>%
  tidyr::spread(Language, participants)

#### Summarise language numbers ####
ccod.lang.summary1a <- ccod.lang.complete %>%
  dplyr::group_by(record_id) %>%
  dplyr::select(-Group) %>%
  dplyr::summarise_all(sum)

ccod.lang.summary1b <- ccod.lang.summary1a %>%
  dplyr::ungroup() %>%
  dplyr::select(-record_id) %>%
  replace(is.na(.), 0) %>%
  dplyr::summarise_all(sum) %>%
  tidyr::gather(Language, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(n)*100, 3))

ccod.lang.summary2 <- ccod.lang.summary1a %>%
  dplyr::mutate(total_n = rowSums(.[2:28], na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(dplyr::vars(both:swedish), dplyr::funs(round((./total_n)*100, 3)))

ccod.lang.summary2b <- ccod.lang.summary2  %>%
  dplyr::filter(english >= 75.000)

ccod.lang.summary2c <- ccod.lang.summary2b %>%
  dplyr::filter(english == 100.000)

  
