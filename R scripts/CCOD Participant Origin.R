#### Header Information ####
# This script file is intended to provide some working code to summarise the
# various origins of individuals within the CCOD database. This provides one
# part of the backbone of the originating systematic review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Filter included articles to retain those articles which report some aspect
#### of race, ethnicity or country of origin. ####

ccod.origin <- ccod.included %>%
  dplyr::filter(origin_reported == "Yes")

#### Remove extraneous columns ####
ccod.origin.limited <- ccod.origin %>%
  dplyr::select(record_id:pub_title, n_nontbi:n_total, origin_analysis, n_orig_nontbi_perc:n_orig_totsamp_zimb) %>%
  dplyr::select(-dplyr::contains("complete"))

#### Convert to long format for manipulation ####
ccod.origin.long <- ccod.origin.limited %>%
  tidyr::gather(Origin, count, c(-record_id:-origin_analysis, -n_orig_nontbi_perc,
                                 -n_orig_mtbi_perc, -n_orig_modtbi_perc, -n_orig_stbi_perc,
                                 -n_orig_totsamp_perc)) %>%
  dplyr::arrange(desc(pub_author))

#### Add group indicator ####
ccod.origin.long <- ccod.origin.long %>%
  dplyr::mutate(Group = ifelse(grepl("nontbi", Origin), "Non-TBI",
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
ccod.origin.complete <- dplyr::bind_rows(ccod.origin.nontbi, ccod.origin.mtbi, 
                                  ccod.origin.modtbi, ccod.origin.stbi, 
                                  ccod.origin.total) %>%
  dplyr::ungroup() %>%
  dplyr::select(record_id, n_nontbi:n_total, Group, Origin, participants) %>%
  tidyr::spread(Origin, participants)

#### Summarise race/ethnicity/country of origin numbers ####
ccod.origin.samplesizes <- ccod.origin.complete %>%
  dplyr::select(record_id:n_total) %>%
  dplyr::mutate(total_n = ifelse(!is.na(n_total), n_total, rowSums(.[2:6], na.rm = TRUE))) %>%
  dplyr::select(record_id, total_n) %>%
  dplyr::distinct()

ccod.origin.summary1a <- ccod.origin.complete %>%
  dplyr::group_by(record_id) %>% 
  dplyr::select(-n_nontbi:-Group) %>%
  dplyr::summarise_at(dplyr::vars(aboriginal:zimb), dplyr::funs(sum)) %>%
  dplyr::left_join(ccod.origin.samplesizes) %>%
  dplyr::select(record_id, total_n, dplyr::everything())

ccod.origin.summary1b <- ccod.origin.summary1a %>%
  dplyr::ungroup() %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-record_id, -total_n) %>%
  dplyr::summarise_all(sum) %>%
  tidyr::gather(Origin, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(ccod.origin.samplesizes$total_n)*100, 3))

# Separate race/ethnicity from country of origin
ccod.origin.summary2 <- ccod.origin.summary1a %>%
  dplyr::select(-ausnz, -brazil, -canada, -cuba, -domrep, -egypt, -india,
                -indonesia, -lebanon, -mexico, -netherlands, -norway, -puertorico, -singapore, -spain, -uk, 
                -unitedstates, -uscanada, -usengl, -usenglspan, -usspanish, -venezuela) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise_all(sum)

ccod.origin.summary2b <- ccod.origin.summary2 %>%
  dplyr::group_by(record_id) %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Origin, n, -record_id) %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::distinct(record_id)

ccod.origin.samplesizes.race <- ccod.origin.samplesizes %>%
  dplyr::filter(record_id %in% ccod.origin.summary2b$record_id)

ccod.origin.summary2c <- ccod.origin.summary2 %>%
  dplyr::ungroup() %>%
  dplyr::select(-record_id, -total_n) %>%
  dplyr::summarise_all(sum, na.rm = TRUE) %>%
  tidyr::gather(Origin, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(ccod.origin.samplesizes.race$total_n)*100, 3))

ccod.origin.summary2d <- ccod.origin.summary2 %>%
  dplyr::group_by(record_id, total_n) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::mutate_at(dplyr::vars(aboriginal:zimb), dplyr::funs(round((./total_n)*100, 3))) %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Origin, n, -record_id) %>%
  dplyr::filter(n >= 70) %>%
  dplyr::group_by(Origin) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))

#### Assess country of origin ####
ccod.origin.summary3 <- ccod.origin.summary1a %>%
  dplyr::select(record_id, total_n, ausnz, brazil, canada, cuba, domrep, egypt, india,
                indonesia, lebanon, mexico, netherlands, norway, puertorico, singapore, spain, uk, 
                unitedstates, uscanada, usengl, usenglspan, usspanish, venezuela) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise_all(sum)

ccod.origin.summary3b <- ccod.origin.summary3 %>%
  dplyr::group_by(record_id) %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Origin, n, -record_id) %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::distinct(record_id)

ccod.origin.summary3b %>% dplyr::filter(record_id %in% ccod.origin.summary2b$record_id) %>% View()

ccod.origin.samplesizes.coo <- ccod.origin.samplesizes %>%
  dplyr::filter(record_id %in% ccod.origin.summary3b$record_id)

ccod.origin.summary3c <- ccod.origin.summary3 %>%
  dplyr::ungroup() %>%
  dplyr::select(-record_id, -total_n) %>%
  dplyr::summarise_all(sum, na.rm = TRUE) %>%
  tidyr::gather(Origin, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(ccod.origin.samplesizes.coo$total_n)*100, 3))

ccod.origin.summary3d <- ccod.origin.summary3 %>%
  dplyr::group_by(record_id, total_n) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::mutate_at(dplyr::vars(ausnz:venezuela), dplyr::funs(round((./total_n)*100, 3))) %>%
  dplyr::select(-total_n) %>%
  tidyr::gather(Origin, n, -record_id) %>%
  dplyr::filter(n >= 50) %>%
  dplyr::group_by(Origin) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))



  
