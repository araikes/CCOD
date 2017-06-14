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
  dplyr::select(record_id, Group, Origin, participants) %>%
  tidyr::spread(Origin, participants)

#### Summarise race/ethnicity/country of origin numbers ####
ccod.origin.summary1a <- ccod.origin.complete %>%
  dplyr::group_by(record_id) %>% 
  dplyr::select(-Group) %>%
  dplyr::summarise_all(sum)


ccod.origin.summary1b <- ccod.origin.summary1a %>%
  dplyr::ungroup() %>%
  dplyr::select(-record_id) %>%
  replace(is.na(.), 0) %>%
  dplyr::summarise_all(sum) %>%
  tidyr::gather(Origin, n) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(proportion = round(n/sum(n)*100, 3))

