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

#### Separate whole numbers from percentage to facilitate manipulation. These
#### will be recombined after disentangling the percentages ####

ccod.origin.percs <- ccod.origin.long %>%
  dplyr::filter(n_orig_nontbi_perc == "Percentage" |
                  n_orig_mtbi_perc == "Percentage" | 
                  n_orig_modtbi_perc == "Percentage" | 
                  n_orig_stbi_perc == "Percentage" |
                  n_orig_totsamp_perc == "Percentage") %>%
  dplyr::mutate(participants = ifelse(Group == "Non-TBI" & !is.na(n_orig_nontbi_perc), 
                                      round((count/100)*n_nontbi,0), NA)) %>%
  dplyr::mutate(participants = ifelse(Group == "mTBI" & !is.na(n_orig_mtbi_perc),
                                      round((count/100)*n_tbi_mild,0), participants)) %>%
  dplyr::mutate(participants = ifelse(Group == "modTBI" & !is.na(n_orig_modtbi_perc),
                                      round((count/100)*n_tbi_mod,0), participants)) %>%
  dplyr::mutate(participants = ifelse(Group == "sTBI" & !is.na(n_orig_stbi_perc),
                                      round((count/100)*n_tbi_sev,0), participants)) %>%
  dplyr::mutate(participants = ifelse(Group == "Total" & !is.na(n_orig_totsamp_perc) & !is.na(n_total),
                                      round((count/100)*n_total,0), participants)) %>%
  dplyr::mutate(check = ifelse(!is.na(count) & is.na(participants), "Error", NA)) %>%
  dplyr::filter(!is.na(check))
          