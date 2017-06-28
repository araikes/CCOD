#### Header Information ####

#### Function Definitions ####

#### Redcap Import ####
redcapImport <- function(uri, key) {
  # Special thanks Will Gray for the redcapExport function and JoAnn Rudd
  # Alvarez for the code for this function. This code was available publicly
  # online at
  # http://biostat.mc.vanderbilt.edu/wiki/pub/Main/JoAnnAlvarez/api.pdf.
  require(RCurl)
  
  source("./R scripts/API Import.R")
  chartAbstraction <- redcapExport(APIKEY = key,
                                   URI = uri,
                                   labels = TRUE,
                                   forms = NULL)
}

compute_origin_by_group <- function(x, severity) {
  reporting.method <- switch(severity,
                             "Non-TBI" = rlang::quo(n_orig_nontbi_perc),
                             "mTBI" = rlang::quo(n_orig_mtbi_perc),
                             "modTBI" = rlang::quo(n_orig_modtbi_perc),
                             "sTBI" = rlang::quo(n_orig_stbi_perc))
  
  severity.n <- switch(severity,
                       "Non-TBI" = rlang::quo(n_nontbi),
                       "mTBI" = rlang::quo(n_tbi_mild),
                       "modTBI" = rlang::quo(n_tbi_mod),
                       "sTBI" = rlang::quo(n_tbi_sev))

  x <- x %>%
    dplyr::filter(Group == severity) 
  
  x.percs <- x %>%
    dplyr::filter(rlang::UQ(reporting.method) == "Percentage") %>%
    dplyr::mutate(participants = round((count/100) * rlang::UQ(severity.n), 0))
  
  x.nums <- x %>%
    dplyr::filter(rlang::UQ(reporting.method) == "Whole numbers") %>%
    dplyr::rename(participants = count)
  
  x.complete <- dplyr::bind_rows(x.percs, x.nums)
  
  return(x.complete)
}

compute_origin_by_total <- function(x) {
  x <- x %>%
    dplyr::filter(Group == "Total")
  
  x.percs <- x %>%
    dplyr::filter(n_orig_totsamp_perc == "Percentage") %>%
    dplyr::mutate(participants = ifelse(!is.na(n_total),
                                        round((count/100)*n_total, 0),
                                        round((count/100)*(rowSums(.[5:9], na.rm = TRUE)), 0)))
  
  x.nums <- x %>%
    dplyr::filter(n_orig_totsamp_perc == "Whole numbers") %>%
    dplyr::rename(participants = count)
  
  x.complete <- dplyr::bind_rows(x.percs, x.nums)
  
  return(x.complete)
}

compute_lang_by_group <- function(x, severity) {
  reporting.method <- switch(severity,
                             "Non-TBI" = rlang::quo(n_lang_nontbi_perc),
                             "mTBI" = rlang::quo(n_lang_mtbi_perc),
                             "modTBI" = rlang::quo(n_lang_modtbi_perc),
                             "sTBI" = rlang::quo(n_lang_stbi_perc))
  
  severity.n <- switch(severity,
                       "Non-TBI" = rlang::quo(n_nontbi),
                       "mTBI" = rlang::quo(n_tbi_mild),
                       "modTBI" = rlang::quo(n_tbi_mod),
                       "sTBI" = rlang::quo(n_tbi_sev))
  
  x <- x %>%
    dplyr::filter(Group == severity) 
  
  x.percs <- x %>%
    dplyr::filter(rlang::UQ(reporting.method) == "Percentage") %>%
    dplyr::mutate(participants = round((count/100) * rlang::UQ(severity.n), 0))
  
  x.nums <- x %>%
    dplyr::filter(rlang::UQ(reporting.method) == "Whole numbers") %>%
    dplyr::rename(participants = count)
  
  x.complete <- dplyr::bind_rows(x.percs, x.nums)
  
  return(x.complete)
}

compute_lang_by_total <- function(x) {
  x <- x %>%
    dplyr::filter(Group == "Total")
  
  x.percs <- x %>%
    dplyr::filter(n_lang_totsamp_perc == "Percentage") %>%
    dplyr::mutate(participants = ifelse(!is.na(n_total),
                                        round((count/100)*n_total, 0),
                                        round((count/100)*(rowSums(.[5:9], na.rm = TRUE)), 0)))
  
  x.nums <- x %>%
    dplyr::filter(n_lang_totsamp_perc == "Whole numbers") %>%
    dplyr::rename(participants = count)
  
  x.complete <- dplyr::bind_rows(x.percs, x.nums)
  
  return(x.complete)
}
