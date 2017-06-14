#### Header Information ####
# This is the initializing script file for the CCOD database. It is the first
# effort to make certain that the data is accessible from the REDCap API. It may
# also serve as the backbone of the initial article, which will be the Race,
# Ethnicity, CoO, and Language Demographic Review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Source files ####
#library(REDCapR)
source("R scripts/CCOD Function Definitions.R")

#### Call REDCap API ####
# As this repository is public, the API key should be pasted into this spot when
# run and then removed so that it is not persistently available.

redcap.uri <- "https://redcap.cehs.usu.edu/api/"
redcap.key <- "795C0CFA3E0445A3AE2C876F37ADC1EA"
ccod.database <- redcapImport(uri = redcap.uri, key = redcap.key)
#ccod.database <- redcap_read(redcap_uri = redcap.uri, token = redcap.key)

rm(redcap.key)

#### Load Libraries ####
library(magrittr)

#### Filter database for included articles ####
ccod.included <- ccod.database %>%
  dplyr::filter(text_scrn_include == "Yes - include" | text_scrn_include == "Kindof - no tests specific outcome??")





ccod.lang <- ccod.included %>%
  dplyr::filter(lang_reported == "Yes")



