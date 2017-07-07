#### Header Information ####
# This is the initializing script file for the CCOD database. It is the first
# effort to make certain that the data is accessible from the REDCap API. It may
# also serve as the backbone of the initial article, which will be the Race,
# Ethnicity, CoO, and Language Demographic Review.

# Author: Adam Raikes
# Initial Date: 06/08/2017

#### Source files ####
source("R scripts/CCOD Function Definitions.R")

#### Call REDCap API ####
# As this repository is public, the API key should be pasted into this spot when
# run and then removed so that it is not persistently available.

redcap.uri <- "https://redcap.cehs.usu.edu/api/"
redcap.key <- ""
ccod.database <- redcapImport(uri = redcap.uri, key = redcap.key)

rm(redcap.key)

#### Load Libraries ####
library(tidyverse)
library(stringr)

#### Filter database for included articles ####
ccod.included <- ccod.database %>%
  filter(text_scrn_include == "Yes - include" | text_scrn_include == "Kindof - no tests specific outcome??")

ccod.articleinfo <- ccod.included %>%
  select(record_id:pub_doi)

#### Evaluate for issues of critical missingness ####
ccod.included %>%
  filter(is.na(origin_reported)) %>%
  View()

ccod.included %>%
  filter(is.na(lang_reported)) %>%
  View()

#### Get labels for languages and groups ####
var.width <- 30
labels <- data.frame(var = colnames(ccod.included), label = get_label(ccod.included)) %>%
  mutate(aesthetic.label = str_wrap(label, width = var.width)) %>%
  select(-label)
rownames(labels) <- c()

