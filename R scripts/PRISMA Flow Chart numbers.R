fulltext.scrn <- ccod.database %>% 
  group_by(text_scrn_include) %>% 
  summarise(count = n()) %>% 
  filter(!is.na(text_scrn_include))

fulltext.filt <- fulltext.scrn %>%
  mutate(status = ifelse(text_scrn_include == "Yes - include" | text_scrn_include == "Kindof - no tests specific outcome??",
                         "Include", "Exclude")) %>%
  group_by(status) %>%
  summarise(totals = sum(count))

