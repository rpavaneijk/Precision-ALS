library('tidyverse')
#Is the age at ddiagnosis different at each site?
ext_main %>% 
  filter(site != 'Karolinsk') %>% #Removing for now to investigate why
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram() +
  facet_wrap(~site)

#I almost prefer it without the facet wrap actually, clearer image that not that different
ext_main %>% 
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram()

#With c9, is it different?
ext_main %>% 
filter(site != 'Karolinsk') %>% 
 filter(., c9orf72_stat_BOOL == TRUE) %>% 
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
         geom_histogram() +
  facet_wrap(~site)

#Add a median v line
ext_main %>% 
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram()
 
