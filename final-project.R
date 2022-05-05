library(tidyverse)
data_all <- read_csv("basketball data.csv")
data_all <- Filter(function(x)!all(is.na(x)), data_all) #remove empty columns
data_p <- data_all %>% filter(endsWith(Team,"*")) %>%
  select(everything()) %>%
  mutate(Team = str_replace(Team,"\\*",""))

data_all <- data_all %>%
  mutate(Team = str_replace(Team,"\\*",""))

#Rk is sorted by NRtg
#Help from https://stackoverflow.com/questions/22104962/how-to-remove-empty-columns-in-r/27098520