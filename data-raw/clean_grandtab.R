library(tidyverse)
library(stringr)

#new data from jessica goes back further
gt <- read_csv('data-raw/grandtab.csv', skip = 3)

gt %>%
  filter(DATA_COLLECTION_TYPE == 'In-River Count')
  glimpse()


gt <- grandtab %>%
  mutate(watershed = ifelse(RIVER == 'Sacramento River Main Stem', 'Upper Sacramento River', RIVER)) %>%
  filter(watershed %in% reaches, DATA_COLLECTION_TYPE %in% c('In-River Count', 'Hatchery')) %>%
  mutate(DATA_COLLECTION_TYPE = replace(DATA_COLLECTION_TYPE, DATA_COLLECTION_TYPE == 'In-River Count', 'Natural')) %>%
  select(year = YEAR, watershed, count = ESTIMATE, run = RUN, type = DATA_COLLECTION_TYPE)


