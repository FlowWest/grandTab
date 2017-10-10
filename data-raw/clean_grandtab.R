library(tidyverse)
library(stringr)

# jessica compiled data 1975-2015
gt <- read_csv('data-raw/grandtab.csv', skip = 3)

gt %>%
  filter(DATA_COLLECTION_TYPE == 'In-River Count') %>%
  group_by(RIVER_SYSTEM, RIVER, REACH) %>%
  filter(!is.na(REACH)) %>%
  summarise(count = n()) %>% View()
  # glimpse()
# sacramento is funky with reach river combo
# what is 'Other' RIVER
# Thomes misspelled Toomes

gt %>%
  mutate(river = case_when(
    RIVER == 'Toomes Creek' ~ 'Thomes Creek',
    RIVER == 'Sacramento River Main Stem' & REACH == 'Keswick Dam to RBDD' ~ 'Upper Sacramento River',
    RIVER == 'Sacramento River Main Stem' & REACH == 'RBDD to Princeton Ferry' ~ 'Upper-mid Sacramento River',
    TRUE ~ RIVER
  )) %>%
  filter(DATA_COLLECTION_TYPE == 'In-River Count') %>%
  group_by(RIVER_SYSTEM, river, REACH) %>%
  filter(!is.na(REACH)) %>%
  summarise(count = n()) %>% View()

gt %>%
  filter(RIVER == 'Sacramento River Main Stem', RUN == 'Fall',
         DATA_COLLECTION_TYPE == 'In-River Count') %>%
  select(year = YEAR, type = DATA_COLLECTION_TYPE, river = RIVER,
         reach = REACH, count = ESTIMATE, run = RUN) %>%
  ggplot(aes(x = year, y = count, fill = reach)) +
  geom_col(position = 'dodge')


gt <- grandtab %>%
  mutate(watershed = ifelse(RIVER == 'Sacramento River Main Stem', 'Upper Sacramento River', RIVER)) %>%
  filter(watershed %in% reaches, DATA_COLLECTION_TYPE %in% c('In-River Count', 'Hatchery')) %>%
  mutate(DATA_COLLECTION_TYPE = replace(DATA_COLLECTION_TYPE, DATA_COLLECTION_TYPE == 'In-River Count', 'Natural')) %>%
  select(year = YEAR, watershed, count = ESTIMATE, run = RUN, type = DATA_COLLECTION_TYPE)


#compare to sac pass data from adam durante 2003-2016
gt2 <- read_csv('data-raw/GrandTab2003_2016eachTrib.csv') %>%
  gather(year, count, -Watershed.full) %>%
  rename(watershed = Watershed.full) %>%
  arrange(year)

gt2
