library(tidyverse)
library(stringr)

cvpia_watershed <- read_csv('data-raw/cvpia_trib_names.csv') %>% pull(watershed)

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

grandtab <- gt %>%
  mutate(watershed = case_when(
    RIVER == 'Toomes Creek' ~ 'Thomes Creek',
    RIVER == 'Stoney Creek' ~ 'Stony Creek',
    RIVER == 'Sacramento River Main Stem' & REACH == 'Keswick Dam to RBDD' ~ 'Upper Sacramento River',
    RIVER == 'Sacramento River Main Stem' & REACH == 'RBDD to Princeton Ferry' ~ 'Upper-mid Sacramento River',
    TRUE ~ RIVER),
    DATA_COLLECTION_TYPE =
      replace(DATA_COLLECTION_TYPE, DATA_COLLECTION_TYPE == 'In-River Count', 'Natural')) %>%
  filter(watershed %in% cvpia_watershed) %>%
  select(year = YEAR, watershed, count = ESTIMATE, run = RUN, type = DATA_COLLECTION_TYPE)

use_data(grandtab)


gt %>%
  filter(RIVER == 'Sacramento River Main Stem', RUN == 'Fall',
         DATA_COLLECTION_TYPE == 'In-River Count') %>%
  select(year = YEAR, type = DATA_COLLECTION_TYPE, river = RIVER,
         reach = REACH, count = ESTIMATE, run = RUN) %>%
  ggplot(aes(x = year, y = count, fill = reach)) +
  geom_col(position = 'dodge')



#compare to sac pass data from adam durante 2003-2016
gt2 <- read_csv('data-raw/GrandTab2003_2016eachTrib.csv') %>%
  gather(year, count, -Watershed.full) %>%
  rename(watershed = Watershed.full) %>%
  arrange(year)

gt2
