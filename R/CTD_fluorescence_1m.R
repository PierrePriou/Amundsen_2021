# Calculate mean fluorescence per 1m depth bin

# Load packages
library(tidyverse)

# Load processed raw data
load("C:/Users/cfer/Raw data PhD/Amundsen 2021/CTD/DarkEdge_CTD.RData")

fluo <- CTD_1m %>%
  ungroup() %>%
  # # Select relevent station 25
  # filter(CTD_id %in% c("02", "13", "25", "26", "28", "29", "40")) %>%
  # Select fluorescence
  dplyr::select(CTD_id, depth_round, fluo) %>%
  # Tidy variables
  mutate(CTD_id = as.numeric(CTD_id)) %>%
  # Rename variables
  rename(cast_number = CTD_id,
         depth = depth_round)

# Write csv
write_csv(fluo, file = "data/CTD/CTD_fluorescence_1m.csv")
