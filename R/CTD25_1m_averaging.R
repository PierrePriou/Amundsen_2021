# CTD25 from the processed Amundsen data contains mostly NaNs so I use the raw 
# data to calculate the averaged data over 1 m bins

# Load packages
library(tidyverse)

# Load processed raw data
load("C:/Users/cfer/Raw data PhD/Amundsen 2021/CTD/DarkEdge_CTD.RData")

CTD25 <- CTD %>%
  ungroup() %>%
  # Select cast 25
  filter(CTD_id == "2105025") %>%
  # Find min date
  mutate(date_start_CTD = min(date),
         # Add 601 m data
         cast2 = if_else(date <= ymd_hms("2021-10-21 10:30:13", tz = "UTC"),
                         "downcast", "upcast")) %>%
  filter(cast2 == "downcast") %>%
  # Rename variables
  rename(PRES_25 = pressure,
         TE90_25 = temp_its90,
         PSAL_25 = sal_practical, 
         FLOR_25 = fluo,
         SIGT_25 = sigmaT, 
         SIGP_25 = sigmatheta) %>%
  select(date_start_CTD, depth, PRES_25, TE90_25, PSAL_25, FLOR_25, SIGT_25, SIGP_25) %>%
  # Summarise in 1 m bin
  mutate(depth = round(depth)) %>%
  group_by(date_start_CTD, depth) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  # Add other variables used to combine the data
  mutate(station = "DE420",
         cast_number = 25)

# Write csv
write_csv(CTD25, file = "data/CTD/CTD25_1m.csv")
