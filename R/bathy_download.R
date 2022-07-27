# Bathy loading

library(tidyverse)
library(marmap)
source("R/getNOAA.ice.bathy.R")

# Download bathy
bathy <- getNOAA.ice.bathy(lon1 = -110, lon2 = -40, lat1 = 60, lat2 = 85,
                           resolution = 2, keep = T, path = "data/bathy/")

# Convert to dataframe
bathy_df <- bathy %>%
  fortify() %>%
  rename(lon = x, lat = y, depth = z)

# Write csv
write_csv(bathy_df, file = "data/bathy/bathy_BaffinBay.csv")
