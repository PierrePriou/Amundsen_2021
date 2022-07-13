# Script that reads processed C-OPS data and digest it in a more readable format

# Load packages
library(tidyverse)

# List files
file_list <- list.files("data/C-OPS/processed/", pattern = ".RData", full.names = T) 

# Empty dataframe
COPS <- data.frame()

# Loop through each file per year
for (i in file_list) { 
  load(i)
  # Extract depth
  depth_tmp <- bind_cols(cops$depth.fitted) %>%
    as_tibble() %>%
    rename(depth = "...1")
  # Extract mean surface irradiance (Ed0.0p) (sensor on deck)
  ed0_tmp <- bind_cols(cops$Ed0.waves, cops$Ed0.0p) %>% 
    as_tibble() %>%
    rename(wavelength = "...1",
           mean_Ed0 = "...2")
  # Extract mean downwelling irradiance below surface (Ed0.0p) and detection levels
  edz_0m_tmp <- bind_cols(cops$EdZ.waves, cops$EdZ.0m, cops$EdZ.detection.limit) %>% 
    as_tibble() %>%
    rename(wavelength = "...1",
           mean_EdZ_0m = "...2", 
           detection_levels = "...3")
  # Extract downwelling irradiance at depth
  edz_tmp <- cops$EdZ.fitted %>% 
    as_tibble() %>%
    bind_cols(depth_tmp, .) %>%
    pivot_longer(2:20, names_to = "wavelength", values_to = "EdZ") %>%
    # Convert wavelength to numeric
    mutate(wavelength = as.numeric(wavelength)) %>%
    # Combine Ed0 and EdZ_0m
    left_join(., ed0_tmp, by = "wavelength") %>%
    left_join(., edz_0m_tmp, by = "wavelength") %>% 
    # Add metadata
    mutate(date_COPS = cops$date.mean,
           station = str_extract(cops$file, "DE[0-9]{3}"),
           cast_COPS = as.numeric(str_remove(
             str_extract(cops$file, "CAST_[0-9]{3}"), "CAST_"))) %>%
    # Create unique combination of cast and station
    unite(COPS_ID, station, cast_COPS, sep = " ", remove = F) %>%
    dplyr::select(- cast_COPS)
  # Combine data
  COPS <- bind_rows(COPS, edz_tmp)
}

# Write csv
write_csv(COPS, file = "data/C-OPS/C-OPS_processed.csv")

# Test plot
COPS %>%
  # Select data at 490 nm
  filter(wavelength == 490) %>%
  # Plot
  ggplot(aes(x = EdZ, y = depth, col = COPS_ID)) +
  geom_point(alpha = 0.2) +
  scale_x_continuous("Log10(EdZ) @490 nm (µW cm-2 nm-1)", trans = "log10") +
  scale_y_reverse() + 
  facet_wrap(~ station, scales = "free")
