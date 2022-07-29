# Script that reads processed C-OPS data and digest it in a more readable format

# Load packages
library(tidyverse)


## Fitted data

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
    rename(lambda_nm = "...1",
           mean_ed0 = "...2")
  # Extract mean downwelling irradiance below surface (Ed0.0p) and detection levels
  edz_0m_tmp <- bind_cols(cops$EdZ.waves, cops$EdZ.0m, cops$EdZ.detection.limit) %>% 
    as_tibble() %>%
    rename(lambda_nm = "...1",
           mean_edz_0m = "...2", 
           detection_level = "...3")
  # Extract PAR (Photosynthetically active radiation 400-700 nm in umol m-2 s-1)
  PAR_tmp <- cops$PARd.z %>%
    as_tibble() %>%
    rename(PAR_umol_m2 = value) %>%
    # Extract depth
    mutate(depth = as.numeric(depth_tmp$depth)) 
  # Extract Kz (attenuation coefficient at depth z)
  kz_edz_tmp <- cops$KZ.EdZ.fitted %>%
    as_tibble() %>%
    # Extract depth
    mutate(depth = as.numeric(rownames(cops$KZ.EdZ.fitted))) %>%
    # Long format
    pivot_longer(1:19, names_to = "lambda_nm", values_to = "kz") %>%
    # Convert wavelength to numeric
    mutate(lambda_nm = as.numeric(lambda_nm)) 
  # Extract downwelling irradiance at depth
  edz_tmp <- cops$EdZ.fitted %>% 
    as_tibble() %>%
    # Extract depth
    mutate(depth = as.numeric(rownames(cops$EdZ.fitted))) %>%
    # Long format
    pivot_longer(1:19, names_to = "lambda_nm", values_to = "ed_uW_cm2") %>%
    # Convert wavelength to numeric
    mutate(lambda_nm = as.numeric(lambda_nm)) %>%
    # Combine PAR, Kz, Ed0 and EdZ_0m
    left_join(., PAR_tmp, by = c("depth")) %>%
    left_join(., kz_edz_tmp, by = c("depth", "lambda_nm")) %>%
    left_join(., ed0_tmp, by = "lambda_nm") %>%
    left_join(., edz_0m_tmp, by = "lambda_nm") %>% 
    # Add metadata
    mutate(date_COPS = cops$date.mean,
           station = str_extract(cops$file, "DE[0-9]{3}"),
           cast_COPS = as.numeric(str_remove(
             str_extract(cops$file, "CAST_[0-9]{3}"), "CAST_"))) %>%
    # Create unique combination of cast and station
    unite(COPS_ID, station, cast_COPS, sep = "_", remove = F) %>%
    dplyr::select(- cast_COPS)
  # Combine data
  COPS <- bind_rows(COPS, edz_tmp)
}

# Reorder variables
COPS <- COPS %>%
  dplyr::select(station, COPS_ID, date_COPS, depth, lambda_nm, detection_levels, 
                PAR_umol_m2, mean_ed0, mean_edz_0m, kz, ed_uW_cm2)

# Write csv
write_csv(COPS, file = "data/C-OPS/C-OPS_fitted_processed.csv")

# Remove temporary variables
rm(file_list, i, cops, depth_tmp, ed0_tmp, edz_0m_tmp, edz_tmp, kz_edz_tmp, PAR_tmp)


## RAW data

# List files
file_list <- list.files("data/C-OPS/processed/", pattern = ".RData", full.names = T) 

# Empty dataframe
COPS_raw <- data.frame()

# Loop through each file per year
for (i in file_list) { 
  load(i)
  # Extract depth
  depth_tmp <- bind_cols(cops$Depth, cops$Depth.good) %>%
    as_tibble() %>%
    rename(depth = "...1",
           depth_good = "...2")
  # Extract detection levels
  detection_tmp <- bind_cols(cops$EdZ.waves, cops$EdZ.detection.limit) %>% 
    as_tibble() %>%
    rename(lambda_nm = "...1",
           detection_level = "...2")
  # Extract downwelling irradiance at depth
  edz_tmp <- cops$EdZ %>% 
    as_tibble() %>%
    # Add depth
    bind_cols(depth_tmp, .) %>%
    # Long format
    pivot_longer(3:21, names_to = "lambda_nm", values_to = "ed_uW_cm2") %>%
    # Convert wavelength to numeric
    mutate(lambda_nm = as.numeric(lambda_nm)) %>%
    # Add detection levels
    left_join(., detection_tmp, by = "lambda_nm")%>%
    # Add metadata
    mutate(date_COPS = cops$date.mean,
           station = str_extract(cops$file, "DE[0-9]{3}"),
           cast_COPS = as.numeric(str_remove(str_extract(cops$file, "CAST_[0-9]{3}"), "CAST_"))) %>%
    # Create unique combination of cast and station
    unite(COPS_ID, station, cast_COPS, sep = "_", remove = F) %>%
    dplyr::select(-cast_COPS) %>%
    # Remove bad measurements
    filter(depth_good == T & ed_uW_cm2 > detection_level) %>%
    select(-depth_good, -detection_level)
  # Combine data
  COPS_raw <- bind_rows(COPS_raw, edz_tmp)
}

# Reorder variables
COPS_raw <- COPS_raw %>%
  dplyr::select(station, COPS_ID, date_COPS, depth, lambda_nm, ed_uW_cm2)

# Write csv
write_csv(COPS_raw, file = "data/C-OPS/C-OPS_RAW_processed.csv")

# Test plot
ggplot() +
  geom_point(data = subset(COPS_raw, lambda_nm == 490 & station == "DE320"),
             aes(x = ed_uW_cm2, y = depth), col = "steelblue3", alpha = 0.2) +
  geom_point(data = subset(COPS, lambda_nm == 490 & station == "DE320"),
             aes(x = ed_uW_cm2, y = depth), col = "red", size = 0.75, alpha = 1) +
  scale_x_continuous("Log10(Edz) @490 nm (µW cm-2 nm-1)") +
  scale_y_reverse() + 
  facet_wrap(~ COPS_ID, scales = "fixed")
