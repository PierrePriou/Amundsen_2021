# Script that reads processed TriOS data and combine them

library(tidyverse)
library(readxl)
library(lubridate)

# List files
file_list <- list.files("C:/Users/cfer/Raw data PhD/Amundsen 2021/TriOS",
                        pattern = "*umol.xlsx", full.names = T) 

# Empty dataframe
trios_raw <- data.frame()

# Loop that reads file and combine them
for (i in 1:length(file_list)){
  tmp <- read_xlsx(file_list[i]) %>%
    # Rename variables
    rename(date_trios = Date_Time,
           lat = Lat,
           lon = Long, 
           PAR_umol_m2 = PAR) %>%
    # Long format
    pivot_longer(5:635, names_to = "lambda_nm", values_to = "e0_umol_m2") %>%
    # Select wavelength common to C-OPS and TriOS to reduce size of the file
    filter(lambda_nm %in% c(380, 395, 412, 443, 465, 490, 510, 532, 555, 560, 589,
                            625, 665, 683, 694, 710, 765, 780, 875)) %>%
    # Fix variable formats
    mutate(date_trios = dmy_hms(date_trios, tz = "UTC"),
           lambda_nm = as.numeric(lambda_nm),
           trios_filename = factor(str_remove(file_list[i], pattern = "C:/Users/cfer/Raw data PhD/Amundsen 2021/TriOS/"))) %>%
    mutate(trios_filename = str_remove(trios_filename, pattern = "2021_Trios_Surface_Lightdata_umol.xlsx"))
  # Combine data
  trios_raw <- bind_rows(trios_raw, tmp)
  # Loop status
  print(paste0("file ", i, " / ", length(file_list), " done"))
}

# Check data distribution
cowplot::plot_grid(
  # Boxplot of e0 at all wavelengths
  trios_raw %>%
    ggplot() +
    geom_boxplot(aes(x = trios_filename, y = e0_umol_m2, color = factor(lambda_nm)), na.rm = T) +
    theme(legend.position = "none"),
  # Boxplot of PAR
  trios_raw %>%
    ggplot() +
    geom_boxplot(aes(x = trios_filename, y = PAR_umol_m2), na.rm = T),
  ncol = 1)
# Outliers on October 15

# Tidy data
trios <- trios_raw %>%
  # Replace potential outliers by NaN
  mutate(e0_umol_m2 = if_else(between(e0_umol_m2, -1 * 10^-5, 1), e0_umol_m2, NaN),
         PAR_umol_m2 = if_else(between(PAR_umol_m2, -1 * 10^-5, 500), PAR_umol_m2, NaN))

# Plot to check whether outliers were removed or not
cowplot::plot_grid(
  # Boxplot of e0 at all wavelengths
  trios %>%
    ggplot() +
    geom_boxplot(aes(x = trios_filename, y = e0_umol_m2, color = factor(lambda_nm)), na.rm = T) +
    theme(legend.position = "none"),
  # Boxplot of PAR
  trios %>%
    ggplot() +
    geom_boxplot(aes(x = trios_filename, y = PAR_umol_m2), na.rm = T),
  ncol = 1)

# Test plot
trios %>%
  filter(lambda_nm == 490) %>%
  ggplot() + 
  geom_point(aes(x = date_trios, y = e0_umol_m2), na.rm = T)

# Write csv
write_csv(trios, file = "data/TriOS/TriOS_processed.csv")
