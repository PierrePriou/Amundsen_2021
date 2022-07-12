# Extract depth and time from CTD casts. This script is tailored to the Amundsen
# 2021 leg5 unprocessed CTD data

# Load packages
library(tidyverse)
library(lubridate)
library(dtplyr)
library(plotly)

# List CTD files in folder
CTD_files <- dir("data/CTD/raw_cast_selection", pattern = "*.cnv", full.names = T)
CTD_id <- str_extract(CTD_files, pattern = "2105[0-9]{3}")

# Create empty matrix
CTD_date_depth <- data.frame()

# Loop that reads and append CTD files
for (i in 1:length(CTD_files))  {
  # Find starting time CTD data
  CTD_time <- read.delim(CTD_files[i], header = F, sep = "", nrows = 1, 
                         skip = 8) %>%
    unite(date_start_CTD, V6, V7, V8, V9, sep = " ") %>%
    mutate(date_start_CTD = mdy_hms(date_start_CTD),
           CTD_id = CTD_id[i]) %>%
    dplyr::select(date_start_CTD, CTD_id)
  
  # Extract pressure, depth, and time from each file
  CTD_data <- read.delim(CTD_files[i], header = F, sep = "", skip = 362) %>% 
    dplyr::select(V1, V2, V16) %>%
    rename("depth" = "V2", "time_elapsed" = "V16") %>%
    # Add CTD_id for matching date_start
    mutate(CTD_id = CTD_id[i]) %>%
    # Join date
    left_join(., CTD_time, by = "CTD_id") %>%
    # Call data table to speed up calculations
    lazy_dt() %>%
    # Calculate time for each row (rounded per second)
    mutate(date_num = round(as.numeric(date_start_CTD) + time_elapsed)) %>%
    # Calculate mean depth for each second
    group_by(date_num, CTD_id) %>%
    summarize_all(mean) %>%
    ungroup() %>%
    # Convert date_num to POSIXct
    mutate(date = as.POSIXct(date_num, origin = "1970-01-01 00:00:00",
                             tz = "UTC")) %>%
    # Select variables of interest
    dplyr::select(CTD_id, date_start_CTD, date, depth) %>%
    as_tibble()
  # Combine data
  CTD_date_depth <- bind_rows(CTD_date_depth, CTD_data) 
}

# Match formatting of other datasets
CTD_date_depth <- CTD_date_depth %>%
  mutate(cast_number = as.numeric(str_remove(CTD_id, pattern = "2105")),
         # Add turning point and classify
         cast = 
           case_when(cast_number == 2 ~ if_else(date <= ymd_hms("2021-10-12 17:24:13", tz = "UTC"),
                                                "downcast", "upcast"),
                     cast_number == 13 ~ if_else(date <= ymd_hms("2021-10-17 16:32:31", tz = "UTC"), 
                                                 "downcast", "upcast"),
                     cast_number == 25 ~ if_else(date <= ymd_hms("2021-10-21 10:30:13", tz = "UTC"),
                                                 "downcast", "upcast"),
                     cast_number == 26 ~ if_else(date <= ymd_hms("2021-10-21 13:20:11", tz = "UTC"),
                                                 "downcast", "upcast"),
                     cast_number == 28 ~ if_else(date <= ymd_hms("2021-10-21 19:22:22", tz = "UTC"),
                                                 "downcast", "upcast"),
                     cast_number == 29 ~ if_else(date <= ymd_hms("2021-10-21 22:41:34", tz = "UTC"),
                                                 "downcast", "upcast"),
                     cast_number == 40 ~ if_else(date <= ymd_hms("2021-10-25 18:34:32", tz = "UTC"),
                                                 "downcast", "upcast"))) %>%
  dplyr::select(-CTD_id)

# Write csv
write_csv(CTD_date_depth, file = "data/CTD/CTD_date_depth.csv")

# Remove unused variables
rm(i, CTD_files, CTD_id, CTD_time, CTD_data)
