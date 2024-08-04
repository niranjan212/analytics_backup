library(dplyr)

# Read files into a dataframe
final_data <- read.csv("final_data1.csv")
final_data_clean <- read.csv("final_data_clean1.csv")


# Define breaks for grouping
breaks <- c(1991, 1996, 2001, 2006, 2011, 2016, 2021, 2026)

# Create 'group' column based on breaks
final_data_clean <- final_data_clean %>%
  mutate(group = cut(Year, breaks = breaks, labels = FALSE, include.lowest = TRUE))

head(final_data_clean)

# Extract unique CBSA, lat_numeric, and lon_numeric pairings
unique_coords <- final_data_clean %>%
  distinct(CBSA, lat_numeric, lon_numeric)

final_data_averaged <- final_data_clean %>%
  select(-c(Year, lat_numeric, lon_numeric)) %>%
  group_by(group, CBSA) %>%
  summarise(across(
    .cols = everything(),
    .fns = ~ ifelse(any(is.na(.)), NA, mean(., na.rm = TRUE))
  ))


# Join with unique coordinates
final_data_averaged <- left_join(final_data_averaged, unique_coords, by = "CBSA")

# Create a lookup table for group and year
lookup_table <- data.frame(
  group = 1:7,
  Year = c(1996, 2001, 2006, 2011, 2016, 2021, 2023)
)

# Merge lookup table with final_data_averaged dataframe
final_data_averaged <- merge(final_data_averaged, lookup_table, by = "group")
