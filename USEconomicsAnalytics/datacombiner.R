# Load necessary libraries
library(dplyr)

# Set the directory where the CSV files are located
data_directory <- "AQI Data"

# Get a list of all CSV files in the directory
csv_files <- list.files(path = data_directory, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames <- list()

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Add the data frame to the list
  data_frames <- append(data_frames, list(data))
}

# Combine all data frames into one
combined_data <- bind_rows(data_frames)

# Print the first few rows of the combined data
head(combined_data)

# Export the combined data frame to a CSV file
#write.csv(combined_data, file = "combined_aqi_data.csv", row.names = FALSE)

# Read the HPI_master.csv file
hpi_data <- read.csv("HPI_master.csv")

# Print column names of both datasets
cat("Columns in hpi_data:", names(hpi_data), "\n")
cat("Columns in combined_aqi_data:", names(combined_data), "\n")

# Check unique values of columns used for merging in combined_data
unique_combined_data <- combined_data[, c("Year", "CBSA")]
dup_rows_combined_data <- duplicated(unique_combined_data) | duplicated(unique_combined_data, fromLast = TRUE)

# Check unique values of columns used for merging in hpi_data
unique_hpi_data <- hpi_data[, c("yr", "place_name")]
dup_rows_hpi_data <- duplicated(unique_hpi_data) | duplicated(unique_hpi_data, fromLast = TRUE)

# Print duplicate rows for each dataset
cat("Duplicates in combined_data:", any(dup_rows_combined_data), "\n")
cat("Duplicates in hpi_data:", any(dup_rows_hpi_data), "\n")

# Remove duplicate rows from hpi_data based on yr and place_name
hpi_data_unique <- unique(hpi_data[, c("yr", "place_name")])

# Perform inner join based on year and region
merged_data <- merge(combined_data, hpi_data_unique, by.x = c("Year", "CBSA"), by.y = c("yr", "place_name"), all = FALSE)

# Print the first few rows of the merged data
head(merged_data)

# Export the merged data frame to a CSV file
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)

# Print column names of both datasets
cat("Columns in combined_data:", names(combined_data), "\n")
cat("Columns in hpi_data:", names(hpi_data), "\n")

# Inspect the structure of combined_data
str(combined_data)

# Inspect the structure of hpi_data
str(hpi_data)

# Rename columns in hpi_data to match combined_data
names(hpi_data)[names(hpi_data) == "yr"] <- "Year"
names(hpi_data)[names(hpi_data) == "place_name"] <- "CBSA"

# Perform inner join based on year and region
merged_data <- merge(combined_data, hpi_data, by.x = c("Year", "CBSA"), by.y = c("Year", "CBSA"), all = FALSE)

# Print the first few rows of the merged data
head(merged_data)

# Export the merged data frame to a CSV file
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)

# Read the USIndicatorsEdited.csv file
indicators_data <- read.csv("USIndicatorsEdited.csv")

print(names(indicators_data))

# Check unique values of the "Subject Descriptor" column
unique_subject_descriptor <- unique(indicators_data$`Subject Descriptor`)

# Print unique values
print(unique_subject_descriptor)

# Perform inner join based on year
final_merged_data <- merge(merged_data, indicators_data, by.x = "Year", by.y = "Subject.Descriptor", all = FALSE)

# Print the first few rows of the final merged data
head(final_merged_data)

#Getting the S&P 500 Index Monthly Closing values
getSymbols("^GSPC", src = "yahoo", from = "1990-01-01", to = "2023-12-31")
monthly_closing_price <- to.monthly(GSPC)[, "GSPC.Close"]

sp500_monthly_returns <- data.frame(Date = index(monthly_closing_price), SP500_Monthly_Returns = coredata(monthly_closing_price))
sp500_monthly_returns$Date <- as.Date(sp500_monthly_returns$Date)
sp500_monthly_returns$Date <- floor_date(sp500_monthly_returns$Date, "month")

# Print column names of final_merged_data
print(names(final_merged_data))

# Print column names of sp500_monthly_returns
print(names(sp500_monthly_returns))

# Rename the "Year" column to "Date" in final_merged_data
names(final_merged_data)[names(final_merged_data) == "Year"] <- "Date"

# Convert "Date" column in sp500_monthly_returns to integer
sp500_monthly_returns$Date <- as.integer(as.Date(sp500_monthly_returns$Date))

# Perform the left join
merged_data <- final_merged_data %>%
  left_join(sp500_monthly_returns, by = "Date")

#Merging index values with main frame
merged_data <- merged_data %>%
  left_join(sp500_monthly_returns, by = c("Date"))

# Export the final merged data frame to a CSV file
write.csv(final_merged_data, file = "final_merged_data.csv", row.names = FALSE)
