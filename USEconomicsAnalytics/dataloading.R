# Load necessary libraries
library(quantmod)
library(dplyr)

# Load the merged_data.csv file
merged_data <- read.csv("merged_data.csv")

# Fetch quarterly closing prices of the GSPC ticker from 1990 to 2023
getSymbols("^GSPC", src = "yahoo", from = "1990-01-01", to = "2023-12-31")
quarterly_closing_price <- to.quarterly(GSPC)[, "GSPC.Close"]

# Create a dataframe for quarterly closing prices
sp500_quarterly_returns <- data.frame(Date = index(quarterly_closing_price), SP500_Quarterly_Returns = coredata(quarterly_closing_price))
sp500_quarterly_returns$Date <- as.Date(sp500_quarterly_returns$Date)

# Create the "Quarter" column in merged_data
merged_data$Quarter <- paste0(merged_data$Year, "Q", merged_data$period)

# Create the "Quarter" column in sp500_quarterly_returns
sp500_quarterly_returns$Quarter <- paste0(year(sp500_quarterly_returns$Date), "Q", quarter(sp500_quarterly_returns$Date))

# Merge the datasets based on the "Quarter" column
final_combined_data <- left_join(merged_data, sp500_quarterly_returns, by = "Quarter")

# Write the final combined dataset to a CSV file
write.csv(final_combined_data, file = "final_combined_data.csv", row.names = FALSE)

# Load the necessary libraries
library(dplyr)

# Read the datasets
final_combined_data <- read.csv("final_combined_data.csv")
USIndicatorsEdited <- read.csv("USIndicatorsEdited.csv")

# Merge the datasets using a left join on the "Year" column
combined_data <- merge(final_combined_data, USIndicatorsEdited, by.x = "Year", by.y = "Subject.Descriptor", all.x = TRUE)

# View the combined dataset
head(combined_data)

# Check the column names and unique values in the "Year" column of both datasets
print("Column names in final_combined_data:")
print(names(final_combined_data))
print("Unique values in final_combined_data$Year:")
print(unique(final_combined_data$Year))

print("Column names in USIndicatorsEdited:")
print(names(USIndicatorsEdited))
print("Unique values in USIndicatorsEdited$Subject.Descriptor:")
print(unique(USIndicatorsEdited$Subject.Descriptor))

# Rename the "Subject.Descriptor" column in USIndicatorsEdited to "Year"
names(USIndicatorsEdited)[1] <- "Year"

# Perform the merge using the "Year" column as the key
merged_data <- merge(final_combined_data, USIndicatorsEdited, by = "Year", all.x = TRUE)

# Write the final combined dataset to a CSV file
write.csv(merged_data, file = "final_data.csv", row.names = FALSE)
