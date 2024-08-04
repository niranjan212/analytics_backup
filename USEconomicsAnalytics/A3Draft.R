library(corrplot)

# Read files into a dataframe
final_data <- read.csv("final_data1.csv")
final_data_clean <- read.csv("final_data_clean_VF.csv")
final_data_averaged <- read.csv("final_data_averaged.csv")

# Display the structure of the dataframes
str(final_data)
str(final_data_clean)

# Write final_data_averaged to a CSV file
write.csv(final_data_averaged, "final_data_averaged.csv", row.names = FALSE)

# Write final_data_clean to a CSV file
write.csv(final_data_clean, "final_data_clean_VF.csv", row.names = FALSE)



##################################

# Calculate correlation matrix
cor_matrix <- cor(final_data_clean[, sapply(final_data_clean, is.numeric)])

# Plot correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Find variables with strong correlations
strong_correlations <- colnames(cor_matrix)[rowSums(abs(cor_matrix) > 0.7) > 1]

# Print the variables with strong correlations  
print(strong_correlations)

##################################

# Load necessary libraries
library(MASS)  # For stepAIC function
library(car)   # For vif function 

# Step 1: Perform stepwise regression for variable selection
stepwise_model <- stepAIC(lm(index_nsa ~ ., data = final_data_clean), direction = "both")

# Print summary of the selected model
summary(stepwise_model)

# Step 2: Check for multicollinearity
vif_values <- vif(stepwise_model)
print(vif_values)

# Set a threshold for VIF values
threshold <- 10

# Identify variables with VIF above the threshold
high_collinearity_vars <- names(vif_values)[vif_values > threshold]

# Remove variables with high collinearity from the model
final_model <- update(stepwise_model, . ~ . - high_collinearity_vars)

summary(final_model)

#################################

#visualizations

# Aggregate the data by year
aggregated_data <- final_data_clean %>%
  group_by(Year) %>%
  summarize(across(where(is.numeric), ~ mean(., na.rm = TRUE)))

# Convert data to long format
aggregated_data_long <- aggregated_data %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value")

str(aggregated_data_long)
aggregated_data_long <- aggregated_data_long %>%
  mutate(Year = as.numeric(as.character(Year)))

library(plotly)

# Loop over each numeric variable and generate a separate interactive plot
for (col in unique(aggregated_data_long$Variable)) {
  plot_data <- aggregated_data_long %>%
    filter(Variable == col)  # Select data for the current numeric variable
  
  # Create the plot
  plot <- plot_ly(plot_data, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines') %>%
    layout(title = paste("Trend over Time for", col), xaxis = list(title = "Year"), yaxis = list(title = col))
  
  # Print the plot
  print(plot)
}

###########################
#index_nsa

library(leaflet)
library(dplyr)
library(knitr)

# Define a function to create Leaflet map with custom popup and heatmap
create_leaflet_map <- function(data) {
  pal <- colorQuantile(c("green", "red"), domain = data$index_nsa, n = 5)  # Define color palette
  
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lon_numeric,
      lat = ~lat_numeric,
      radius = 7,
      color = ~pal(index_nsa),  # Use color palette based on index_nsa values
      fillOpacity = 0.7,
      popup = paste(
        "<b>CBSA:</b> ", data$CBSA, "<br>",
        "<b>Year:</b> ", data$Year, "<br>",
        "<b>Good Days:</b> ", data$Good.Days, "<br>",
        "<b>Max AQI:</b> ", data$Max.AQI, "<br>",
        "<b>Index NSA:</b> ", data$index_nsa
      ),
      label = ~substr(data$CBSA, 1, 20), # Shorten CBSA name for label
      labelOptions = labelOptions(noHide = FALSE)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~index_nsa,
      title = "Index NSA"
    )
}


# Call the function for each group
maps <- list()
for (i in unique(final_data_averaged$group)) {
  data <- filter(final_data_averaged, group == i)
  maps[[i]] <- create_leaflet_map(data)
  
  maps[i]
  # Print the table with specified variable values for the current group
  cat("\n\nData Summary for Group:", i, "\n\n")
  print(kable(select(data, 
                     GSPC.Close,
                     Gross.domestic.product..constant.prices,
                     Gross.domestic.product.per.capita..constant.prices,
                     Gross.domestic.product.per.capita..current.prices,
                     Gross.domestic.product.based.on.purchasing.power.parity..PPP..share.of.world.total,
                     `Inflation..average.consumer.prices`,
                     Volume.of.imports.of.goods.and.services,
                     Volume.of.exports.of.goods.and.services,
                     Unemployment.rate)))
}

maps

#aqi

create_leaflet_map2 <- function(data) {
  pal <- colorQuantile(c("green", "red"), domain = data$Max.AQI, n = 5)  # Define color palette
  
  leaflet(data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lon_numeric,
      lat = ~lat_numeric,
      radius = 7,
      color = ~pal(index_nsa),  # Use color palette based on index_nsa values
      fillOpacity = 0.7,
      popup = paste(
        "<b>CBSA:</b> ", data$CBSA, "<br>",
        "<b>Year:</b> ", data$Year, "<br>",
        "<b>Good Days:</b> ", data$Good.Days, "<br>",
        "<b>Max AQI:</b> ", data$Max.AQI, "<br>",
        "<b>Index NSA:</b> ", data$index_nsa
      ),
      label = ~substr(data$CBSA, 1, 20), # Shorten CBSA name for label
      labelOptions = labelOptions(noHide = FALSE)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~Max.AQI,
      title = "Average Max AQI"
    )
}

# Call the function for each group
maps2 <- list()
for (i in unique(final_data_averaged$group)) {
  data <- filter(final_data_averaged, group == i)
  maps2[[i]] <- create_leaflet_map2(data)
  
  # Print the table with specified variable values for the current group
  cat("\n\nData Summary for Group:", i, "\n\n")
  print(kable(select(data, 
                     GSPC.Close,
                     Gross.domestic.product..constant.prices,
                     Gross.domestic.product.per.capita..constant.prices,
                     Gross.domestic.product.per.capita..current.prices,
                     Gross.domestic.product.based.on.purchasing.power.parity..PPP..share.of.world.total,
                     `Inflation..average.consumer.prices`,
                     Volume.of.imports.of.goods.and.services,
                     Volume.of.exports.of.goods.and.services,
                     Unemployment.rate)))
}

maps2
