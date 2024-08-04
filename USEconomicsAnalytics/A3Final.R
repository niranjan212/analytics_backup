library(corrplot)
library(tidyr)
library(dplyr)
library(MASS)
library(car)
library(ggplot2)
library(plotly)
library(leaflet)
library(knitr)

# Read files into a dataframe
final_data <- read.csv("C:/Users/x/OneDrive/Documents/GitHub/analytics-assignment3/final_data1.csv")
final_data_clean <- read.csv("C:/Users/x/OneDrive/Documents/GitHub/analytics-assignment3/final_data_clean_VF.csv")
final_data_averaged <- read.csv("C:/Users/x/OneDrive/Documents/GitHub/analytics-assignment3/final_data_averaged.csv")

# Calculate correlation matrix
cor_matrix <- cor(final_data_clean[, sapply(final_data_clean, is.numeric)])

# Plot correlation matrix as a heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.35)

# Find variables with strong correlations
strong_correlations <- colnames(cor_matrix)[rowSums(abs(cor_matrix) > 0.7) > 1]

# Print the variables with strong correlations  
print(strong_correlations)

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

# Scatter plot of index_nsa vs. each numeric predictor variable
scatter_plots <- lapply(names(final_data_clean)[sapply(final_data_clean, is.numeric)], function(var) {
  ggplot(final_data_clean, aes_string(x = var, y = "index_nsa")) +
    geom_point() +
    labs(x = var, y = "index_nsa") +
    ggtitle(paste("Scatter plot of index_nsa vs.", var))
})

# Line plot of index_nsa vs. each numeric predictor variable
line_plots <- lapply(names(final_data_clean)[sapply(final_data_clean, is.numeric)], function(var) {
  ggplot(final_data_clean, aes_string(x = var, y = "index_nsa")) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = var, y = "index_nsa") +
    ggtitle(paste("Line plot of index_nsa vs.", var))
})

# Print scatter plots
print(scatter_plots)

# Print line plots
print(line_plots)

# Remove CBSA, Year, and group columns
data_subset <- final_data_clean %>%
  select(-CBSA, -Year, -group)

# Perform stepwise regression
stepwise_model <- stepAIC(lm(index_nsa ~ ., data = data_subset), direction = "both")

# Print summary of the selected model
summary(stepwise_model)

# Step 2: Check for multicollinearity
vif_values <- vif(stepwise_model)
print(vif_values)

# Set a threshold for VIF values
threshold <- 10

# Identify variables with VIF above the threshold
high_collinearity_vars <- names(vif_values)[vif_values > threshold]

print(high_collinearity_vars)

# Remove variables with high collinearity from the model
final_model <- update(stepwise_model, . ~ . - 
                        Gross.domestic.product.per.capita..constant.prices - GSPC.Close - 
                        Gross.domestic.product.based.on.purchasing.power.parity..PPP..share.of.world.total )

# Check for multicollinearity again
vif_values <- vif(final_model)
print(vif_values)

# Set a threshold for VIF values
threshold <- 10

# Identify variables with VIF above the threshold
high_collinearity_vars <- names(vif_values)[vif_values > threshold]

print(high_collinearity_vars)

# Summary of the updated final model
summary(final_model)

#####################################

#index_nsa

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
  print(kable(head(select(data, 
                     GSPC.Close,
                     Gross.domestic.product..constant.prices,
                     Gross.domestic.product.per.capita..constant.prices,
                     Gross.domestic.product.per.capita..current.prices,
                     Gross.domestic.product.based.on.purchasing.power.parity..PPP..share.of.world.total,
                     `Inflation..average.consumer.prices`,
                     Volume.of.imports.of.goods.and.services,
                     Volume.of.exports.of.goods.and.services,
                     Unemployment.rate))))
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
  print(kable(head(select(data, 
                     GSPC.Close,
                     Gross.domestic.product..constant.prices,
                     Gross.domestic.product.per.capita..constant.prices,
                     Gross.domestic.product.per.capita..current.prices,
                     Gross.domestic.product.based.on.purchasing.power.parity..PPP..share.of.world.total,
                     `Inflation..average.consumer.prices`,
                     Volume.of.imports.of.goods.and.services,
                     Volume.of.exports.of.goods.and.services,
                     Unemployment.rate))))
}

maps2

