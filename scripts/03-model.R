#### Preamble ####
# Purpose: Model data for meaningful analysis
# Author: Navya Hooda
# Date: April 9, 2023
# Contact: navya.hooda@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(modelsummary)


#### Read data ####
merged_data <- read_csv("data/analysis_data/merged_data.csv")
housing_starts <- read_csv("data/analysis_data/total_starts_by_year.csv")
#
print(merged_data)
# Assuming merged_data is your merged dataset
# Assuming housing_starts is your dataset containing the number of housing starts


# Subset the relevant columns from merged_data
model_data <- merged_data[, c("Average_House_Only", "Inflation Rate (%)", "Interest_Rate", "Unemployment Rate (%)", "Number of immigrants in Canada from 2000 to 2023", "Starts")]

# Rename columns for convenience
colnames(model_data) <- c("House_Price_Index", "Inflation_Rate", "Interest_Rate", "Unemployment_Rate", "Immigration_numbers", "Housing_Starts")
print(colnames(model_data))
# Assuming 'merged_data' is your merged dataset containing the housing starts data

# Fit a linear regression model with housing starts as an additional predictor
pricing_model <- stan_glm(House_Price_Index ~ Inflation_Rate + Interest_Rate + Unemployment_Rate + Immigration_numbers + Housing_Starts, data = model_data)

# Summary of the model
summary(pricing_model)
modelsummary(pricing_model)



#### Save model ####
saveRDS(
  pricing_model,
  file = "models/pricing_model.rds"
)

