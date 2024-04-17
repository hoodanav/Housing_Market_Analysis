#### Preamble ####
# Purpose: Simulate Data 
# Author: Navya Hooda
# Date: April 9, 2023
# Contact: navya.hooda@mail.utoronto.ca
# License: MIT

# Set seed for reproducibility
set.seed(123)

# Number of observations
num_observations <- 100

# Simulate data for new housing added in the last few years
new_housing_added <- rnorm(num_observations, mean = 500, sd = 100)

# Simulate data for proposed housing units in the next 2-3 years
proposed_housing_units <- rnorm(num_observations, mean = 300, sd = 50)

# Simulate data for housing rates
housing_rates <- 50000 + 10*new_housing_added - 5*proposed_housing_units + rnorm(num_observations, mean = 0, sd = 10000)

# Create a data frame to store the simulated data
simulated_data <- data.frame(
  New_Housing_Added = new_housing_added,
  Proposed_Housing_Units = proposed_housing_units,
  Housing_Rates = housing_rates
)

# Display the first few rows of the simulated data
head(simulated_data)

# Create a scatter plot of New_Housing_Added against Housing_Rates
plot(simulated_data$New_Housing_Added, simulated_data$Housing_Rates,
     xlab = "New Housing Added", ylab = "Housing Rates",
     main = "Scatter plot of New Housing Added vs Housing Rates",
     col = "blue", pch = 16
)

# Create a scatter plot of Proposed_Housing_Units against Housing_Rates
plot(simulated_data$Proposed_Housing_Units, simulated_data$Housing_Rates,
     xlab = "Proposed Housing Units", ylab = "Housing Rates",
     main = "Scatter plot of Proposed Housing Units vs Housing Rates",
     col = "blue", pch = 16
)



