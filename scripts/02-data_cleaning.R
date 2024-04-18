#### Preamble ####
# Purpose: Clean data for meaningful analysis
# Author: Navya Hooda
# Date: April 9, 2023
# Contact: navya.hooda@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(dplyr)
install.packages("readxl")  # Install the readxl package if not already installed
library(readxl)  # Load the readxl package

#### Clean data ####
inflation_data <- read_csv("data/raw_data/inflation-rate-cpi.csv", skip = 16)
unemployment_data <- read_csv("data/raw_data/canada-unemployment-rate.csv", skip = 16)
interest_data <- read_excel("data/raw_data/interest_rate_2000.xlsx",skip=, col_types = "text")
hpi_index <- read_excel("data/raw_data/HPI_20-23.xlsx",skip=, col_types = "text")
immigration <- read_excel("data/raw_data/immigration_data.xlsx",skip=, col_types = "text")
housing_starts <- read_excel("data/raw_data/housing_starts.xlsx",skip=, col_types = "text")
# inflation
# Clean data
inflation_data_cleaned <- inflation_data %>%
  # Convert 'date' to date format and extract the year
  mutate(year = as.integer(substring(date, 1, 4))) %>%
  # Filter for years 2013-2023
  filter(year >= 2000 & year <= 2023) %>%
  # Select relevant columns
  select(year, `Inflation Rate (%)`, `Annual Change`) %>%
  # Replace NA values in 'Annual Change' with 0
  mutate(`Annual Change` = ifelse(is.na(`Annual Change`), 0, `Annual Change`))

# Print the cleaned data
print(inflation_data_cleaned)

# unemployment 

unemployment_cleaned <- unemployment_data %>%
  # Convert 'date' to date format and extract the year
  mutate(year = as.integer(substring(date, 1, 4))) %>%
  # Filter for years 2013-2023
  filter(year >= 2000 & year <= 2023) %>%
  # Select relevant columns
  select(year, `Unemployment Rate (%)`, `Annual Change`) %>%
  # Replace NA values in 'Annual Change' with 0
  mutate(`Annual Change` = ifelse(is.na(`Annual Change`), 0, `Annual Change`))

# Print the cleaned data
print(unemployment_cleaned)

# interest rates 

# Find the row indexes for 'Geography' and 'Canada'
geography_index <- which(interest_data[, 1] == "Geography")
print(geography_index)
canada_index <- which(interest_data[, 1] == "Canada")
print(canada_index)
# Extract the 'Geography' and 'Canada' rows
geography_row <- interest_data[geography_index, ]
canada_row <- interest_data[canada_index, ]

# Remove the first element (column name) from each row
geography_row <- geography_row[, -1]
canada_row <- canada_row[, -1]

# Transpose the rows to convert them into columns
geography_data <- t(geography_row)
canada_data <- t(canada_row)

# Create a new dataframe with 'Year' and 'Interest_Rate' columns
new_dataframe <- data.frame(Year = geography_data, Interest_Rate = canada_data)

# Print the new dataframe
print(new_dataframe)


# Convert Year to a Date object
new_dataframe$Year <- as.Date(new_dataframe$Year)

# Extract the year from the date
new_dataframe$Year <- format(new_dataframe$Year, "%Y")

# Convert Interest_Rate to numeric
new_dataframe$Interest_Rate <- as.numeric(new_dataframe$Interest_Rate)

# Aggregate to calculate the average interest rate by year
interest_averages <- aggregate(Interest_Rate ~ Year, data = new_dataframe, mean)

# Print the averages
print(interest_averages)



# house-price-index

# Assuming 'hpi_index' is your original dataframe

# Extract the rows by row number
new_housing_index <- 9
house_only_index <- 12

new_housing_data <- hpi_index[new_housing_index, -1]  # Exclude the first column
house_only_data <- hpi_index[house_only_index, -1]  # Exclude the first column

# Transpose the rows to convert them into columns
new_housing_data_transposed <- t(new_housing_data)
house_only_data_transposed <- t(house_only_data)

# Create a new dataframe with 'New housing price index' and 'House only' columns
new_dataframe <- data.frame(Year = new_housing_data_transposed, HPI = house_only_data_transposed)

# Print the new dataframe
print(head(new_dataframe))

new_dataframe$Year <- as.Date(new_dataframe$Year)
# Extract the year from the date
new_dataframe$Year <- format(new_dataframe$Year, "%Y")

new_dataframe$HPI <- as.numeric(new_dataframe$HPI)


# Group by year and calculate the average for 'House Only'
house_only_averages <- aggregate(HPI ~ Year, data = new_dataframe, mean)

print(house_only_averages)
# Rename the 'House Only' column to 'Average_House_Only'
colnames(house_only_averages)[2] <- "Average_House_Only"

# Print the averages
print(house_only_averages)



# immigration
print(immigration)


# Update the Year column
immigration$Year <- sub(".* - ", "", immigration$Year)

# Print the updated dataframe
print(immigration)

# Create a new row for 2000 with NA immigration value
new_row <- data.frame(Year = "2000", `Number of immigrants in Canada from 2000 to 2023` = 0)

# Ensure column names match
colnames(new_row) <- colnames(immigration)

# Append the new row to the immigration dataframe
immigration <- rbind(immigration, new_row)

# Calculate percent change

# Convert the immigration column to numeric
immigration$`Number of immigrants in Canada from 2000 to 2023` <- as.numeric(immigration$`Number of immigrants in Canada from 2000 to 2023`)

# Calculate percent change
# Print the modified dataframe
print(tail(immigration))




# Print the updated dataframe
print(immigration)

# Print the modified data
print(tail(immigration))


# Print the modified data
print(immigration)



# cleaning housing starts 
print(housing_starts)


# Find the row indices where "housing starts" and "housing estimates" are located in the first column
housing_starts_row <- which(housing_starts[, 1] == "Housing starts")
housing_estimates_row <- which(housing_starts[, 1] == "Housing estimates")

# Extract the rows based on the found indices
housing_starts_data <- housing_starts[housing_starts_row, ]
housing_estimates_data <- housing_starts[housing_estimates_row, ]

# Combine the two dataframes
combined_data <- rbind(housing_starts_data, housing_estimates_data)
print(combined_data)
# Optionally, you can remove the rows containing 'NA'
combined_data <- na.omit(combined_data)

# Transpose row 13 into two columns: "year" and "starts"
transposed_data <- t(combined_data)
transposed_data_df <- as.data.frame(transposed_data)
transposed_data_df <- transposed_data_df[-c(1:2), ]
colnames(transposed_data_df) <- c("Starts", "Year")
print(transposed_data_df)
# Assuming you have already performed the previous operations and have `transposed_data_df` dataframe

# Convert the "year"  and 'starts' column to numeric
# Extract the year from the "year" column (last four characters)
transposed_data_df$Year <- substr(transposed_data_df$Year, nchar(transposed_data_df$Year) - 3, nchar(transposed_data_df$Year))

print(str(transposed_data_df$Year))
print(head(transposed_data_df$Year))


transposed_data_df$Year <- as.numeric(transposed_data_df$Year)
transposed_data_df$Starts <- as.numeric(transposed_data_df$Starts)

# Group by year and calculate the total for "starts"
total_starts <- aggregate(Starts ~ Year, data = transposed_data_df, FUN = sum, na.rm = TRUE)
# Print the first few rows of the averaged dataframe
print(head(total_starts))


# Group by year and calculate the average for "starts"
print(head(total_starts))
# Write the dataframe to a CSV file
write.csv(total_starts, "data/analysis_data/total_starts_by_year.csv", row.names = FALSE)



# clean merge set

names(inflation_data_cleaned)[names(inflation_data_cleaned) == "year"] <- "Year"
names(unemployment_cleaned)[names(unemployment_cleaned) == "year"] <- "Year"

# Merge the cleaned datasets by the "year" property
# Convert 'Year' to character in all datasets
inflation_data_cleaned$Year <- as.character(inflation_data_cleaned$Year)
unemployment_cleaned$Year <- as.character(unemployment_cleaned$Year)
interest_averages$Year <- as.character(interest_averages$Year)
house_only_averages$Year <- as.character(house_only_averages$Year)
immigration$Year <- as.character(immigration$Year)
total_starts$Year <- as.character(total_starts$Year)


# Check structure and content of the "Year" column in merged_data
str(merged_data$year)
head(merged_data$year)

# Check structure and content of the "Year" column in interest_averages
str(interest_averages$Year)
head(interest_averages$Year)


# Merge the datasets
merged_data <- Reduce(
  function(x, y) merge(x, y, by = "Year", all = TRUE),
  list(inflation_data_cleaned, unemployment_cleaned, interest_averages, house_only_averages, immigration, total_starts)
)


# Set scipen option to disable scientific notation
options(scipen = 999)

# Merge datasets based on the 'Year' column
merged_data <- Reduce(
  function(x, y) merge(x, y, by = "Year", all = TRUE),
  list(
    house_only_averages,
    interest_averages,
    unemployment_cleaned,
    inflation_data_cleaned, 
    immigration, 
    total_starts
    # Add other datasets as needed
  )
)

# Print the merged data
print(merged_data)


# tests for merged data 

# Check data types of the 'Year' column in each dataset
data_types <- sapply(list(house_only_averages, interest_averages, 
                          unemployment_cleaned, inflation_data_cleaned),
                     function(df) class(df$Year))
print(data_types)

# Check column names of each dataset
colnames_list <- lapply(list(house_only_averages, interest_averages, 
                             unemployment_cleaned, inflation_data_cleaned),
                        colnames)
print(colnames_list)



# Check for duplicates in the 'Year' column of each dataset
duplicated_rows <- sapply(list(house_only_averages, interest_averages, 
                               unemployment_cleaned, inflation_data_cleaned),
                          function(df) any(duplicated(df$Year)))
print(duplicated_rows)



# Save the merged data to a CSV file
write.csv(merged_data, "data/analysis_data/merged_data.csv", row.names = FALSE)


# Print the merged data
print(merged_data)


