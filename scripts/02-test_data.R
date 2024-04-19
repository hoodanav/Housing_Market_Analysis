#### Preamble ####
# Purpose: Test data for checks 
# Author: Navya Hooda
# Date: April 9, 2023
# Contact: navya.hooda@mail.utoronto.ca
# License: MIT




#### Workspace setup ####
library(tidyverse)
library(testthat)


#### Test data ####
merged_data <- read.csv("data/analysis_data/merged_data.csv")



#### Test data ####

# Test the structure and type of the economic variables
test_that("Economic variables columns are of the correct type", {
  expect_type(merged_data$Interest_Rate, "double")
  expect_type(merged_data$Inflation_Rate, "numeric")
  expect_type(merged_data$Unemployment_Rate, "numeric")
})

# Test the structure and type of the housing starts data
test_that("Housing starts data columns are of the correct type", {
  expect_type(merged_data$Year, "integer")
  expect_type(merged_data$Housing_Starts, "numeric")
})

# Test the structure and type of the immigration data
test_that("Immigration data columns are of the correct type", {
  expect_type(merged_data$Year, "integer")
  expect_type(merged_data$Immigrants, "numeric")
})

# Test that the years are within the expected range
test_that("Year values are within the expected range", {
  expect_true(all(merged_data$Year >= 2000 & merged_data$Year <= 2024))
})

# Test for missing values
test_that("There are no missing values in key columns", {
  expect_true(all(!is.na(merged_data$Interest_Rate)))
  expect_true(all(!is.na(merged_data$Housing_Starts)))
  expect_true(all(!is.na(merged_data$Immigrants)))
})
