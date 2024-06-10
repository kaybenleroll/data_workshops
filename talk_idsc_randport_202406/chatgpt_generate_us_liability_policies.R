# Load necessary libraries
library(tidyverse)

# Constants
num_policies <- 1000

sectors <- c("Manufacturing", "Retail", "Technology", "Construction",
             "Food & Beverage", "Healthcare", "Transportation",
             "Education", "Renewable Energy", "Travel & Leisure")
risk_levels <- c("Low", "Medium", "High")

# Random data generation functions
generate_company_name <- function(sector, index) {
  paste(sector, "Co", index + 1)
}

generate_employee_headcount <- function(sector) {
  if (sector %in% c("Retail", "Food & Beverage", "Travel & Leisure")) {
    return(sample(10:100, 1))
  } else if (sector %in% c("Manufacturing", "Construction", "Healthcare", "Transportation")) {
    return(sample(50:500, 1))
  } else if (sector %in% c("Technology", "Renewable Energy")) {
    return(sample(50:1000, 1))
  } else {
    return(sample(20:300, 1))
  }
}

generate_num_locations <- function(sector) {
  if (sector %in% c("Retail", "Food & Beverage", "Travel & Leisure")) {
    return(sample(1:10, 1))
  } else {
    return(sample(1:5, 1))
  }
}

generate_annual_revenue <- function(employee_headcount) {
  employee_headcount * sample(50000:100000, 1)
}

generate_premium_amount <- function(annual_revenue, risk_level) {
  base_premium <- annual_revenue * 0.001
  if (risk_level == "Low") {
    return(base_premium * 0.8)
  } else if (risk_level == "High") {
    return(base_premium * 1.2)
  } else {
    return(base_premium)
  }
}

# Generate dataset
set.seed(123) # For reproducibility
data <- tibble(
  Policy_Number = paste0("GL", sprintf("%05d", 1:num_policies)),
  Company_Sector = sample(sectors, num_policies, replace = TRUE),
  Company_Name = map2_chr(Company_Sector, 1:num_policies, generate_company_name),
  Employee_Headcount = map_int(Company_Sector, generate_employee_headcount),
  Number_of_Locations = map_int(Company_Sector, generate_num_locations),
  Annual_Revenue = map_dbl(Employee_Headcount, generate_annual_revenue),
  Risk_Exposure_Level = sample(risk_levels, num_policies, replace = TRUE)
)

data <- data %>%
  mutate(
    Premium_Amount = map2_dbl(Annual_Revenue, Risk_Exposure_Level, generate_premium_amount)
    )

# Display the first few rows of the dataset
print(head(data, 10))
