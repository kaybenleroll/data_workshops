# Load necessary libraries
library(tidyverse)
library(lubridate)

# Set the seed for reproducibility
set.seed(123)

# Number of claims
num_claims <- 100

# Function to generate timestamps
generate_timestamps <- function(num_movements) {
  start_date <- ymd("2020-01-01")
  end_date <- ymd("2023-01-01")
  sort(sample(seq.Date(start_date, end_date, by = "day"), num_movements, replace = TRUE))
}

# Function to generate payment amounts with a heavy tail
generate_payments <- function(num_movements) {
  # Parameters for the lognormal distribution
  meanlog <- 10
  sdlog <- 1

  # Generate lognormal payments
  payments <- rlnorm(num_movements, meanlog = meanlog, sdlog = sdlog)

  # Apply a Pareto transformation to some of the payments to create a heavy tail
  pareto_threshold <- quantile(payments, 0.95)
  heavy_tail <- payments > pareto_threshold
  payments[heavy_tail] <- payments[heavy_tail] * rpareto(sum(heavy_tail), shape = 2)

  return(payments)
}

# Function to generate claims data
generate_claim_data <- function(num_claims) {
  claims <- tibble(
    Claim_ID = sprintf("CL%05d", 1:num_claims),
    Num_Movements = sample(5:15, num_claims, replace = TRUE)
  )

  claims <- claims %>%
    rowwise() %>%
    mutate(
      Timestamps = list(generate_timestamps(Num_Movements)),
      Payments = list(generate_payments(Num_Movements))
    ) %>%
    unnest(cols = c(Timestamps, Payments)) %>%
    mutate(
      Movement_Type = "Paid",
      Incurred = cumsum(Payments),
      Outstanding = Incurred - Payments
    )

  return(claims)
}

# Generate claims data
claims_data <- generate_claim_data(num_claims)

# View the first few rows of the data
print(head(claims_data, 10))
