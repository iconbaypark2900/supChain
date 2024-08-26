# Function to generate synthetic supply chain data
library(tidyverse)

generate_supply_chain_data <- function(days = 365, start_date = "2023-01-01") {
  set.seed(123)  # For reproducibility
  
  date_seq <- seq(as.Date(start_date), by = "day", length.out = days)
  
  # Generate base order volume with weekly seasonality and trend
  base_order_volume <- 100 + 0.1 * (1:days) + 20 * sin(2 * pi * (1:days) / 7)
  
  # Add holiday effects (e.g., increased orders around Christmas)
  holiday_effect <- ifelse(format(date_seq, "%m-%d") %in% c("12-23", "12-24", "12-25"), 50, 0)
  
  # Generate data
  data <- tibble(
    date = date_seq,
    order_volume = rpois(days, lambda = base_order_volume + holiday_effect),
    inventory_level = cumsum(rnorm(days, mean = 10, sd = 5)) + 500,  # Start with base inventory
    lead_time = rexp(days, rate = 1/3) + rnorm(days, mean = 0, sd = 0.5),  # Add some noise
    supplier_performance = pmin(pmax(rnorm(days, mean = 0.9, sd = 0.05), 0), 1)  # Ensure between 0 and 1
  )
  
  # Add sudden supply chain disruption
  disruption_start <- sample(60:(days-30), 1)
  disruption_end <- disruption_start + sample(5:15, 1)
  data$lead_time[disruption_start:disruption_end] <- data$lead_time[disruption_start:disruption_end] * runif(disruption_end - disruption_start + 1, 1.5, 3)
  data$supplier_performance[disruption_start:disruption_end] <- data$supplier_performance[disruption_start:disruption_end] * runif(disruption_end - disruption_start + 1, 0.6, 0.9)
  
  return(data)
}