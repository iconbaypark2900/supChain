library(tidyverse)
library(forecast)
library(TTR)

# Function to calculate key supply chain metrics
calculate_metrics <- function(data) {
  metrics <- list(
    avg_order_volume = mean(data$order_volume),
    avg_lead_time = mean(data$lead_time),
    avg_supplier_performance = mean(data$supplier_performance),
    inventory_turnover = sum(data$order_volume) / mean(data$inventory_level),
    order_fulfillment_rate = 1 - sum(pmax(data$order_volume - data$inventory_level, 0)) / sum(data$order_volume),
    perfect_order_rate = mean(data$supplier_performance > 0.95 & data$lead_time < quantile(data$lead_time, 0.25)),
    cash_to_cash_cycle = mean(data$lead_time) + mean(diff(data$inventory_level) / diff(as.numeric(data$date)))
  )
  
  return(metrics)
}

# Function to perform trend analysis
perform_trend_analysis <- function(data) {
  # Calculate moving averages
  data$order_volume_ma7 <- SMA(data$order_volume, n = 7)
  data$order_volume_ma30 <- SMA(data$order_volume, n = 30)
  
  # Calculate rate of change
  data$order_volume_roc <- ROC(data$order_volume, n = 1)
  
  # Identify trends
  trend_analysis <- list(
    overall_trend = if(coef(lm(order_volume ~ as.numeric(date), data = data))[2] > 0) "Increasing" else "Decreasing",
    volatility = sd(data$order_volume_roc, na.rm = TRUE),
    peak_day = data$date[which.max(data$order_volume)],
    trough_day = data$date[which.min(data$order_volume)]
  )
  
  return(list(data = data, trend_analysis = trend_analysis))
}

# Function to forecast demand
forecast_demand <- function(data, forecast_horizon = 30) {
  ts_data <- ts(data$order_volume, frequency = 7)  # Assuming weekly seasonality
  model <- auto.arima(ts_data)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  # Calculate forecast accuracy metrics
  accuracy_metrics <- accuracy(forecast_result)
  
  return(list(forecast = forecast_result, accuracy = accuracy_metrics))
}

# Function to identify supply chain disruptions
identify_disruptions <- function(data, threshold = 2) {
  # Calculate z-scores for lead time and supplier performance
  data$lead_time_zscore <- scale(data$lead_time)
  data$supplier_performance_zscore <- scale(data$supplier_performance)
  
  # Identify disruptions
  disruptions <- data %>%
    filter(lead_time_zscore > threshold | supplier_performance_zscore < -threshold) %>%
    select(date, lead_time, supplier_performance)
  
  return(disruptions)
}