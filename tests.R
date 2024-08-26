library(testthat)
library(shiny)
source("data_generation.R")
source("data_analysis.R")

test_that("generate_supply_chain_data produces correct output", {
  data <- generate_supply_chain_data(days = 30)
  expect_equal(nrow(data), 30)
  expect_equal(ncol(data), 5)
  expect_true(all(c("date", "order_volume", "inventory_level", "lead_time", "supplier_performance") %in% colnames(data)))
})

test_that("calculate_metrics produces correct output", {
  data <- generate_supply_chain_data(days = 30)
  metrics <- calculate_metrics(data)
  expect_type(metrics, "list")
  expect_true(all(c("avg_order_volume", "avg_lead_time", "avg_supplier_performance", "inventory_turnover", "order_fulfillment_rate", "perfect_order_rate", "cash_to_cash_cycle") %in% names(metrics)))
})

test_that("perform_trend_analysis produces correct output", {
  data <- generate_supply_chain_data(days = 30)
  trend_data <- perform_trend_analysis(data)
  expect_type(trend_data, "list")
  expect_true(all(c("data", "trend_analysis") %in% names(trend_data)))
  expect_true(all(c("overall_trend", "volatility", "peak_day", "trough_day") %in% names(trend_data$trend_analysis)))
})

test_that("forecast_demand produces correct output", {
  data <- generate_supply_chain_data(days = 30)
  forecast_result <- forecast_demand(data)
  expect_type(forecast_result, "list")
  expect_true(all(c("forecast", "accuracy") %in% names(forecast_result)))
})

test_that("identify_disruptions produces correct output", {
  data <- generate_supply_chain_data(days = 30)
  disruptions <- identify_disruptions(data)
  expect_s3_class(disruptions, "data.frame")
  expect_true(all(c("date", "lead_time", "supplier_performance") %in% colnames(disruptions)))
})