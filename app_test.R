library(shinytest)

test_that("Shiny app works correctly", {
  # Record initial app state
  app <- ShinyDriver$new(".")
  app$snapshotInit("initial")

  # Test file upload
  app$uploadFile(file_upload = "path/to/test_data.csv")
  app$click("update_btn")
  app$snapshot()

  # Test date range selection
  app$setInputs(date_range = c("2023-02-01", "2023-03-01"))
  app$click("update_btn")
  app$snapshot()

  # Test tab switching
  app$click(selector = "a[data-value='forecasting']")
  app$snapshot()

  app$click(selector = "a[data-value='disruptions']")
  app$snapshot()
})