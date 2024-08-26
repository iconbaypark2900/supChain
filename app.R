# Load required libraries
library(shiny)
library(tidyverse)
library(forecast)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(memoise)
library(cachem)
library(shinythemes)
library(DT)

# Source helper functions
source("data_generation.R")
source("data_analysis.R")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Supply Chain Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Disruptions", tabName = "disruptions", icon = icon("exclamation-triangle"))
    ),
    br(),
    fileInput("file_upload", "Upload CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    dateRangeInput("date_range", "Select Date Range:",
                   start = "2023-01-01",
                   end = "2023-12-31",
                   min = "2023-01-01",
                   max = "2023-12-31"),
    actionButton("update_btn", "Update Dashboard", class = "btn-primary")
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("data_source_info", width = 12)
              ),
              fluidRow(
                valueBoxOutput("avg_order_volume", width = 4),
                valueBoxOutput("avg_lead_time", width = 4),
                valueBoxOutput("avg_supplier_performance", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("order_volume_plot"), width = 6),
                box(plotlyOutput("inventory_level_plot"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("lead_time_plot"), width = 6),
                box(plotlyOutput("supplier_performance_plot"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("combined_metrics_plot"), width = 12)
              )
      ),
      tabItem(tabName = "forecasting",
              fluidRow(
                box(plotlyOutput("forecast_plot"), width = 12)
              ),
              fluidRow(
                box(verbatimTextOutput("trend_analysis"), title = "Trend Analysis", width = 12)
              )
      ),
      tabItem(tabName = "disruptions",
              fluidRow(
                box(DTOutput("disruptions_table"), width = 6),
                box(plotlyOutput("disruption_details"), width = 6)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Function to read and process uploaded CSV
  process_uploaded_data <- function(file) {
    req(file)
    tryCatch(
      {
        df <- read_csv(file$datapath)
        required_cols <- c("date", "order_volume", "inventory_level", "lead_time", "supplier_performance")
        if (!all(required_cols %in% colnames(df))) {
          stop("Uploaded CSV must contain columns: date, order_volume, inventory_level, lead_time, supplier_performance")
        }
        df$date <- as.Date(df$date)
        return(df)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  }

  # Reactive value to store the data source (generated or uploaded)
  data_source <- reactiveVal("generated")

  # Reactive value to store the data
  supply_chain_data <- reactiveVal()

  # Generate initial data or use uploaded data
  observe({
    if (data_source() == "generated") {
      supply_chain_data(generate_supply_chain_data(days = 365, start_date = "2023-01-01"))
    }
  })

  # Update data when file is uploaded
  observeEvent(input$file_upload, {
    tryCatch({
      uploaded_data <- process_uploaded_data(input$file_upload)
      supply_chain_data(uploaded_data)
      data_source("uploaded")
      updateDateRangeInput(session, "date_range",
                           start = min(uploaded_data$date),
                           end = max(uploaded_data$date),
                           min = min(uploaded_data$date),
                           max = max(uploaded_data$date))
      shinyalert("Success", "File uploaded successfully!", type = "success")
    }, error = function(e) {
      shinyalert("Error", paste("File upload failed:", e$message), type = "error")
    })
  })

  # Update data when the button is clicked
  observeEvent(input$update_btn, {
    req(supply_chain_data())
    if (nrow(supply_chain_data()) == 0) {
      shinyalert("Error", "No data available for the selected date range.", type = "error")
    } else {
      data <- supply_chain_data()
      filtered_data <- data %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2])
      supply_chain_data(filtered_data)
    }
  })

  # Add an output to display data source information
  output$data_source_info <- renderInfoBox({
    infoBox(
      "Data Source", 
      if (data_source() == "generated") "Using generated data" else paste("Using uploaded data:", input$file_upload$name),
      icon = icon("database"),
      color = "aqua"
    )
  })

  # Add error handling for calculations and visualizations
  safe_calculate_metrics <- safely(calculate_metrics)
  safe_perform_trend_analysis <- safely(perform_trend_analysis)
  safe_forecast_demand <- safely(forecast_demand)
  safe_identify_disruptions <- safely(identify_disruptions)

  # Create a cache
  cache <- cachem::cache_mem()

  # Memoize expensive functions
  memoised_calculate_metrics <- memoise(calculate_metrics, cache = cache)
  memoised_perform_trend_analysis <- memoise(perform_trend_analysis, cache = cache)
  memoised_forecast_demand <- memoise(forecast_demand, cache = cache)
  memoised_identify_disruptions <- memoise(identify_disruptions, cache = cache)

  # Update reactive expressions to use memoised functions
  metrics <- reactive({
    req(supply_chain_data())
    result <- safe_calculate_metrics(memoised_calculate_metrics(supply_chain_data()))
    if (!is.null(result$error)) {
      shinyalert("Error", paste("Failed to calculate metrics:", result$error), type = "error")
      return(NULL)
    }
    result$result
  })

  trend_data <- reactive({
    req(supply_chain_data())
    result <- safe_perform_trend_analysis(memoised_perform_trend_analysis(supply_chain_data()))
    if (!is.null(result$error)) {
      shinyalert("Error", paste("Failed to perform trend analysis:", result$error), type = "error")
      return(NULL)
    }
    result$result
  })

  forecast_result <- reactive({
    req(supply_chain_data())
    result <- safe_forecast_demand(memoised_forecast_demand(supply_chain_data()))
    if (!is.null(result$error)) {
      shinyalert("Error", paste("Failed to generate forecast:", result$error), type = "error")
      return(NULL)
    }
    result$result
  })

  disruptions <- reactive({
    req(supply_chain_data())
    result <- safe_identify_disruptions(memoised_identify_disruptions(supply_chain_data()))
    if (!is.null(result$error)) {
      shinyalert("Error", paste("Failed to identify disruptions:", result$error), type = "error")
      return(NULL)
    }
    result$result
  })

  # Optimize data processing for larger datasets
  observe({
    data <- supply_chain_data()
    if (nrow(data) > 10000) {
      # For large datasets, downsample for plotting
      data_downsampled <- data %>%
        group_by(date = floor_date(date, unit = "week")) %>%
        summarise(across(everything(), mean, na.rm = TRUE))
      supply_chain_data_downsampled(data_downsampled)
    } else {
      supply_chain_data_downsampled(data)
    }
  })

  # Update the render functions to handle NULL results
  output$avg_order_volume <- renderValueBox({
    req(metrics())
    valueBox(
      round(metrics()$avg_order_volume, 2),
      "Avg. Order Volume",
      icon = icon("shopping-cart"),
      color = "primary"
    )
  })

  output$avg_lead_time <- renderValueBox({
    req(metrics())
    valueBox(
      round(metrics()$avg_lead_time, 2),
      "Avg. Lead Time (days)",
      icon = icon("clock"),
      color = "info"
    )
  })

  output$avg_supplier_performance <- renderValueBox({
    req(metrics())
    valueBox(
      paste0(round(metrics()$avg_supplier_performance * 100, 2), "%"),
      "Avg. Supplier Performance",
      icon = icon("star"),
      color = "success"
    )
  })

  # Create interactive visualizations
  output$order_volume_plot <- renderPlotly({
    req(supply_chain_data_downsampled())
    req(trend_data())
    data <- supply_chain_data_downsampled()
    trend <- trend_data()$data
    
    plot_ly() %>%
      add_lines(x = ~date, y = ~order_volume, name = "Order Volume", data = data) %>%
      add_lines(x = ~date, y = ~order_volume_ma7, name = "7-day MA", data = trend, line = list(dash = "dash")) %>%
      add_lines(x = ~date, y = ~order_volume_ma30, name = "30-day MA", data = trend, line = list(dash = "dot")) %>%
      layout(title = "Order Volume Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Order Volume"),
             hovermode = "compare")
  })

  output$inventory_level_plot <- renderPlotly({
    req(supply_chain_data_downsampled())
    data <- supply_chain_data_downsampled()
    
    plot_ly(data, x = ~date, y = ~inventory_level, type = "scatter", mode = "lines",
            hoverinfo = "text",
            text = ~paste("Date: ", date, "<br>Inventory Level: ", round(inventory_level, 2))) %>%
      layout(title = "Inventory Level Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Inventory Level"))
  })

  output$lead_time_plot <- renderPlotly({
    req(supply_chain_data_downsampled())
    data <- supply_chain_data_downsampled()
    
    plot_ly(data, x = ~date, y = ~lead_time, type = "scatter", mode = "markers",
            marker = list(size = 8, color = ~lead_time, colorscale = "Viridis", showscale = TRUE),
            hoverinfo = "text",
            text = ~paste("Date: ", date, "<br>Lead Time: ", round(lead_time, 2), "days")) %>%
      layout(title = "Lead Time Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Lead Time (days)"))
  })

  output$supplier_performance_plot <- renderPlotly({
    req(supply_chain_data_downsampled())
    data <- supply_chain_data_downsampled()
    
    plot_ly(data, x = ~date, y = ~supplier_performance, type = "scatter", mode = "lines",
            line = list(color = ~supplier_performance, colorscale = "RdYlGn", showscale = TRUE),
            hoverinfo = "text",
            text = ~paste("Date: ", date, "<br>Supplier Performance: ", round(supplier_performance * 100, 2), "%")) %>%
      layout(title = "Supplier Performance Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Supplier Performance"))
  })

  # Add a new output for combined metrics
  output$combined_metrics_plot <- renderPlotly({
    req(supply_chain_data_downsampled())
    data <- supply_chain_data_downsampled()
    
    plot_ly() %>%
      add_lines(x = ~date, y = ~order_volume, name = "Order Volume", data = data, yaxis = "y1") %>%
      add_lines(x = ~date, y = ~inventory_level, name = "Inventory Level", data = data, yaxis = "y2") %>%
      add_lines(x = ~date, y = ~lead_time, name = "Lead Time", data = data, yaxis = "y3") %>%
      layout(title = "Combined Supply Chain Metrics",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Order Volume", side = "left"),
             yaxis2 = list(title = "Inventory Level", overlaying = "y", side = "right"),
             yaxis3 = list(title = "Lead Time", overlaying = "y", side = "right"),
             legend = list(x = 1.1, y = 0.5))
  })

  # Update the forecast plot
  output$forecast_plot <- renderPlotly({
    req(supply_chain_data())
    req(forecast_result())
    data <- supply_chain_data()
    forecast_result <- forecast_result()
    
    plot_ly() %>%
      add_lines(x = data$date, y = data$order_volume, name = "Historical Data") %>%
      add_lines(x = time(forecast_result$forecast$mean), y = forecast_result$forecast$mean, name = "Forecast") %>%
      add_ribbons(x = time(forecast_result$forecast$mean), 
                  ymin = forecast_result$forecast$lower[, 2], 
                  ymax = forecast_result$forecast$upper[, 2], 
                  name = "95% Confidence Interval", 
                  fillcolor = "rgba(68, 68, 68, 0.3)", 
                  line = list(color = "transparent")) %>%
      layout(title = "Demand Forecast", xaxis = list(title = "Date"), yaxis = list(title = "Order Volume"))
  })

  # Add a new output for trend analysis
  output$trend_analysis <- renderText({
    req(trend_data())
    trend <- trend_data()$trend_analysis
    paste(
      "Overall Trend:", trend$overall_trend,
      "\nVolatility:", round(trend$volatility, 2),
      "\nPeak Day:", as.character(trend$peak_day),
      "\nTrough Day:", as.character(trend$trough_day)
    )
  })

  # Add a new output for disruptions
  output$disruptions_table <- renderDT({
    req(disruptions())
    datatable(disruptions(), options = list(pageLength = 5))
  })

  # Add drill-down capability for disruptions
  output$disruption_details <- renderPlotly({
    req(input$disruptions_table_rows_selected)
    req(disruptions())
    req(supply_chain_data())
    
    selected_disruption <- disruptions()[input$disruptions_table_rows_selected, ]
    data <- supply_chain_data()
    
    start_date <- selected_disruption$date - 7
    end_date <- selected_disruption$date + 7
    
    filtered_data <- data %>% filter(date >= start_date & date <= end_date)
    
    plot_ly() %>%
      add_lines(x = ~date, y = ~lead_time, name = "Lead Time", data = filtered_data, yaxis = "y1") %>%
      add_lines(x = ~date, y = ~supplier_performance, name = "Supplier Performance", data = filtered_data, yaxis = "y2") %>%
      add_markers(x = selected_disruption$date, y = selected_disruption$lead_time, name = "Disruption", yaxis = "y1",
                  marker = list(size = 10, color = "red", symbol = "star")) %>%
      layout(title = paste("Disruption Details for", selected_disruption$date),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Lead Time", side = "left"),
             yaxis2 = list(title = "Supplier Performance", overlaying = "y", side = "right"),
             legend = list(x = 1.1, y = 0.5))
  })

  # Validate date range input
  observe({
    req(input$date_range)
    if (input$date_range[1] > input$date_range[2]) {
      shinyalert("Invalid Date Range", "Start date must be before end date.", type = "error")
      updateDateRangeInput(session, "date_range",
                           start = input$date_range[2],
                           end = input$date_range[2])
    }
  })

  # Show disruption details when a row is selected
  observe({
    req(input$disruptions_table_rows_selected)
    shinyjs::show("disruption_details")
  })
}

# Run the app
shinyApp(ui = ui, server = server)