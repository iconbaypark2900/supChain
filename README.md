# Supply Chain Analytics Dashboard

This Shiny application provides a comprehensive dashboard for supply chain analytics, offering insights into order volumes, inventory levels, lead times, and supplier performance.

## Features

- Interactive visualizations of key supply chain metrics
- Demand forecasting using time series analysis
- Trend analysis and identification of supply chain disruptions
- Support for both generated and uploaded data
- Responsive design with a user-friendly interface

## Getting Started

### Prerequisites

- R (version 4.0.0 or higher)
- RStudio (recommended for development)

### Installation

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/supply-chain-analytics.git
   ```

2. Open the project in RStudio or your preferred R environment.

3. Install the required packages:
   ```R
   install.packages(c("shiny", "tidyverse", "forecast", "plotly", "shinydashboard", "shinyjs", "shinyalert", "memoise", "cachem", "shinythemes", "DT"))
   ```

### Running the Application

1. Open `app.R` in RStudio.
2. Click the "Run App" button or run the following command in the R console:
   ```R
   shiny::runApp()
   ```

## Usage

1. The dashboard initially loads with generated data.
2. To use your own data, click "Browse" in the sidebar to upload a CSV file.
3. Adjust the date range using the date picker in the sidebar.
4. Click "Update Dashboard" to refresh the visualizations and analysis.
5. Navigate between different tabs to explore various aspects of the supply chain data.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
