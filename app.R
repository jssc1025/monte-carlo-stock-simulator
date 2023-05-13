# Load necessary libraries
library(shiny)
library(shinydashboard)
library(quantmod)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
library(rmarkdown)

# Define confidence intervals
ci_levels <- c(0.9, 0.95, 0.99)

# Define UI using shinydashboard for a more modern look
ui <- dashboardPage(
  dashboardHeader(title = "Monte Carlo Stock Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation Options", tabName = "simulate", icon = icon("cogs")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      textInput("symbol", "Stock Ticker Symbol"),
      numericInput("riskFreeRate", "Risk-free rate", value = 0.02, min = 0, max = 1, step = 0.01),
      checkboxInput("simulate30", "30-day Simulation", value = TRUE),
      checkboxInput("simulate180", "180-day Simulation"),
      checkboxInput("simulate365", "365-day Simulation"),
      actionButton("simulateBtn", "Simulate")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "simulate",
              fluidRow(
                box(plotOutput("plot30"), title = "30-day Simulation", solidHeader = TRUE, status = "primary"),
                box(plotOutput("plot180"), title = "180-day Simulation", solidHeader = TRUE, status = "primary"),
                box(plotOutput("plot365"), title = "365-day Simulation", solidHeader = TRUE, status = "primary"),
                box(plotOutput("histogram"), title = "Histogram of Final Simulated Prices (30-day)", solidHeader = TRUE, status = "primary"),
                box(verbatimTextOutput("metrics"), title = "Additional Metrics", solidHeader = TRUE, status = "primary")
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(includeMarkdown("about.Rmd"), title = "About", solidHeader = TRUE, status = "primary")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Create reactive values to store symbol
  values <- reactiveValues(symbol = NULL, rf_rate = NULL)
  
  # Function to retrieve stock data and handle missing values
  getStockData <- function(symbol, start_date, end_date) {
    data <- tryCatch(
      getSymbols.yahoo(symbol, from = start_date, to = end_date, auto.assign = FALSE)[, 6],
      error = function(e) {
        message(paste("Error retrieving data for symbol", symbol))
        return(NULL)
      }
    )
    
    if (is.null(data)) {
      return(NULL)
    }
    
    if (anyNA(data)) {
      message(paste("Missing values detected in data for symbol", symbol))
      data <- na.approx(data)
    }
    
    # Convert data to a single-column vector
    data <- as.vector(data)
    
    return(data)
  }
  
  performSimulation <- function(data, num_simulations, num_days, ci_levels) {
    log_returns <- diff(log(data))
    daily_return_mean <- mean(log_returns, na.rm = TRUE)
    daily_return_sd <- sd(log_returns, na.rm = TRUE)
    
    # Incorporate the drift and volatility into simulations using GBM model
    drift <- daily_return_mean - (0.5 * daily_return_sd^2) + input$riskFreeRate
    simulations <- matrix(0, nrow = num_days, ncol = num_simulations)
    simulations[1, ] <- data[length(data)]
    
    for (i in 2:num_days) {
      simulations[i, ] <- simulations[i-1, ] * exp(drift + daily_return_sd * rnorm(num_simulations))
    }
    
    # Calculate additional metrics
    mean_prices <- apply(simulations, 1, mean)
    sd_prices <- apply(simulations, 1, sd)
    confidence_intervals <- matrix(0, nrow = num_days, ncol = length(ci_levels) * 2)
    
    for (i in 1:length(ci_levels)) {
      ci <- ci_levels[i]
      lower_bound <- mean_prices - qnorm(1 - ci/2) * sd_prices / sqrt(num_simulations)
      upper_bound <- mean_prices + qnorm(1 - ci/2) * sd_prices / sqrt(num_simulations)
      confidence_intervals[, (2*i - 1):(2*i)] <- cbind(lower_bound, upper_bound)
    }
    
    return(list(simulations = simulations, mean_prices = mean_prices,
                sd_prices = sd_prices, confidence_intervals = confidence_intervals))
  }
  
  # Define a reactive expression for simulation results
  simulationResults <- eventReactive(input$simulateBtn, {
    symbol <- toupper(input$symbol)
    rf_rate <- as.numeric(input$rf_rate)
    
    if (symbol == "") {
      return(NULL)
    }
    
    data <- getStockData(symbol, "2021-01-01", "2023-05-13")
    
    if (is.null(data)) {
      return(NULL)
    }
    
    results <- list()
    
    if (input$simulate30) {
      results$simulations30 <- performSimulation(data, 100, 30, ci_levels)
    }
    
    if (input$simulate180) {
      results$simulations180 <- performSimulation(data, 100, 180, ci_levels)
    }
    
    if (input$simulate365) {
      results$simulations365 <- performSimulation(data, 100, 365, ci_levels)
    }
    
    values$symbol <- symbol
    values$rf_rate <- rf_rate
    
    return(results)
  })
  
  # Plot 30-day simulation
  output$plot30 <- renderPlot({
    results <- simulationResults()
    symbol <- values$symbol
    if (!is.null(results$simulations30) && !is.null(symbol)) {
      simulations <- results$simulations30
      ggplot() +
        geom_line(aes(x = 1:30, y = simulations$simulations[, 1]), color = "blue") +
        labs(title = paste("30-day Monte Carlo Simulation for", symbol),
             x = 'Days', y = 'Stock Price') +
        theme_minimal()
    }
  })
  
  # Plot 180-day simulation
  output$plot180 <- renderPlot({
    results <- simulationResults()
    symbol <- values$symbol
    
    if (!is.null(results$simulations180) && !is.null(symbol)) {
      simulations <- results$simulations180
      ggplot() +
        geom_line(aes(x = 1:180, y = simulations$simulations[, 1]), color = "blue") +
        labs(title = paste("180-day Monte Carlo Simulation for", symbol),
             x = 'Days', y = 'Stock Price') +
        theme_minimal()
    }
  })
  
  # Plot 365-day simulation
  output$plot365 <- renderPlot({
    results <- simulationResults()
    symbol <- values$symbol
    
    if (!is.null(results$simulations365) && !is.null(symbol)) {
      simulations <- results$simulations365
      ggplot() +
        geom_line(aes(x = 1:365, y = simulations$simulations[, 1]), color = "blue") +
        labs(title = paste("365-day Monte Carlo Simulation for", symbol),
             x = 'Days', y = 'Stock Price') +
        theme_minimal()
    }
  })
  
  # Histogram of final simulated prices
  output$histogram <- renderPlot({
    results <- simulationResults()
    
    if (!is.null(results) && !is.null(results$simulations30)) {
      symbol <- values$symbol
      final_prices <- results$simulations30$simulations[30, ]
      ggplot(data.frame(final_prices), aes(x = final_prices)) +
        geom_histogram(fill = "blue", color = "black", bins = 30) +
        labs(title = "Histogram of Final Simulated Prices (30-day)", x = "Price") +
        theme_minimal()
    }
  })
  
  # Additional metrics
  output$metrics <- renderPrint({
    results <- simulationResults()
    
    if (!is.null(results) && !is.null(results$simulations30)) {
      symbol <- values$symbol
      rf_rate <- values$rf_rate
      final_prices <- results$simulations30$simulations[30, ]
      max_drawdown <- max(1 - (final_prices / cummax(final_prices)))
      var_95 <- quantile(final_prices, 0.05)
      
      # Calculate Expected Shortfall
      es_95 <- mean(final_prices[final_prices <= var_95])
      
      cat("30-day Simulation\n")
      cat("Maximum Drawdown:", round(max_drawdown, 4), "\n")
      cat("Value at Risk (95%):", round(var_95, 4), "\n")
      cat("Expected Shortfall (95%):", round(es_95, 4), "\n")
      cat("Risk-Free Rate:", round(rf_rate, 4), "\n")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

    
    
