library(shiny)
library(plotly)
library(quantmod)
library(htmltools)
library(shinydashboard)

# Set the directory paths
wd <- getwd()

# List of prediction dates
pred_dates <- c("2024-04-26", "2024-04-29", "2024-04-30", "2024-05-01", "2024-05-02",
                "2024-05-03", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09")

# Model choices
models <- c("SNaive", "SARIMA", "SVM", "Elman")
models2 <- c("SVM", "Elman")

symbols <- c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS",
             "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS", "INDIGO.NS",
             "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS", "ADANIGREEN.NS","BOMDYEING.NS")

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Plotly Graphs Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("View Forecaster", tabName = "viewForecaster", icon = icon("dashboard")),
      menuItem("View Performance", tabName = "viewPerformance", icon = icon("line-chart")),
      menuItem("Stock Price", tabName = "StockPrice", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "viewForecaster",
              fluidRow(
                column(6, selectInput("selectedModel", "Select Model", choices = models)),
                column(6, selectInput("selectedDate", "Select Date", choices = pred_dates, selected = pred_dates[1]))
              ),
              fluidRow(
                h3("Efficient Frontier Plot"),
                htmlOutput("efficientFrontierPlot")
              ),
              fluidRow(
                column(12, h3("Portfolio Metrics")),
                column(6, h3("Total Portfolio Value"), htmlOutput("portfolioValue")),
                column(6, h3("Total Profit"), htmlOutput("totalProfit"))
              ),
              fluidRow(
                column(6, h3("Max Sharpe Ratio Portfolio"), htmlOutput("maxSharpe")),
                column(6, h3("Min Variance Portfolio"), htmlOutput("minVariance")),
                column(6, h3("Naive Portfolio"), htmlOutput("naivePortfolio")),
                column(6, h3("Risk Parity Portfolio"), htmlOutput("riskParity")),
                column(12, h3("Transactions"), htmlOutput("transactions"))
              )
      ),
      tabItem(tabName = "viewPerformance",
              fluidRow(
                column(6, selectInput("selectedModel2", "Select Model", choices = models2)),
                column(6, selectInput("selectedDate2",  "Select Date", choices = pred_dates, selected = pred_dates[1])),
                column(6, selectInput("selectedCompany2", "Select Company", choices = symbols))
              ),
              fluidRow(
                column(width = 1),
                column(width = 10, h3('Train Test Performance'),htmlOutput('traintestchart')),
                column(width = 1)
              )
              ),
      tabItem(tabName = "StockPrice",
              fluidRow(
                column(6, selectInput("company","Select Company", choices = symbols))
              ),
              fluidRow(
                column(width = 1),
                column(width = 10, h3('Historical Stock Price'), plotlyOutput('stockprice')),
                column(width = 1)
              ),
              fluidRow(
                column(width = 1),
                column(width = 10, h3('Market Behavior'), plotlyOutput('nifty50')),
                column(width = 1)
              ),
              )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  addResourcePath("resources", wd)
  
  output$stockprice <- renderPlotly({
    req(input$company)
    price = getSymbols(input$company, src = 'yahoo', auto.assign = F, from = '2023-01-01', to = Sys.Date())[,4]
    plot_ly(x = index(price), y = as.numeric(price[,1]), type = 'scatter', mode = 'lines', name = 'Closing Price') %>% 
      layout(title = paste('Closing Price of', input$company), xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  })
  
  output$nifty50 <- renderPlotly({
    price = getSymbols("^NSEI", src = 'yahoo', auto.assign = F, from = '2023-01-01', to = Sys.Date())[,4]
    plot_ly(x = index(price), y = as.numeric(price[,1]), type = 'scatter', mode = 'lines', name = 'Closing Price') %>% 
      layout(title = "Nifty 50 Index", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  })
  
  output$traintestchart <- renderUI({
    req(input$selectedCompany2, input$selectedDate2, input$selectedModel2)
    a = ifelse(input$selectedModel2 =="Elman",
               paste0("resources/ELMAN/Elman_plots_for_ ",input$selectedDate2,'/',input$selectedCompany2,'_plot.html'),
               paste0("resources/SVM/SVM_plots_for_ ",input$selectedDate2,'/',input$selectedCompany2,'_',input$selectedDate2,'_plot.html'))
    tags$iframe(style = "width:100%; height:400px;", src = paste0(session$clientData$url_prefix, a))
  })
  
  output$efficientFrontierPlot <- renderUI({
    req(input$selectedModel, input$selectedDate)  # Ensure that both inputs are set
    model <- tolower(input$selectedModel)
    date <- input$selectedDate
    path <- paste0("resources/", model, "/", date, " ", model, "_weights/efficient frontier_plot.html")
    tags$iframe(style = "width:100%; height:400px;", src = paste0(session$clientData$url_prefix, path))
  })
  
  plotTypes <- c("portfolioValue" = "Total Portfolio Value.html",
                 "totalProfit" = "Total Profit.html",
                 "maxSharpe" = "Max Sharpe Ratio Portfolio Holdings.html",
                 "minVariance" = "Min Variance Portfolio Holdings.html",
                 "naivePortfolio" = "Naive Portfolio Holdings.html",
                 "riskParity" = "Risk Parity Portfolio Holdings.html",
                 "transactions" = "Transactions.html")
  
  for (plotType in names(plotTypes)) {
    local({
      type <- plotType
      file <- plotTypes[type]
      output[[type]] <- renderUI({
        req(input$selectedModel)  # Ensure model is selected
        model <- tolower(input$selectedModel)
        path <- paste0("resources/", model, "/", file)
        tags$iframe(style = "width:100%; height:400px;", src = paste0(session$clientData$url_prefix, path))
      })
    })
  }
}

shinyApp(ui = ui, server = server)
