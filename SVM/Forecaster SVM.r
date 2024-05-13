library(plotly)
library(foreach)
library(ggplot2)
library(Metrics)
library(forecast)
library(openxlsx)
library(quantmod)
library(iterators)
library(doParallel)
library(htmlwidgets)

symbols <- c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS",
             "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS", "INDIGO.NS",
             "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS", "ADANIGREEN.NS","BOMDYEING.NS")

# Scale and unscale data functions
scale.data <- function(x) { (x - min(x)) / (max(x) - min(x)) }
unscale.data <- function(x, xmin, xmax) { x * (xmax - xmin) + xmin }

total_symbols <- length(symbols)

forecasts <- data.frame(forecast = rep(NA, total_symbols), roi = rep(NA, total_symbols), row.names = symbols)

plt = list()  
folder_name <- paste("SVM_plots_for_", pred_date)

dir.create(folder_name, showWarnings = FALSE)

for (idx in 1:total_symbols) {
  
  symbol <- symbols[idx]
  cat(sprintf("Processing %d/%d: %s\n", idx, total_symbols, symbol))
  
  df <- getSymbols(symbol, src = 'yahoo', auto.assign = F, from = '2023-01-01', to = as.Date(pred_date))
  df <- ts(data = as.numeric(df[, 4]), frequency = 365)
  
  # Process data
  used_log <- any(df < 0)
  y <- if(used_log) df else log(df)
  min.data <- min(y)
  max.data <- max(y)
  y <- scale.data(y)
  y <- as.zoo(y)
  
  # Matrix construction for Elman Network
  freq <- 7
  m <- data.frame(matrix(NA, ncol = freq, nrow = length(y)))
  for (i in 1:freq) { m[, i] = Lag(y, k = i) }
  x <- cbind(m, y)
  x <- x[-(1:freq),]
  
  # Defining training and testing sets
  n <- nrow(x)
  h <- 60
  n.train <- n - h
  outputs <- x[, freq + 1]
  inputs <- x[, 1:freq]
  
  train_data <- x[1:n.train, ]
  test_data = x[-(1:n.train),]
  
  svmodel <- e1071::svm(y ~ ., data = train_data, type = "eps-regression", kernel = "linear")
  
  svm_pred <- predict(svmodel, newdata = test_data[, -ncol(test_data)])
  svm_pred <-  exp(unscale.data(svm_pred, min.data, max.data))
  actual_test <-  exp(unscale.data(outputs[-(1:n.train)], min.data, max.data))
  mape_test = 100 * round(mape(actual = actual_test, predicted = svm_pred), 4)
  rmse_test = sqrt(mean((actual_test - svm_pred)^2))
  
  svm.df = cbind.data.frame(n=1:length(svm_pred),actual_test, svm_pred)
  title = paste0("SVM Regression(g=",round(svmodel$gamma,3),",ep=",svmodel$epsilon,",cost=",svmodel$cost,") for ",symbol," ",pred_date)
  plt[[idx]] = plot_ly(data = svm.df, x = ~n) %>%
    add_lines(y = ~actual_test, name = 'Actual Data', line = list(color = 'navy')) %>% add_markers(y = ~actual_test, name = 'Actual Data', color = I('navy'), showlegend = FALSE) %>%
    add_lines(y = ~svm_pred, name = 'Predictions', line = list(color = 'red')) %>% add_markers(y = ~svm_pred, name = 'Predictions', color = I('red'), showlegend = FALSE) %>%
    layout(title = title, xaxis = list(title = ""), yaxis = list(title = ""), showlegend = FALSE, plot_bgcolor = "rgba(0,0,0,0)",  paper_bgcolor = "rgba(0,0,0,0)" )
  
  # saveWidget(plt[[idx]], file = paste0(folder_name,"/", symbol,"_", pred_date,"_plot.html"))
  
  # Predict the next day
  last_input <- matrix(tail(y, freq), nrow=1)
  next_value <- exp(unscale.data(predict(svmodel, last_input), min.data, max.data))
  forecasts[idx,1] <- next_value
  forecasts[idx,2] <- as.numeric((next_value-tail(df, 1))/next_value)
  colnames(forecasts) = c('Forecasts','ROI') 

}  

# Output the forecasts
excel_filename <- paste("SVM_forecasts_", pred_date, ".xlsx", sep = "")
write.xlsx(forecasts, excel_filename)
cat("Forecasts have been saved to", excel_filename, "\n")