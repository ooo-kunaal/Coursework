library(RSNNS)
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

# Initialize an Excel workbook
wb <- createWorkbook()

# Setup parallel backend to use multiple cores
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)

# Process each symbol
total_symbols <- length(symbols)

for (idx in 1:total_symbols) {
  
  symbol <- symbols[idx]
  cat(sprintf("Processing %d/%d: %s\n", idx, total_symbols, symbol,"\n"))
  
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
  
  # Grid Search Loop
  results <- foreach(l1 = 2:25, .combine = rbind) %:%
    foreach(l2 = 2:10, .combine = rbind, .packages = c("RSNNS", "Metrics")) %dopar% {
      hidden <- if (l2 == 0) l1 else c(l1, l2)
      set.seed(2023)
      fit.elman <- elman(x = inputs[1:n.train, ], y = outputs[1:n.train], size = hidden, maxit = 500,learnFuncParams = c(0.01))
      pred.elman_train <- predict(fit.elman, inputs[1:n.train, ])
      pred.elman_test <- predict(fit.elman, inputs[-(1:n.train), ])
      
      preds_train <- if(used_log) exp(unscale.data(pred.elman_train, min(df), max(df)))      else unscale.data(pred.elman_train, min(df), max(df))
      preds_test <- if(used_log) exp(unscale.data(pred.elman_test, min(df), max(df)))       else unscale.data(pred.elman_test, min(df), max(df))
      actual_train <- if(used_log) exp(unscale.data(outputs[1:n.train], min(df), max(df)))    else unscale.data(outputs[1:n.train], min(df), max(df))
      actual_test <- if(used_log) exp(unscale.data(outputs[-(1:n.train)], min(df), max(df))) else unscale.data(outputs[-(1:n.train)], min(df), max(df))
      
      mape_train <- 100 * round(mape(actual = actual_train, predicted = preds_train), 4)
      mape_test <- 100 * round(mape(actual = actual_test, predicted = preds_test), 4)
      rmse_train <- sqrt(mean((actual_train - preds_train)^2))
      rmse_test <- sqrt(mean((actual_test - preds_test)^2))
      
      data.frame(l1 = l1, l2 = l2, MAPE_Train = mape_train, MAPE_Test = mape_test, RMSE_Train = rmse_train, RMSE_Test = rmse_test)
    }
  
  # Add data to individual worksheets
  sheet_name <- paste(symbol, "Details", sep="_")
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, results)

}

# Save the workbook
saveWorkbook(wb, paste(pred_date,"Model_Performance_Elman.xlsx"), overwrite = TRUE)
cat("Excel file with model performance metrics has been created: 'Model_Performance_Elman.xlsx'\n")

# Stop the parallel cluster
stopCluster(cl)

# Now read the best model parameters and predict the next day's value
forecasts <- data.frame(forecast = rep(NA, total_symbols), roi = rep(NA, total_symbols), row.names = symbols)
plt = list()
folder_name <- paste("Elman_plots_for_", pred_date)
dir.create(folder_name, showWarnings = FALSE)

for (i in 1:total_symbols) {
  
  symbol <- symbols[i]
  print(paste('Evaluating',symbol))
  sheet_name <- paste(symbol, "Details", sep="_")
  models_data <- read.xlsx(paste(pred_date,"Model_Performance_Elman.xlsx"), sheet = sheet_name)
  best_model <- models_data[which.min(models_data$MAPE_Test),]
  
  # Re-fetch the complete data for training
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
  for (j in 1:freq) { m[, j] = Lag(y, k = j) }
  x <- cbind(m, y)
  x <- x[-(1:freq),]
  
  # Defining training and testing sets
  n <- nrow(x)
  h <- 60
  n.train <- n - h
  outputs <- x[, freq + 1]
  inputs <- x[, 1:freq]
  
  # Retrain model
  set.seed(2023)
  fit.elman <- elman(x = inputs[1:n.train, ], y = outputs[1:n.train], size = c(best_model$l1, best_model$l2), maxit = 1000)
  # test data performance
  pred.elman = predict(fit.elman, inputs[-(1:n.train),])
  preds = exp(unscale.data(pred.elman, min.data, max.data))
  actual = exp(unscale.data(outputs[-(1:n.train)], min.data, max.data))
  elman.df = cbind.data.frame(n=1:length(preds),actual, preds)
  plt[[i]] = plot_ly(data = elman.df, x = ~n) %>%
    add_lines(y = ~actual, name = 'Actual Data', line = list(color = 'navy')) %>% add_markers(y = ~actual, name = 'Actual Data', color = I('navy'), showlegend = FALSE) %>%
    add_lines(y = ~preds, name = 'Predictions', line = list(color = 'red')) %>% add_markers(y = ~preds, name = 'Predictions', color = I('red'), showlegend = FALSE) %>%
    layout(title =paste('Elman for',symbol), xaxis = list(title = ""), yaxis = list(title = ""), showlegend = FALSE, plot_bgcolor = "rgba(0,0,0,0)",  paper_bgcolor = "rgba(0,0,0,0)" )
  
  saveWidget(plt[[i]], file = paste0(folder_name, "/", symbol, "_plot.html"))
  # Predict the next day
  latest_input <- matrix(tail(y, freq), nrow=1)
  pred <- predict(fit.elman, latest_input)
  pred_unscaled <- if(used_log) exp(unscale.data(pred, min(df), max(df))) else unscale.data(pred, min(df), max(df))
  
  forecasts[i,1] <- pred_unscaled
  forecasts[i,2] <- (pred_unscaled-as.numeric(tail(df, 1)))/pred_unscaled
  colnames(forecasts) = c('Forecasts','ROI') 
}

excel_filename <- paste("Elman_forecasts_", pred_date, ".xlsx", sep = "")
write.xlsx(forecasts, excel_filename)
cat("Forecasts have been saved to", excel_filename, "\n")
