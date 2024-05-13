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

total_symbols <- length(symbols)

forecasts <- data.frame(forecast = rep(NA, total_symbols), roi = rep(NA, total_symbols), row.names = symbols)

for (idx in 1:total_symbols) {
  symbol <- symbols[idx]
  cat(sprintf("Processing %d/%d: %s\n", idx, total_symbols, symbol))
  
  df <- getSymbols(symbol, src = 'yahoo', auto.assign = F, from = '2023-01-01', to = as.Date(pred_date)+1)
  df <- ts(data = as.numeric(df[, 4]), frequency = 365)
  
  # Process data
  used_log <- any(df < 0)
  y <- if(used_log) df else log(df)

  fit = auto.arima(y[1:(length(df)-1)])
  
  pred = as.numeric(forecast(fit, h = 1)$mean)
  pred <- if(used_log) pred else exp(pred)
  actual = as.numeric(tail(df, 1))
  
  pred.df = cbind.data.frame(n=1,actual, pred)
  
  forecasts[idx,1] <- pred[1] # next value
  forecasts[idx,2] <- as.numeric((pred[1]-tail(df, 1))/pred[1])
  colnames(forecasts) = c(paste('Forecasts for',as.character(Sys.Date()+1)),'ROI') 
}
# Output the forecasts
print(forecasts)
excel_filename <- paste("SARIMA_forecasts_", pred_date, ".xlsx", sep = "")
write.xlsx(forecasts, excel_filename)
cat("Forecasts have been saved to", excel_filename, "\n") 
