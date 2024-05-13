library(foreach)
library(Metrics)
library(quantmod)
library(forecast)
library(openxlsx)
library(iterators)
library(doParallel)
library(htmlwidgets)

symbols <- c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS", 
             "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS", "INDIGO.NS", 
             "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS", "ADANIGREEN.NS","BOMDYEING.NS")

# pred_date = "2024-04-30"

total_symbols <- length(symbols)

forecasts <- data.frame(forecast = rep(NA, total_symbols), 
                        roi = rep(NA, total_symbols), 
                        row.names = symbols)

for (idx in 1:total_symbols) {
  
  symbol <- symbols[idx]
  
  cat(sprintf("Processing %d/%d: %s\n", idx, total_symbols, symbol))
  
  df <- getSymbols(symbol, src = 'yahoo', auto.assign = F, from = '2023-01-01', to = as.Date(pred_date)+1)
  df <- ts(data = as.numeric(df[, 4]), frequency = 365)
  
  fit = naive(df[1:(length(df)-1)], h = 1)

  pred = fit$mean
  
  pred.df = cbind.data.frame(n =1 ,actual = as.numeric(tail(df, 1)), pred)
  mape = 100 * round(mape(actual = pred.df$actual, predicted = pred.df$pred), 4)
  rmse = round(rmse(actual = pred.df$actual, predicted = pred.df$pred), 4)
  
  forecasts[idx,1] <- pred
  forecasts[idx,2] <- as.numeric((pred[1]-tail(df, 1))/pred[1])
  colnames(forecasts) = c('Forecasts','ROI') 
}

print(forecasts)
excel_filename <- paste("Snaive_forecasts_", pred_date, ".xlsx", sep = "")
write.xlsx(forecasts, excel_filename)
cat("Forecasts have been saved to", excel_filename, "\n") 
