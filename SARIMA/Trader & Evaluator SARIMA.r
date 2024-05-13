library(plotly)
library(quantmod)
library(openxlsx)
library(htmlwidgets)

symbols <- c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS",
             "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS", "INDIGO.NS",
             "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS", "ADANIGREEN.NS","BOMDYEING.NS")

pred_dates = c("2024-04-26","2024-04-29","2024-04-30","2024-05-01", "2024-05-02",
               "2024-05-03","2024-05-06","2024-05-07", "2024-05-08","2024-05-09")

op.price =  cl.price =  high.price = low.price = NULL

for(ticker in symbols){
  data = getSymbols.yahoo(ticker, from = "2024-04-25", to = "2024-05-11", periodicity = 'daily',auto.assign=F)
  op.price   = cbind(op.price, data[,1])
  high.price = cbind(high.price, data[,2])
  low.price  = cbind(low.price, data[,3])
  cl.price   = cbind(cl.price, data[,4])
}

n = length(symbols)
df = data.frame(symbols, high = rep(1.02, n), low = rep(0.98, n))

# initialise
holding_naive = holding_minvar = holding_maxsr = holding_riskpp = list() 
holding_naive[[1]] = holding_minvar[[1]] = holding_maxsr[[1]] = holding_riskpp[[1]] = rep(10, 15)

wt_naive = wt_minvar = wt_maxsr = wt_riskpp = list() 
wt_naive[[1]] = wt_minvar[[1]] = wt_maxsr[[1]] = wt_riskpp[[1]] = rep(1/length(symbols),15)

port_value_naive = port_value_minvar = port_value_maxsr = port_value_riskpp = numeric() 
port_value_naive[1] = port_value_minvar[1] = port_value_maxsr[1] = port_value_riskpp[1] = sum(holding_naive[[1]]*t(cl.price[1,]))

profit_naive = profit_minvar = profit_maxsr = profit_riskpp = numeric() 
profit_naive[1] = profit_minvar[1] = profit_maxsr[1] = profit_riskpp[1] = 0

trans_naive = trans_minvar = trans_maxsr = trans_riskpp = numeric() 
trans_naive[1] = trans_minvar[1] = trans_maxsr[1] = trans_riskpp[1] = 0

change = transaction = NULL

for(i in 1:9){
  print(paste("Evaluating for",pred_dates[i]))
  # weights
  wt_naive[[i+1]] =  read.xlsx(paste0("SARIMA_Portfolio_Weights_",pred_dates[i],".xlsx"))[,2] # naive
  wt_minvar[[i+1]] = read.xlsx(paste0("SARIMA_Portfolio_Weights_",pred_dates[i],".xlsx"))[,3] # min variance
  wt_maxsr[[i+1]]  = read.xlsx(paste0("SARIMA_Portfolio_Weights_",pred_dates[i],".xlsx"))[,4] # max Sharpe ratio
  wt_riskpp[[i+1]] = read.xlsx(paste0("SARIMA_Portfolio_Weights_",pred_dates[i],".xlsx"))[,5] # risk parity
  # stop loss validation
  buy.price = df$low*t(op.price)[,i+1]
  sell.price = df$high*t(op.price)[,i+1]
  # buy-sell decision-making
  status = ifelse(wt_minvar[[2]] > wt_minvar[[1]], 'buy','sell')
  buy.status = ifelse(t(low.price)[,i+1]<= buy.price & status == "buy",as.numeric(buy.price),0)
  sell.status = ifelse(t(high.price)[,i+1]>= sell.price & status == "sell",as.numeric(sell.price),0)
  # new holding on the basis of opening price
  holding_naive[[i+1]] = holding_naive[[i]] + pmax(sum(holding_naive[[i]])*(wt_naive[[i+1]]-wt_naive[[i]]), -holding_naive[[i]])
  holding_minvar[[i+1]] = holding_minvar[[i]] + pmax(sum(holding_minvar[[i]])*(wt_minvar[[i+1]]-wt_minvar[[i]]), -holding_minvar[[i]])
  holding_maxsr[[i+1]] = holding_maxsr[[i]] + pmax(sum(holding_maxsr[[i]])*(wt_maxsr[[i+1]]-wt_maxsr[[i]]), -holding_maxsr[[i]])
  holding_riskpp[[i+1]] = holding_riskpp[[i]] + pmax(sum(holding_riskpp[[i]])*(wt_riskpp[[i+1]]-wt_riskpp[[i]]), -holding_riskpp[[i]])
  # transaction amounts
  trans_naive[i+1]  = sum( ifelse(status=='buy', pmax(sum(holding_naive[[i]])*(wt_naive[[i+1]]-wt_naive[[i]]), -holding_naive[[i]])*buy.status, pmax(sum(holding_naive[[i]])*(wt_naive[[i+1]]-wt_naive[[i]]), -holding_naive[[i]])*sell.status) )
  trans_minvar[i+1] = sum( ifelse(status=='buy', pmax(sum(holding_minvar[[i]])*(wt_minvar[[i+1]]-wt_minvar[[i]]), -holding_minvar[[i]])*buy.status, pmax(sum(holding_minvar[[i]])*(wt_minvar[[i+1]]-wt_minvar[[i]]), -holding_minvar[[i]])*sell.status) )
  trans_maxsr[i+1]  = sum( ifelse(status=='buy', pmax(sum(holding_maxsr[[i]])*(wt_maxsr[[i+1]]-wt_maxsr[[i]]), -holding_maxsr[[i]])*buy.status, pmax(sum(holding_maxsr[[i]])*(wt_maxsr[[i+1]]-wt_maxsr[[i]]), -holding_maxsr[[i]])*sell.status) )
  trans_riskpp[i+1] = sum( ifelse(status=='buy', pmax(sum(holding_riskpp[[i]])*(wt_riskpp[[i+1]]-wt_riskpp[[i]]), -holding_riskpp[[i]])*buy.status, pmax(sum(holding_riskpp[[i]])*(wt_riskpp[[i+1]]-wt_riskpp[[i]]), -holding_riskpp[[i]])*sell.status) )
  # portfolio value at market closing
  port_value_naive[i+1] = sum(holding_naive[[i+1]]*t(cl.price[i+1,]))
  port_value_minvar[i+1] = sum(holding_minvar[[i+1]]*t(cl.price[i+1,]))
  port_value_maxsr[i+1] = sum(holding_maxsr[[i+1]]*t(cl.price[i+1,]))
  port_value_riskpp[i+1] = sum(holding_riskpp[[i+1]]*t(cl.price[i+1,]))
  # profit 
  profit_naive[i+1]  = port_value_naive[i+1] - trans_naive[[i+1]] - port_value_naive[[i]]
  profit_minvar[i+1] = port_value_minvar[i+1] - trans_minvar[[i+1]] - port_value_minvar[[i]]
  profit_maxsr[i+1]  = port_value_maxsr[i+1] - trans_maxsr[[i+1]] - port_value_maxsr[[i]]
  profit_riskpp[i+1] = port_value_riskpp[i+1] - trans_riskpp[[i+1]] - port_value_riskpp[[i]]
}

weight_plot = function(df, title){
  df_holding = data.frame(t(matrix(unlist(df), nrow=15)))
  rownames(df_holding) = pred_dates
  colnames(df_holding) = symbols
  options(warn = -1)
  data_long <- reshape2::melt(df_holding, variable.name = "Stock", value.name = "Holdings")
  data_long$Date <- factor(pred_dates, levels = pred_dates)
  plot_ly(data_long, x = ~Date, y = ~Holdings, type = 'bar', color = ~Stock, colors = RColorBrewer::brewer.pal(9, "Set1"), name = ~Stock) %>%
    layout(title=title, yaxis = list(title = 'Holdings'), barmode = 'stack')
}

wplt = list()
wplt[[1]] = weight_plot(holding_naive, 'Naive Portfolio Holdings')
wplt[[2]] = weight_plot(holding_minvar,'Min Variance Portfolio Holdings')
wplt[[3]] = weight_plot(holding_maxsr, 'Max Sharpe Ratio Portfolio Holdings')
wplt[[4]] = weight_plot(holding_riskpp,'Risk Parity Portfolio Holdings')

wplt[[5]] = plot_ly(x = pred_dates, y = port_value_minvar, type = 'scatter', mode = 'lines+markers', name = 'Minimum Variance') %>% 
  add_trace(x = pred_dates, y = port_value_naive, name = 'Naive') %>% 
  add_trace(x = pred_dates, y = port_value_maxsr, name = 'Maximum Sharpe Ratio') %>% 
  add_trace(x = pred_dates, y = port_value_riskpp, name = 'Risk Parity') %>% 
  layout(title='SARIMA - Total Portfolio Value')

wplt[[6]] = plot_ly(x = pred_dates, y = profit_minvar, type = 'scatter', mode = 'lines+markers', name = 'Minimum Variance') %>% 
  add_trace(x = pred_dates, y = profit_naive, name = 'Naive') %>% 
  add_trace(x = pred_dates, y = profit_maxsr, name = 'Maximum Sharpe Ratio') %>% 
  add_trace(x = pred_dates, y = profit_riskpp, name = 'Risk Parity') %>% 
  layout(title='SARIMA - Total Growth')

cat("Saving plot 1","\n")
saveWidget(wplt[[1]], file = "Naive Portfolio Holdings.html")
cat("Saving plot 2","\n")
saveWidget(wplt[[2]], file = "Min Variance Portfolio Holdings.html")
cat("Saving plot 3","\n")
saveWidget(wplt[[3]], file = "Max Sharpe Ratio Portfolio Holdings.html")
cat("Saving plot 4","\n")
saveWidget(wplt[[4]], file = "Risk Parity Portfolio Holdings.html")
cat("Saving plot 5","\n")
saveWidget(wplt[[5]], file = "SARIMA - Total Portfolio Value.html")
cat("Saving plot 6","\n")
saveWidget(wplt[[6]], file = "SARIMA - Total Profit.html")

# transaction plots
transaction.df = data.frame(pred_dates, trans_naive, trans_minvar, trans_maxsr, trans_riskpp)
wplt[[7]] = plot_ly(transaction.df, x =~ pred_dates, y =~ trans_minvar, type='bar',name='Min Variance') %>% 
  add_trace(x =~ pred_dates, y =~ trans_maxsr,name='Max Sharpe') %>% 
  add_trace(x =~ pred_dates, y =~ trans_riskpp,name='Risk PP') %>% 
  add_trace(x =~ pred_dates, y =~ trans_naive,name='Naive') %>% 
  layout(title='Transactions under SARIMA forecaster',yaxis=list(title='Cashflow'))
cat("Saving plot 7","\n")
saveWidget(wplt[[7]], file = "Transactions under SARIMA forecaster.html")
cat("Plots have been saved","\n")
#### evaluation
cat("Evaluation begins","\n")

risk_free_rate <- 0.07/252 
portfolio_returns_naive <- diff(log(port_value_naive))
portfolio_returns_minvar <- diff(log(port_value_minvar))
portfolio_returns_maxsr <- diff(log(port_value_maxsr))
portfolio_returns_riskpp <- diff(log(port_value_riskpp))

# Calculate Sharpe Ratios
sharpe_ratio_naive <- mean(portfolio_returns_naive - risk_free_rate) / sd(portfolio_returns_naive)
sharpe_ratio_minvar <- mean(portfolio_returns_minvar - risk_free_rate) / sd(portfolio_returns_minvar)
sharpe_ratio_maxsr <- mean(portfolio_returns_maxsr - risk_free_rate) / sd(portfolio_returns_maxsr)
sharpe_ratio_riskpp <- mean(portfolio_returns_riskpp - risk_free_rate) / sd(portfolio_returns_riskpp)

# Define downside deviation function
downside_deviation <- function(returns, target = risk_free_rate) {
  sqrt(mean(pmin(0, returns - target)^2))
}
## Calculate Sortino Ratios
sortino_ratio_naive <- mean(portfolio_returns_naive - risk_free_rate) / downside_deviation(portfolio_returns_naive)
sortino_ratio_minvar <- mean(portfolio_returns_minvar - risk_free_rate) / downside_deviation(portfolio_returns_minvar)
sortino_ratio_maxsr <- mean(portfolio_returns_maxsr - risk_free_rate) / downside_deviation(portfolio_returns_maxsr)
sortino_ratio_riskpp <- mean(portfolio_returns_riskpp - risk_free_rate) / downside_deviation(portfolio_returns_riskpp)

names = c('naive','minvar','maxsr','riskpp')
eval.df = data.frame(names, 
                     sharpe_ratio = c(sharpe_ratio_naive, sharpe_ratio_minvar,sharpe_ratio_maxsr,sharpe_ratio_riskpp),
                     sortino_ratio = c(sortino_ratio_naive, sortino_ratio_minvar, sortino_ratio_maxsr, sortino_ratio_riskpp) )
cat("Saving Financial Ratios","\n")
write.csv(eval.df, file = "Eval ratios.csv")

# forecast evaluation
forecast = matrix(NA, nrow = length(symbols), ncol = length(pred_dates))
for(i in 1:10){
  name = paste0("SARIMA_forecasts_",pred_dates[i],".xlsx")
  forecast[,i] = read.xlsx(name)[,1]
}
colnames(forecast) = pred_dates
rownames(forecast) = symbols

model_mape = numeric()
model_rmse = numeric()
library(Metrics)
for(i in 1:15){
  model_mape[i] = round(100*mape(actual = as.numeric(cl.price[,i]), predicted = as.numeric(forecast[i,])),2)
  model_rmse[i] = round(rmse(actual = as.numeric(cl.price[,i]), predicted = as.numeric(forecast[i,])),2)
}
performance = cbind(symbols,model_mape, model_rmse)
cat("Saving forecast analysis","\n")
write.csv(performance, file = 'SARIMA Performance.csv')

png("Forecasting Performance.png", width = 4000, height = 2500, res = 300)
par(mfrow=c(3,5))
for(i in 1:15){
  plot(as.numeric(cl.price[,i]), type='b',main = symbols[i], ylab='Closing Price',xlab = 'Trading Days',col='navy',lwd=2,
       ylim=c(min(as.numeric(cl.price[,i]), forecast[i,]),max(forecast[i,], as.numeric(cl.price[,i]))))	
  lines(forecast[i,], col='red',lwd=2)
}
legend("right", c('Actual', 'Forecast'), col=c('navy', 'red'), lwd=2,bty = 'n')
par(mfrow=c(1,1))
dev.off()