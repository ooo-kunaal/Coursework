# portfolio optimisation
library(tidyr)
library(plotly)
library(timetk)
library(forcats)
library(quantmod)
library(openxlsx)
library(tidyquant)
library(htmlwidgets)
library(riskParityPortfolio)

options(warn=-1)

tickers = c("RELIANCE.NS", "HDFCBANK.NS", "ITC.NS", "INFY.NS", "IFBIND.NS", 
            "HINDUNILVR.NS", "TATAPOWER.NS", "BIOCON.NS", "BHARTIARTL.NS", "INDIGO.NS", 
            "ALKYLAMINE.NS", "HEROMOTOCO.NS", "HDFCLIFE.NS", "ADANIGREEN.NS","BOMDYEING.NS")

since = "2023-01-01"

# Create directory
folder_name = paste(pred_date, "Elman_weights")
dir.create(folder_name, showWarnings = FALSE)

cat('fetching closing prices',"\n")
portfolioprices = NULL
for(ticker in tickers){
  portfolioprices = cbind(portfolioprices, 
                          getSymbols.yahoo(ticker, from = since, to = as.Date(pred_date), periodicity = 'daily',auto.assign=F)[,4])
}

cat('fetching forecasts',"\n")
excel_filename <- paste("Elman_forecasts_", pred_date, ".xlsx", sep = "")
preds <- read.xlsx(excel_filename)
preds = xts(matrix(preds[,1], nrow = 1), as.Date(pred_date))

portfolioprices = rbind(log(portfolioprices), log(preds)) # combine historical + forecasted price
portfolioret = na.omit(ROC(portfolioprices))

# mean return
mean_ret <- colMeans(tail(portfolioret, 3))
# annual covariance matrix
cov_mat <- cov(portfolioret) * 252

## Monte Carlo Simulation
num_port <- 50000
all_wts <- matrix(nrow = num_port, ncol = length(tickers))
port_returns <- vector('numeric', length = num_port)
port_risk <- vector('numeric', length = num_port)
sharpe_ratio <- vector('numeric', length = num_port)

cat('Monte Carlo Simulation begins',"\n")
for (i in seq_along(port_returns)) {
  wts <- runif(length(tickers)) # random weights
  all_wts[i,] <- wts/sum(wts)
  portfolio_returns = portfolioret %*% wts
  port_ret <- sum(wts * mean_ret)                     # Portfolio returns
  port_returns[i] <- ((port_ret + 1)^252) - 1         # Annualized portfolio return
  port_risk[i] <- sqrt(t(wts) %*% (cov_mat  %*% wts)) # Portfolio risk
  sharpe_ratio[i] <- port_ret/port_risk[i]            # Sharpe Ratio (Assuming 0% Risk free rate)
}
cat('Monte Carlo Simulation complete',"\n")

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns, Risk = port_risk,  SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(portfolioret)
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

# (1) min variance portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
# (2) max Sharpe ratio portfolio
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
# (3) risk parity portfolio
risk.p.port = riskParityPortfolio(cov_mat)$w

df = cbind.data.frame(naive = rep(1/length(tickers)), min_var = t(min_var)[1:15], max_sr = t(max_sr)[1:15], risk.p.port = data.frame(risk.p.port))

name <- paste("Elman_Portfolio_Weights_", pred_date, ".xlsx", sep = "")
write.xlsx(cbind(tickers,df), name)
cat("Weights have been saved to", excel_filename, "\n")

plot_weights <- function(df, name = "naive") {
  weight_data <- data.frame(Ticker = tickers, Weight = df)
  plot_ly(weight_data, x = ~Ticker, y = ~Weight, type = 'bar', name = 'Weights', marker = list(color = 'rgba(50, 171, 96, 0.6)'), text = ~sprintf("%.2f%%", Weight * 100), hoverinfo = 'text+x+y') %>%
    layout(title = name, xaxis = list(title = "", tickangle = -90), yaxis = list(title = "Weight"))
}

plt = list()
plt[[1]] = plot_weights(df[,1], paste("Naive Portfolio", pred_date))
plt[[2]] = plot_weights(df[,2], paste("Min Variance Portfolio", pred_date))
plt[[3]] = plot_weights(df[,3], paste("Max Sharpe Ratio Portfolio", pred_date))
plt[[4]] = plot_weights(df[,4], paste("Risk Parity Portfolio", pred_date))

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) + geom_point() + theme_classic() +
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk', y = 'Annualized Returns', title = paste("Efficient Frontier",pred_date)) +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'green') +
  geom_point(aes(x = sqrt(t(risk.p.port) %*% (cov_mat  %*% risk.p.port)), y = (sum(mean_ret*risk.p.port)+1)^252-1), data = max_sr, color = 'black') 
plt[[5]] = ggplotly(p)

cat("Saving Plot 1", "\n")
saveWidget(plt[[1]], file = paste0(folder_name, "/", "naive_plot.html"))
cat("Saving Plot 2", "\n")
saveWidget(plt[[2]], file = paste0(folder_name, "/", "min variance_plot.html"))
cat("Saving Plot 3", "\n")
saveWidget(plt[[3]], file = paste0(folder_name, "/", "max sharpe_plot.html"))
cat("Saving Plot 4", "\n")
saveWidget(plt[[4]], file = paste0(folder_name, "/", "risk parity_plot.html"))
cat("Saving Plot 5", "\n")
saveWidget(plt[[5]], file = paste0(folder_name, "/", "efficient frontier_plot.html"))

cat("Plots have been saved to", folder_name, "\n")
