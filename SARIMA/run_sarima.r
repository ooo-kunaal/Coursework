pred_dates = c("2024-04-26","2024-04-29","2024-04-30","2024-05-01",
               "2024-05-02","2024-05-03","2024-05-06","2024-05-07",
               "2024-05-08","2024-05-09")

for (pred_date in pred_dates){
  pred_date = pred_date
  print(paste('running for', pred_date))
  source('Forecaster SARIMA.r')
  source('Portfolio Optimization SARIMA.R')
}
source('Trader & Evaluator SARIMA.r')