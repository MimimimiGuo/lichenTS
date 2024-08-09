#auto tuning ARIMA
model_arima <- auto.arima(df_ts_train, xreg = xreg_train,
                          seasonal = TRUE, stepwise = FALSE,
                          max.p = 15, max.q = 15,
                          max.P = 2, max.Q = 2,
                          max.order = 30,
                          parallel = TRUE)

best_params_arima <- list(
  p = model_arima$arma[1],
  d = model_arima$arma[6],
  q = model_arima$arma[2],
  P = model_arima$arma[3],
  D = model_arima$arma[7],
  Q = model_arima$arma[4],
  seasonal_periods = model_arima$arma[5]
)
