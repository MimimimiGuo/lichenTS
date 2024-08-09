library(lubridate)
library(forecast)
library(dplyr)
library(parallel)
library(readr)
library(ggplot2)
library(prophet)

df <- read_csv("data_23.csv")

# fill missing dates
complete_dates <- data.frame(date = seq.Date(from = min(df$date), to = max(df$date), by = "day"))
df_complete <- complete_dates %>%
  left_join(df, by = "date")
df_complete$n[is.na(df_complete$n)] <- 0
df_complete$pro <- 11

# ts object
df_ts <- ts(df_complete$n, start = c(year(min(df_complete$date)), yday(min(df_complete$date))), frequency = 365)

# exogenous variables
step1 <- as.numeric(df_complete$date >= as.Date("2020-01-01") & df_complete$date <= as.Date("2022-12-01"))
step2 <- as.numeric(df_complete$date > as.Date("2022-12-01")) * 2
step <- step1 + step2
ramp <- c(rep(0, sum(step == 0)),
          seq(from = 1, length.out = sum(step == 1), by = 1),
          seq(from = max(seq(1, sum(step == 1), 1) + 1), length.out = sum(step == 2), by = 2))
xreg <- cbind(step, ramp)


#-----------------ARIMA--------------------
model_arima <- auto.arima(df_ts, seasonal = TRUE,
                          xreg = xreg, max.p = 10, 
                          max.q = 10, max.P = 2,
                          max.Q = 2, max.order = 30,
                          allowdrift = TRUE, allowmean = FALSE,
                          approximation = FALSE, ic = "aic", 
                          max.d = 2, max.D = 1, stepwise = FALSE,
                          parallel = TRUE)


horizon <- 60  # Define your forecast horizon length

# future exogenous
future_step <- rep(2, horizon)
future_ramp <- seq(max(ramp) + 2, by = 2, length.out = horizon)
future_xreg <- cbind(future_step, future_ramp)

# Forecast future values
forecast_arima <- forecast(model_arima, xreg = future_xreg, h = horizon)

fitted_values <- fitted(model_arima)

#---------timegpt-------------
nixtla_set_api_key(api_key = "nixtla-tok-oEM8qvkiPLUOr4F0otMnBFuM4dv9fGIv6JzmWOX3Fo20QCbMia8wB3mglOu6LmeEVjtgng2eKlizg7JQ")

nixtla_client_fcst <- nixtla_client_forecast(df = df_complete, h = horizon, 
                                             id_col = "pro",
                                             time_col = "date",
                                             target_col = "n",
                                             level = 95)

nixtla_client_fitted_values <- nixtla_client_historic(df_complete, 
                                                      id_col = "pro", 
                                                      time_col = "date",
                                                      target_col = "n",
                                                      level = 95)

# Align and fill missing Nixtla fitted values
nixtla_fitted_complete <- df_complete  %>%
  left_join(nixtla_client_fitted_values %>% mutate(pro = as.numeric(pro)))

# Fill NAs if necessary - here just filling with NA, but consider interpolation if appropriate
nixtla_fitted_complete$n[is.na(nixtla_fitted_complete$n)] <- NA



# Prepare a dataframe for plotting
last_date <- as.Date(tail(df_complete$date, 1))
future_dates <- seq(from = last_date + 1, by = "day", length.out = horizon)




#-----------------Prophet--------------------

# df for Prophet
df_prophet <- df_complete %>%
  rename(ds = date, y = n)

# add some parameters for better performance, can test more
model_prophet <- prophet(
  changepoint.prior.scale = 0.5,
  seasonality_prior_scale = 0.5
)

model_prophet <- fit.prophet(model_prophet, df_prophet)

# future values
future_prophet <- make_future_dataframe(model_prophet, periods = horizon)
forecast_prophet <- predict(model_prophet, future_prophet)
historical_fitted_prophet <- predict(model_prophet, df_prophet)



# Prepare data for plotting
plot_data <- data.frame(
  Date = c(df_complete$date, future_dates),
  Value = c(as.numeric(df_ts), rep(NA, horizon)),
  Fitted_arima = c(fitted_values, as.numeric(forecast_arima$mean)),
  Fitted_nn = c(nixtla_fitted_complete$TimeGPT, as.numeric(nixtla_client_fcst$TimeGPT)),
  Fitted_prophet = as.numeric(forecast_prophet$yhat),
  
  Lower_arima = c(rep(NA, length(df_ts)), forecast_arima$lower[,2]),
  Upper_arima = c(rep(NA, length(df_ts)), forecast_arima$upper[,2]),
  Lower_nn = c(rep(NA, length(df_ts)), nixtla_client_fcst$`TimeGPT-lo-95`),
  Upper_nn = c(rep(NA, length(df_ts)), nixtla_client_fcst$`TimeGPT-hi-95`),
  Lower_prophet = c(rep(NA, length(df_ts)), forecast_prophet$yhat_lower[(length(df_ts) + 1):(length(df_ts) + horizon)]),
  Upper_prophet = c(rep(NA, length(df_ts)), forecast_prophet$yhat_upper[(length(df_ts) + 1):(length(df_ts) + horizon)]),
  Type = c(rep("Actual", length(df_ts)), rep("Forecast", horizon))
)

# Plot the results
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Value, colour = "Actual Values"), size = 1.2) +
  geom_line(aes(y = Fitted_arima, colour = "Fitted/Forecast ARIMA"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = Fitted_nn, colour = "Fitted/Forecast TimeGPT"), size = 1.2, linetype = "dotted") +
  geom_line(aes(y = Fitted_prophet, colour = "Fitted/Forecast Prophet"), size = 1.2, linetype = "twodash") +
  geom_ribbon(aes(ymin = Lower_arima, ymax = Upper_arima), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_prophet, ymax = Upper_prophet), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_nn, ymax = Upper_nn), fill = "blue", alpha = 0.2) +
  
  labs(title = "Actual vs Fitted/Forecasted Values", x = "Date", y = "Value") +
  scale_color_manual("", values = c("Actual Values" = "black", 
                                    "Fitted/Forecast ARIMA" = "red", 
                                    "Fitted/Forecast TimeGPT" = "green",
                                    "Fitted/Forecast Prophet" = "Blue")) +
  theme_minimal()


