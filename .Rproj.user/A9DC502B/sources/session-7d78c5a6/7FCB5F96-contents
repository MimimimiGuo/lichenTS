library(dplyr)
library(readr)
library(prophet)
library(forecast)
library(nixtlar)
library(lubridate)
library(MASS)  

# Load the data
df <- read_csv("data_23.csv")


#fill missing dates 
complete_dates <- data.frame(date = seq.Date(from = min(df$date), to = max(df$date), by = "day"))
df_complete <- complete_dates %>%
  left_join(df, by = "date") %>%
  mutate(n = ifelse(is.na(n), 0, n))  
df_complete$pro[is.na(df_complete$pro)] <- 11

# exogenous variables for COVID.. step and slope change. 
# step=0 - pre COVID, step=1 - COVID, step=2 - post COVID
# slope = 0  - pre COVID, slope increase by 1 - COVID, slope increase by 2 - post COVID
step1 <- as.numeric(df_complete$date >= as.Date("2020-01-01") & df_complete$date <= as.Date("2022-12-01"))
step2 <- as.numeric(df_complete$date > as.Date("2022-12-01")) * 2
step <- step1 + step2
ramp <- c(rep(0, sum(step == 0)),
          seq(from = 1, length.out = sum(step == 1), by = 1),
          seq(from = max(seq(from = 1, length.out = sum(step == 1), by = 1)) + 1, length.out = sum(step == 2), by = 2))
xreg <- cbind(step, ramp)

df_complete <- df_complete %>%
  mutate(step = step, ramp = ramp)


# Split the dataset into training and validation sets
validation_start_date <- as.Date("2023-01-01")
df_train <- df_complete %>% filter(date < validation_start_date)
df_validation <- df_complete %>% filter(date >= validation_start_date)

# Extract the training and validation xregs
xreg_train <- xreg[df_complete$date < validation_start_date, ]
xreg_validation <- xreg[df_complete$date >= validation_start_date, ]

# ARIMA 
df_ts_train <- ts(df_train$n, start = c(year(min(df_train$date)), yday(min(df_train$date))), frequency = 365)
source(arima_tuning.R)
model_arima <- Arima(df_ts_train,
                     order = c(best_params_arima$p, best_params_arima$d, best_params_arima$q),
                     seasonal = list(order = c(best_params_arima$P, best_params_arima$D, best_params_arima$Q),
                                     period = best_params_arima$seasonal_periods),
                     xreg = xreg_train)
forecast_arima <- forecast(model_arima, xreg = xreg_validation, h = nrow(df_validation))

# Prophet

#tuning
source(prophet_tuning.R)

cap_value <- max(df_train$n) * 1.2  # capacity slightly higher than the maximum observed value

df_prophet_train <- df_train %>% rename(ds = date, y = n) %>%
  mutate(step = xreg_train[, 1], ramp = xreg_train[, 2],  cap = cap_value)
model_prophet <- prophet(
  changepoint.prior.scale = best_params_prophet$changepoint_prior_scale,
  seasonality.prior.scale = best_params_prophet$seasonality_prior_scale,
  seasonality.mode = best_params_prophet$seasonality_mode,
  growth = as.character(best_params_prophet$growth),

  yearly.seasonality = best_params_prophet$yearly_seasonality,
  weekly.seasonality = best_params_prophet$weekly_seasonality,
  daily.seasonality = best_params_prophet$daily_seasonality
)
model_prophet <- add_regressor(model_prophet, 'step')
model_prophet <- add_regressor(model_prophet, 'ramp')

model_prophet <- fit.prophet(model_prophet, df_prophet_train)
future_prophet <- make_future_dataframe(model_prophet, periods = nrow(df_validation), include_history = FALSE)
df_prophet_validation <- df_validation %>% rename(ds = date, y = n) %>%
  mutate(step = xreg_validation[, 1], ramp = xreg_validation[, 2], cap = cap_value)
forecast_prophet <- predict(model_prophet, bind_cols(future_prophet, df_prophet_validation[, c("step", "ramp", "cap")]))


# Negative binomial
df_nb_train <- df_train %>% mutate(step = xreg_train[, 1], ramp = xreg_train[, 2])
model_nb <- glm.nb(n ~ step + ramp, data = df_nb_train)
forecast_nb <- predict(model_nb, newdata = df_validation %>% mutate(step = xreg_validation[, 1], ramp = xreg_validation[, 2]), type = "response")





# TimeGPT model
df_timegpt_train <- df_train %>% 
  mutate(step = xreg_train[, 1], ramp = xreg_train[, 2])
df_timegpt_validation <- df_validation %>% 
  mutate(step = xreg_validation[, 1], ramp = xreg_validation[, 2])

nixtla_set_api_key(api_key = "nixtla-tok-LVjq2UsYs9uFi5TkLXK57n9rvXiSBsPT9jR4v18SonfNQxw3S4kZL0giPBShayz8BQ9vqDOAJZhJqHtd")

nixtla_client_fcst_val <- nixtla_client_forecast(df = df_timegpt_train, 
                                                 h = nrow(df_timegpt_validation), 
                                                 id_col = "pro", time_col = "date", 
                                                 target_col = "n", level = 95)

#MASE

naive_errors <- abs(diff(df_train$n))
mean_naive_error <- mean(naive_errors)

calculate_mase <- function(forecasted_values, actual_values, training_mean_naive_error) {
  mae <- mean(abs(forecasted_values - actual_values))
  mase <- mae / training_mean_naive_error
  return(mase)
}

mase_arima <- calculate_mase(forecast_arima$mean, df_validation$n, mean_naive_error)

mase_prophet <- calculate_mase(forecast_prophet$yhat, df_validation$n, mean_naive_error)

mase_timegpt <- calculate_mase(nixtla_client_fcst_val$TimeGPT, df_validation$n, mean_naive_error)

mase_nb <- calculate_mase(forecast_nb, df_validation$n, mean_naive_error)

mase_results <- data.frame(model = c("ARIMA", "Prophet", "Negative Binomial", "TimeGPT"),
                           MASE = c(mase_arima, mase_prophet,mase_nb,  mase_timegpt))

write_csv(mase_results, "mase_validation_results.csv")
