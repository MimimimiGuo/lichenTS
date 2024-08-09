library(lubridate)
library(forecast)
library(dplyr)
library(parallel)
library(readr)
library(ggplot2)

df <- read_csv("data_23.csv")
complete_dates <- data.frame(date = seq.Date(from = min(df$date), to = max(df$date), by = "day"))

df_complete <- complete_dates %>%
  left_join(df, by = "date")

df_complete$n[is.na(df_complete$n)] <- 0

df_ts <- ts(df_complete$n, start = c(year(min(df_complete$date)), yday(min(df_complete$date))), frequency = 365)


step1 <- as.numeric(df_complete$date >= as.Date("2020-01-01") & df_complete$date <= as.Date("2022-12-01"))
step2<- as.numeric(df_complete$date > as.Date("2022-12-01"))*2
step <- step1 + step2
ramp <- c(rep(0, sum(step == 0)),
          seq(from = 1, length.out = sum(step == 1), by = 1),
          seq(from = max(seq(1, sum(step == 1), 1)+1), length.out = sum(step == 2), by = 2))
xreg <- cbind(step, ramp)

# Define the rolling forecast cross-validation function
rolling_cv <- function(i, ts_data, xreg_data, initial_window, horizon) {
  train_end <- initial_window + i - 1
  test_end <- train_end + horizon - 1
  
  train_ts <- window(ts_data, end = c(start(ts_data)[1] + (train_end - 1) %/% 365, (train_end - 1) %% 365 + 1))
  test_ts <- window(ts_data, start = c(start(ts_data)[1] + train_end %/% 365, train_end %% 365 + 1),
                    end = c(start(ts_data)[1] + (test_end - 1) %/% 365, (test_end - 1) %% 365 + 1))
  
  train_xreg <- xreg_data[1:train_end, ]
  test_xreg <- xreg_data[(train_end + 1):test_end, ]
  
  model <- auto.arima(train_ts,
                      seasonal = TRUE,
                      xreg = train_xreg,
                      max.p = 15, max.q = 15,
                      max.P = 2, max.Q = 2,
                      max.order = 20,
                      allowdrift = TRUE,
                      allowmean = FALSE,
                      approximation = TRUE,
                      ic = "aic",
                      max.d = 2, max.D = 1,
                      stepwise = TRUE, trace = FALSE)
  
  forecast <- forecast(model, xreg = test_xreg, h = horizon)
  error <- mean((forecast$mean - test_ts)^2, na.rm = TRUE)  # Calculate Mean Squared Error
  
  return(error)
}

# Perform rolling forecast origin cross-validation
initial_window <- length(window(df_ts, end = c(2023, 30)))
horizon <- 30  # Define your horizon length

# Initialize cluster
cl1 <- makeCluster(detectCores() - 1)

# Export necessary variables and functions to cluster
clusterExport(cl = cl1, varlist = c("rolling_cv", "df_ts", "xreg", "initial_window", "horizon"), envir = environment())
clusterEvalQ(cl = cl1, {
  library(lubridate)
  library(forecast)
  library(dplyr)
})

# Perform parallel rolling forecast cross-validation
n <- length(df_ts)
start_indices <- seq_len(n - initial_window - horizon + 1)
cv_errors <- parLapply(cl = cl1, start_indices, function(i) rolling_cv(i, df_ts, xreg, initial_window, horizon))

# Calculate mean cross-validation error
cv_error <- mean(unlist(cv_errors))

# Stop the cluster
stopCluster(cl1)

# Print cross-validation error
print(cv_error)


# Define forecast horizon
horizon <- 30  # Define your forecast horizon length

# Generate future exogenous variables
future_step <- rep(max(step) + 1, horizon)
future_ramp <- seq(max(ramp) + 1, by = 1, length.out = horizon)
future_xreg <- cbind(future_step, future_ramp)

# Forecast future values
forecast_arima <- forecast(model_arima, xreg = future_xreg, h = horizon)

# Get actual and predicted values for comparison
actual_values <- df_ts[(length(df_ts) - horizon + 1):length(df_ts)]
predicted_values <- forecast_arima$mean

# Correct date conversion
start_date <- as.Date(paste(start(df_ts)[1], start(df_ts)[2], sep = "-"), format = "%Y-%j")
end_date <- start_date + length(df_ts) - 1
dates <- seq.Date(from = end_date - horizon + 1, by = "day", length.out = horizon)

# Create a comparison data frame
comparison <- data.frame(
  Date = dates,
  Actual = as.numeric(actual_values),
  Predicted = as.numeric(predicted_values)
)

# Plot actual vs predicted values
ggplot(comparison, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values", x = "Date", y = "Value", color = "Legend") +
  theme_minimal()
