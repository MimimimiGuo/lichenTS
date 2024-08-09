library(prophet)
library(dplyr)
library(parallel)

# Define the parameter grid
param_grid <- expand.grid(
  changepoint_prior_scale = c(0.001, 0.01, 0.1, 0.5),
  seasonality_prior_scale = c(0.01, 0.1, 1.0, 10.0),
  seasonality_mode = c('multiplicative', 'additive'),
  growth = c('linear', 'logistic'),
  yearly_seasonality = c(5, 10, 20, 40),
  weekly_seasonality = c(5, 10, 20, 40),
  daily_seasonality = c(5, 10, 20, 40, 80)
)

# Function to train and evaluate a Prophet model
evaluate_model <- function(params, df_train, df_validation) {
  if (params$growth == 'logistic') {
    cap_value <- max(df_train$y) * 1.2
    df_train <- df_train %>% mutate(cap = cap_value)
    df_validation <- df_validation %>% mutate(cap = cap_value)
  }
  
  model <- prophet(
    changepoint.prior.scale = params$changepoint_prior_scale,
    seasonality.prior.scale = params$seasonality_prior_scale,
    seasonality.mode = params$seasonality_mode,
    growth = params$growth,
    yearly.seasonality = params$yearly_seasonality,
    weekly.seasonality = params$weekly_seasonality,
    daily.seasonality = params$daily_seasonality
  )
  model <- add_regressor(model, 'step')
  model <- add_regressor(model, 'ramp')
  model <- fit.prophet(model, df_train)
  
  future <- make_future_dataframe(model, periods = nrow(df_validation), include_history = FALSE)
  future <- bind_cols(future, df_validation[, c("step", "ramp")])
  if (params$growth == 'logistic') {
    future <- future %>% mutate(cap = cap_value)
  }
  
  forecast <- predict(model, future)
  
  # Calculate error metric
  actuals <- df_validation$y
  preds <- forecast$yhat
  error <- mean(abs(actuals - preds))
  
  return(list(params = params, error = error))
}

# Set up parallel processing
num_cores <- detectCores() - 1  # Use one less than the available cores
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, varlist = c("evaluate_model", "df_prophet_train", "df_prophet_validation", "param_grid"))
clusterEvalQ(cl, {library(dplyr)
  library(prophet)})

# Evaluate all hyperparameter combinations in parallel
results <- parLapply(cl, 1:nrow(param_grid), function(i) {
  params <- param_grid[i, ]
  evaluate_model(params, df_prophet_train, df_prophet_validation)
})

# Stop the cluster
stopCluster(cl)

# Find the best parameters based on the minimum error
best_result <- Reduce(function(x, y) if(x$error < y$error) x else y, results)
best_params_prophet <- best_result$params
best_error_prophet <- best_result$error

# Print the best parameters and error
print(best_params_prophet)
print(best_error_prophet)
