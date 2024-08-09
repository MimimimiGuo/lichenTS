param_grid <- expand.grid(
  finetune_steps = c(5, 10, 50, 100, 200)
)

# Function to train and evaluate a TimeGPT model
evaluate_model <- function(finetune_steps, df_train, df_validation) {
  nixtla_client_fcst_val <- nixtla_client_forecast(
    df = df_train, 
    h = nrow(df_validation), 
    id_col = "pro", 
    time_col = "date", 
    target_col = "n", 
    level = 95,
    finetune_steps = finetune_steps
  )
  
  # Extract predictions
  forecast <- nixtla_client_fcst_val$TimeGPT
  
  # Calculate error metric
  actuals <- df_validation$n
  preds <- forecast
  error <- mean(abs(actuals - preds))
  
  return(error)
}

# Evaluate all hyperparameter combinations
results <- param_grid %>%
  rowwise() %>%
  mutate(error = evaluate_model(finetune_steps, df_timegpt_train, df_timegpt_validation))

# Select the best hyperparameters
best_params_nn <- results %>% filter(error == min(error)) %>% pull(finetune_steps)
best_error_nn <- results %>% filter(error == min(error)) %>% pull(error)

