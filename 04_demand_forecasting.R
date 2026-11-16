
# ============================================================
# StockSense AI
# Step 4: Demand Forecasting
# ============================================================

# 1. Load cleaned sales data
sales <- read.csv(
  "data/cleaned_sales.csv",
  stringsAsFactors = FALSE
)

sales$date <- as.Date(sales$date)

# 2. Sort sales data
sales <- sales[
  order(sales$product_id, sales$date),
]

# 3. Define forecasting settings
forecast_days <- 7
test_days <- 14

# Create empty lists to store results
evaluation_results <- list()
forecast_results <- list()

# 4. Get unique products
product_ids <- unique(sales$product_id)

# 5. Forecast demand for each product
for (product in product_ids) {
  
  # Select one product
  product_data <- sales[
    sales$product_id == product,
  ]
  
  product_data <- product_data[
    order(product_data$date),
  ]
  
  # Aggregate demand by date in case of repeated dates
  daily_data <- aggregate(
    demand ~ date,
    data = product_data,
    FUN = sum
  )
  
  daily_data <- daily_data[
    order(daily_data$date),
  ]
  
  # Ensure dates are continuous
  all_dates <- data.frame(
    date = seq(
      min(daily_data$date),
      max(daily_data$date),
      by = "day"
    )
  )
  
  daily_data <- merge(
    all_dates,
    daily_data,
    by = "date",
    all.x = TRUE
  )
  
  daily_data$demand[is.na(daily_data$demand)] <- 0
  
  # Check that enough data is available
  if (nrow(daily_data) <= test_days + 7) {
    next
  }
  
  # 6. Split data into training and testing
  n <- nrow(daily_data)
  
  train_end <- n - test_days
  
  train_data <- daily_data$demand[
    1:train_end
  ]
  
  actual <- daily_data$demand[
    (train_end + 1):n
  ]
  
  # 7. Model 1: Mean demand baseline
  baseline_prediction <- rep(
    mean(train_data),
    test_days
  )
  
  # 8. Model 2: Seasonal naive forecasting
  # Predict using demand from the same weekday
  # in the previous week
  
  seasonal_prediction <- daily_data$demand[
    (train_end + 1 - 7):(n - 7)
  ]
  
  # 9. Calculate model evaluation metrics
  baseline_mae <- mean(
    abs(actual - baseline_prediction)
  )
  
  baseline_rmse <- sqrt(
    mean((actual - baseline_prediction)^2)
  )
  
  seasonal_mae <- mean(
    abs(actual - seasonal_prediction)
  )
  
  seasonal_rmse <- sqrt(
    mean((actual - seasonal_prediction)^2)
  )
  
  # Store evaluation results
  evaluation_results[[product]] <- data.frame(
    product_id = product,
    model = c(
      "Mean Baseline",
      "Seasonal Naive"
    ),
    MAE = c(
      baseline_mae,
      seasonal_mae
    ),
    RMSE = c(
      baseline_rmse,
      seasonal_rmse
    )
  )
  
  # 10. Select the model with the lower MAE
  if (seasonal_mae <= baseline_mae) {
    best_model <- "Seasonal Naive"
    best_prediction <- seasonal_prediction
  } else {
    best_model <- "Mean Baseline"
    best_prediction <- baseline_prediction
  }
  
  # 11. Forecast the next 7 days
  future_dates <- seq(
    from = max(daily_data$date) + 1,
    by = "day",
    length.out = forecast_days
  )
  
  # Generate forecasts from the selected model
  if (best_model == "Seasonal Naive") {
    
    future_prediction <- tail(
      daily_data$demand,
      7
    )
    
  } else {
    
    future_prediction <- rep(
      mean(train_data),
      forecast_days
    )
  }
  
  # Demand cannot be negative
  future_prediction <- pmax(
    0,
    future_prediction
  )
  
  # Round predicted demand to whole units
  future_prediction <- round(
    future_prediction
  )
  
  # Store forecast results
  forecast_results[[product]] <- data.frame(
    product_id = product,
    date = future_dates,
    predicted_demand = future_prediction,
    selected_model = best_model
  )
}

# 12. Combine results for all products
model_evaluation <- do.call(
  rbind,
  evaluation_results
)

demand_forecast <- do.call(
  rbind,
  forecast_results
)

# 13. Display model evaluation
cat("\n--- FORECAST MODEL EVALUATION ---\n")
print(model_evaluation)

# 14. Display selected models
cat("\n--- SELECTED MODEL BY PRODUCT ---\n")

selected_models <- model_evaluation[
  !duplicated(model_evaluation$product_id),
  c("product_id", "model")
]

print(selected_models)

# 15. Display future demand predictions
cat("\n--- NEXT 7 DAYS DEMAND FORECAST ---\n")
print(demand_forecast)

# 16. Save results
write.csv(
  model_evaluation,
  "data/model_evaluation.csv",
  row.names = FALSE
)

write.csv(
  demand_forecast,
  "data/demand_forecast.csv",
  row.names = FALSE
)

# 17. Plot forecast for the first product
first_product <- product_ids[1]

historical <- sales[
  sales$product_id == first_product,
]

historical_daily <- aggregate(
  demand ~ date,
  data = historical,
  FUN = sum
)

historical_daily <- historical_daily[
  order(historical_daily$date),
]

future <- demand_forecast[
  demand_forecast$product_id == first_product,
]

plot(
  tail(historical_daily$date, 30),
  tail(historical_daily$demand, 30),
  type = "l",
  main = paste(
    "Demand Forecast:",
    first_product
  ),
  xlab = "Date",
  ylab = "Demand (Units)"
)

lines(
  future$date,
  future$predicted_demand,
  type = "b",
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "Historical Demand",
    "Forecast Demand"
  ),
  lty = c(1, 2),
  pch = c(NA, 1)
)

cat("\nDemand forecasting completed successfully!\n")
cat("Two forecasting CSV files have been saved.\n")
