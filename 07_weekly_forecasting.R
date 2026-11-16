
# ============================================================
# StockSense AI - Improved Weekly Forecasting
# Compare:
# 1. Mean Baseline
# 2. Weekly Seasonal Naive
# 3. 7-Day Moving Average
# ============================================================

# STEP 1: Load cleaned sales data

sales <- read.csv(
  "data/cleaned_sales.csv",
  stringsAsFactors = FALSE
)

# STEP 2: Identify date and demand columns

date_candidates <- c("date", "sale_date", "order_date")

demand_candidates <- c(
  "quantity_sold", "units_sold", "sales_quantity",
  "demand", "quantity", "sales"
)

date_matches <- intersect(date_candidates, names(sales))
demand_matches <- intersect(demand_candidates, names(sales))

if (length(date_matches) == 0 ||
    length(demand_matches) == 0) {
  stop(
    paste(
      "Check column names in cleaned_sales.csv:",
      paste(names(sales), collapse = ", ")
    )
  )
}

if (!"product_id" %in% names(sales)) {
  stop("The sales file must contain product_id.")
}

date_col <- date_matches[1]
demand_col <- demand_matches[1]

# STEP 3: Prepare daily demand data

sales$date_fixed <- as.Date(sales[[date_col]])
sales$demand_fixed <- suppressWarnings(
  as.numeric(sales[[demand_col]])
)

# Remove rows with missing or invalid values

sales <- sales[
  !is.na(sales$date_fixed) &
    !is.na(sales$demand_fixed) &
    !is.na(sales$product_id),
]

daily <- aggregate(
  demand_fixed ~ product_id + date_fixed,
  data = sales,
  FUN = sum
)

names(daily) <- c(
  "product_id",
  "date",
  "actual_demand"
)

daily <- daily[order(daily$product_id, daily$date), ]

# STEP 4: Create functions for forecasting

# Model 1: Mean Baseline
mean_forecast <- function(train, horizon) {
  rep(mean(train), horizon)
}

# Model 2: Weekly Seasonal Naive
# Uses demand from seven days earlier.
# During multi-day forecasting, previous predictions
# are used when actual values are not available.

seasonal_forecast <- function(history, horizon) {
  
  predictions <- numeric(horizon)
  
  for (i in seq_len(horizon)) {
    
    lag_position <- length(history) + i - 7
    
    if (lag_position <= length(history)) {
      
      predictions[i] <- history[lag_position]
      
    } else {
      
      predictions[i] <- predictions[i - 7]
    }
  }
  
  predictions
}

# Model 3: 7-Day Moving Average
# Each prediction uses the most recent seven values.
# Future predictions are added to history recursively.

moving_average_forecast <- function(history, horizon) {
  
  predictions <- numeric(horizon)
  extended_history <- history
  
  for (i in seq_len(horizon)) {
    
    recent_values <- tail(extended_history, 7)
    
    predictions[i] <- mean(recent_values)
    
    extended_history <- c(
      extended_history,
      predictions[i]
    )
  }
  
  predictions
}

# STEP 5: Calculate model evaluation metrics

calculate_mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# STEP 6: Evaluate all models for each product

evaluation_list <- list()
forecast_list <- list()

test_size <- 14
forecast_horizon <- 7

for (p in unique(daily$product_id)) {
  
  one_product <- daily[
    daily$product_id == p,
  ]
  
  one_product <- one_product[
    order(one_product$date),
  ]
  
  n <- nrow(one_product)
  
  if (n < 28) {
    warning(
      paste("Not enough data for product", p)
    )
    next
  }
  
  # Hold out the last 14 days
  # Use earlier observations for training
  
  train_end <- n - test_size
  
  train <- one_product$actual_demand[
    seq_len(train_end)
  ]
  
  actual_test <- one_product$actual_demand[
    (train_end + 1):n
  ]
  
  # MODEL 1: Mean Baseline
  
  mean_predictions <- mean_forecast(
    train,
    test_size
  )
  
  # MODEL 2: Weekly Seasonal Naive
  
  seasonal_predictions <- seasonal_forecast(
    train,
    test_size
  )
  
  # MODEL 3: 7-Day Moving Average
  
  moving_predictions <- moving_average_forecast(
    train,
    test_size
  )
  
  # Calculate MAE for each model
  
  mae_mean <- calculate_mae(
    actual_test,
    mean_predictions
  )
  
  mae_seasonal <- calculate_mae(
    actual_test,
    seasonal_predictions
  )
  
  mae_moving <- calculate_mae(
    actual_test,
    moving_predictions
  )
  
  # Calculate RMSE for each model
  
  rmse_mean <- calculate_rmse(
    actual_test,
    mean_predictions
  )
  
  rmse_seasonal <- calculate_rmse(
    actual_test,
    seasonal_predictions
  )
  
  rmse_moving <- calculate_rmse(
    actual_test,
    moving_predictions
  )
  
  # Store evaluation results
  
  product_evaluation <- data.frame(
    product_id = p,
    
    model = c(
      "Mean Baseline",
      "Weekly Seasonal Naive",
      "7-Day Moving Average"
    ),
    
    MAE = c(
      mae_mean,
      mae_seasonal,
      mae_moving
    ),
    
    RMSE = c(
      rmse_mean,
      rmse_seasonal,
      rmse_moving
    ),
    
    stringsAsFactors = FALSE
  )
  
  # Select the model with the lowest MAE
  
  best_index <- which.min(product_evaluation$MAE)
  
  best_model <- product_evaluation$model[
    best_index
  ]
  
  evaluation_list[[as.character(p)]] <-
    product_evaluation
  
  # STEP 7: Forecast the next seven days
  
  all_history <- one_product$actual_demand
  
  if (best_model == "Mean Baseline") {
    
    future_demand <- mean_forecast(
      all_history,
      forecast_horizon
    )
    
  } else if (
    best_model == "Weekly Seasonal Naive"
  ) {
    
    future_demand <- seasonal_forecast(
      all_history,
      forecast_horizon
    )
    
  } else {
    
    future_demand <- moving_average_forecast(
      all_history,
      forecast_horizon
    )
  }
  
  future_dates <- seq(
    max(one_product$date) + 1,
    by = "day",
    length.out = forecast_horizon
  )
  
  forecast_list[[as.character(p)]] <- data.frame(
    product_id = p,
    date = future_dates,
    predicted_demand = round(
      pmax(0, future_demand)
    ),
    selected_model = best_model,
    stringsAsFactors = FALSE
  )
}

# STEP 8: Combine results safely

if (length(evaluation_list) == 0 ||
    length(forecast_list) == 0) {
  stop(
    "No products were evaluated. Check the cleaned sales data."
  )
}

model_evaluation_improved <- do.call(
  rbind,
  evaluation_list
)

demand_forecast_improved <- do.call(
  rbind,
  forecast_list
)

rownames(model_evaluation_improved) <- NULL
rownames(demand_forecast_improved) <- NULL

# STEP 9: Save output files
# These filenames preserve dashboard compatibility.

write.csv(
  model_evaluation_improved,
  "data/model_evaluation_improved.csv",
  row.names = FALSE
)

write.csv(
  demand_forecast_improved,
  "data/demand_forecast_improved.csv",
  row.names = FALSE
)

# STEP 10: Display evaluation results

cat("\n========================================\n")
cat("STOCKSENSE AI - MODEL EVALUATION\n")
cat("========================================\n")

print(model_evaluation_improved)

cat("\n========================================\n")
cat("SELECTED MODEL FOR EACH PRODUCT\n")
cat("========================================\n")

selected_models <- unique(
  demand_forecast_improved[
    c("product_id", "selected_model")
  ]
)

print(selected_models)

cat("\n========================================\n")
cat("NEXT SEVEN DAYS - DEMAND FORECAST\n")
cat("========================================\n")

print(demand_forecast_improved)

cat("\n========================================\n")
cat("FORECASTING COMPLETED SUCCESSFULLY!\n")
cat("========================================\n")

cat(
  "\nFiles created:",
  "\n1. data/model_evaluation_improved.csv",
  "\n2. data/demand_forecast_improved.csv\n"
)
