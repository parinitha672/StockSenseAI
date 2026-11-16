
# ============================================================
# StockSense AI
# Step 5: Improved Stock-Out Prediction and Replenishment
# ============================================================

# 1. Load the datasets

inventory <- read.csv(
  "data/cleaned_inventory.csv",
  stringsAsFactors = FALSE
)

sales <- read.csv(
  "data/cleaned_sales.csv",
  stringsAsFactors = FALSE
)

# Prefer improved forecast; use original if unavailable

if (file.exists("data/demand_forecast_improved.csv")) {
  
  forecast <- read.csv(
    "data/demand_forecast_improved.csv",
    stringsAsFactors = FALSE
  )
  
  cat("Using improved demand forecast.\n")
  
} else if (file.exists("data/demand_forecast.csv")) {
  
  forecast <- read.csv(
    "data/demand_forecast.csv",
    stringsAsFactors = FALSE
  )
  
  cat("Using original demand forecast.\n")
  
} else {
  
  stop("No forecast file found. Run Step 4 or Step 7 first.")
}

# 2. Validate required columns

if (!"product_id" %in% names(inventory) ||
    !"product_id" %in% names(sales) ||
    !"product_id" %in% names(forecast)) {
  stop("All three files must contain product_id.")
}

if (!"date" %in% names(forecast) ||
    !"predicted_demand" %in% names(forecast)) {
  stop("Forecast file must contain date and predicted_demand.")
}

if (!"current_stock" %in% names(inventory) ||
    !"lead_time_days" %in% names(inventory) ||
    !"reorder_level" %in% names(inventory)) {
  stop("Inventory file is missing required stock columns.")
}

# Find the sales date and demand columns

date_candidates <- c("date", "sale_date", "order_date")

demand_candidates <- c(
  "demand",
  "quantity_sold",
  "units_sold",
  "sales_quantity",
  "quantity",
  "sales"
)

date_matches <- intersect(date_candidates, names(sales))
demand_matches <- intersect(demand_candidates, names(sales))

if (length(date_matches) == 0 ||
    length(demand_matches) == 0) {
  stop(
    paste(
      "Could not identify sales date or demand columns:",
      paste(names(sales), collapse = ", ")
    )
  )
}

sales_date_col <- date_matches[1]
sales_demand_col <- demand_matches[1]

# 3. Prepare dates and numeric columns

sales$date_fixed <- as.Date(
  sales[[sales_date_col]]
)

sales$demand_fixed <- suppressWarnings(
  as.numeric(sales[[sales_demand_col]])
)

forecast$date <- as.Date(forecast$date)

forecast$predicted_demand <- suppressWarnings(
  as.numeric(forecast$predicted_demand)
)

inventory$current_stock <- as.numeric(
  inventory$current_stock
)

inventory$lead_time_days <- as.numeric(
  inventory$lead_time_days
)

inventory$reorder_level <- as.numeric(
  inventory$reorder_level
)

# Remove invalid sales and forecast rows

sales <- sales[
  !is.na(sales$product_id) &
    !is.na(sales$date_fixed) &
    !is.na(sales$demand_fixed),
]

forecast <- forecast[
  !is.na(forecast$product_id) &
    !is.na(forecast$date) &
    !is.na(forecast$predicted_demand),
]

if (nrow(sales) == 0 || nrow(forecast) == 0) {
  stop("Sales or forecast data contains no valid rows.")
}

if (anyNA(inventory$current_stock) ||
    anyNA(inventory$lead_time_days) ||
    anyNA(inventory$reorder_level)) {
  stop("Inventory contains missing or invalid stock values.")
}

# 4. Calculate average daily demand
# First total sales by product and date.
# Then calculate the average across each product's recorded days.

daily_sales <- aggregate(
  demand_fixed ~ product_id + date_fixed,
  data = sales,
  FUN = sum
)

average_demand <- aggregate(
  demand_fixed ~ product_id,
  data = daily_sales,
  FUN = mean
)

names(average_demand)[2] <- "average_daily_demand"

# Remove any old average demand column before merging

if ("average_daily_demand" %in% names(inventory)) {
  inventory$average_daily_demand <- NULL
}

# 5. Calculate next seven days of forecast demand

forecast_summary <- aggregate(
  predicted_demand ~ product_id,
  data = forecast,
  FUN = sum
)

names(forecast_summary)[2] <- "next_7_days_demand"

# Merge inventory and demand information

stockout <- merge(
  inventory,
  average_demand,
  by = "product_id",
  all.x = TRUE
)

stockout <- merge(
  stockout,
  forecast_summary,
  by = "product_id",
  all.x = TRUE
)

# Fill missing forecast summaries using average demand

stockout$average_daily_demand[
  is.na(stockout$average_daily_demand)
] <- 0

stockout$next_7_days_demand[
  is.na(stockout$next_7_days_demand)
] <- 0

# 6. Calculate estimated days of stock remaining

stockout$days_of_stock <- ifelse(
  stockout$average_daily_demand > 0,
  stockout$current_stock /
    stockout$average_daily_demand,
  ifelse(stockout$current_stock <= 0, 0, Inf)
)

# 7. Estimate demand during supplier lead time
# Use forecast values where available.
# Use average demand to estimate days beyond the forecast horizon.

stockout$expected_lead_time_demand <- 0

for (i in seq_len(nrow(stockout))) {
  
  product <- stockout$product_id[i]
  
  product_forecast <- forecast[
    forecast$product_id == product,
  ]
  
  product_forecast <- product_forecast[
    order(product_forecast$date),
  ]
  
  lead_days <- max(
    0,
    ceiling(stockout$lead_time_days[i])
  )
  
  available_days <- min(
    lead_days,
    nrow(product_forecast)
  )
  
  forecast_demand <- if (available_days > 0) {
    sum(
      head(
        product_forecast$predicted_demand,
        available_days
      )
    )
  } else {
    0
  }
  
  extra_days <- max(
    0,
    lead_days - available_days
  )
  
  extra_demand <- extra_days *
    stockout$average_daily_demand[i]
  
  stockout$expected_lead_time_demand[i] <- ceiling(
    forecast_demand + extra_demand
  )
}

# 8. Identify stock-out risk
# High Risk means current stock may not cover
# expected demand during supplier lead time.

stockout$stockout_risk <- ifelse(
  stockout$current_stock <=
    stockout$expected_lead_time_demand,
  "High Risk",
  "Lower Risk"
)

# 9. Identify current stock status

stockout$stock_status <- ifelse(
  stockout$current_stock <= stockout$reorder_level,
  "Reorder Required",
  "Stock Available"
)

# 10. Estimate stock-out dates using the forecast

stockout$estimated_stockout_date <- as.Date(
  rep(NA_character_, nrow(stockout))
)

stockout$estimated_days_until_stockout <- NA_real_

for (i in seq_len(nrow(stockout))) {
  
  product <- stockout$product_id[i]
  
  product_forecast <- forecast[
    forecast$product_id == product,
  ]
  
  product_forecast <- product_forecast[
    order(product_forecast$date),
  ]
  
  remaining_stock <- stockout$current_stock[i]
  
  days_until_stockout <- NA_real_
  estimated_date <- as.Date(NA)
  
  # Stock is already depleted
  
  if (remaining_stock <= 0) {
    
    days_until_stockout <- 0
    
    if (nrow(product_forecast) > 0) {
      estimated_date <- product_forecast$date[1]
    }
    
  } else {
    
    # Check each available forecast day
    
    for (j in seq_len(nrow(product_forecast))) {
      
      remaining_stock <- remaining_stock -
        product_forecast$predicted_demand[j]
      
      if (remaining_stock <= 0) {
        
        days_until_stockout <- j
        estimated_date <- product_forecast$date[j]
        
        break
      }
    }
    
    # If stock lasts beyond the forecast horizon,
    # estimate remaining days using average demand.
    
    if (is.na(days_until_stockout) &&
        stockout$average_daily_demand[i] > 0) {
      
      extra_days <- ceiling(
        remaining_stock /
          stockout$average_daily_demand[i]
      )
      
      if (nrow(product_forecast) > 0) {
        
        days_until_stockout <-
          nrow(product_forecast) + extra_days
        
        estimated_date <-
          max(product_forecast$date) + extra_days
        
      } else {
        
        days_until_stockout <- extra_days
        
        estimated_date <- max(sales$date_fixed) +
          extra_days
      }
    }
  }
  
  stockout$estimated_days_until_stockout[i] <-
    days_until_stockout
  
  stockout$estimated_stockout_date[i] <-
    estimated_date
}

# 11. Calculate recommended order quantity
# Target stock = expected demand during lead time
# plus a seven-day buffer.
#
# Forecast demand is used for the available forecast days.
# Average demand estimates any remaining days.

stockout$recommended_order <- 0

for (i in seq_len(nrow(stockout))) {
  
  product <- stockout$product_id[i]
  
  product_forecast <- forecast[
    forecast$product_id == product,
  ]
  
  product_forecast <- product_forecast[
    order(product_forecast$date),
  ]
  
  lead_days <- max(
    0,
    ceiling(stockout$lead_time_days[i])
  )
  
  buffer_days <- 7
  
  target_days <- lead_days + buffer_days
  
  available_days <- min(
    target_days,
    nrow(product_forecast)
  )
  
  forecast_target_demand <- if (available_days > 0) {
    sum(
      head(
        product_forecast$predicted_demand,
        available_days
      )
    )
  } else {
    0
  }
  
  remaining_target_days <- max(
    0,
    target_days - available_days
  )
  
  estimated_extra_demand <- remaining_target_days *
    stockout$average_daily_demand[i]
  
  target_stock <- ceiling(
    forecast_target_demand +
      estimated_extra_demand
  )
  
  stockout$recommended_order[i] <- max(
    0,
    target_stock - stockout$current_stock[i]
  )
}

# 12. Display stock-out prediction results

cat("\n========================================\n")
cat("STOCKSENSE AI - STOCK-OUT PREDICTIONS\n")
cat("========================================\n")

print(
  stockout[, c(
    "product_id",
    "product_name",
    "current_stock",
    "average_daily_demand",
    "next_7_days_demand",
    "days_of_stock",
    "expected_lead_time_demand",
    "estimated_stockout_date",
    "estimated_days_until_stockout",
    "stockout_risk",
    "stock_status",
    "recommended_order"
  )]
)

# 13. Count high-risk products

high_risk_count <- sum(
  stockout$stockout_risk == "High Risk",
  na.rm = TRUE
)

cat(
  "\nProducts at high stock-out risk:",
  high_risk_count,
  "\n"
)

# 14. Display products requiring attention

high_risk_products <- stockout[
  stockout$stockout_risk == "High Risk",
]

cat("\n--- PRODUCTS REQUIRING ATTENTION ---\n")

if (nrow(high_risk_products) > 0) {
  
  print(
    high_risk_products[, c(
      "product_id",
      "product_name",
      "current_stock",
      "expected_lead_time_demand",
      "estimated_stockout_date",
      "recommended_order"
    )]
  )
  
} else {
  
  cat("No products currently meet the high-risk rule.\n")
}

# 15. Save predictions for the dashboard

write.csv(
  stockout,
  "data/stockout_predictions.csv",
  row.names = FALSE
)

cat("\nStock-out prediction completed successfully!\n")
cat("Results saved to data/stockout_predictions.csv\n")
