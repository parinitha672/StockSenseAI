
# StockSense AI
# Step 1: Create sample inventory and sales datasets

# Make the results reproducible
set.seed(123)

# Create a folder for the data
if (!dir.exists("data")) {
  dir.create("data")
}

# Define sample products
products <- data.frame(
  product_id = c("P101", "P102", "P103", "P104", "P105", "P106"),
  product_name = c(
    "Rice", "Cooking Oil", "Milk",
    "Bread", "Sugar", "Coffee"
  ),
  category = c(
    "Grocery", "Grocery", "Dairy",
    "Bakery", "Grocery", "Beverages"
  ),
  average_daily_demand = c(25, 12, 30, 20, 15, 8),
  current_stock = c(220, 75, 45, 38, 160, 90),
  lead_time_days = c(4, 5, 2, 2, 4, 6),
  reorder_level = c(120, 60, 70, 50, 80, 55)
)

# Generate 180 days of historical sales
dates <- seq(
  from = Sys.Date() - 179,
  to = Sys.Date(),
  by = "day"
)

# Create one row per product per day
sales <- expand.grid(
  date = dates,
  product_id = products$product_id
)

# Add product information
sales$product_name <- products$product_name[
  match(sales$product_id, products$product_id)
]

sales$category <- products$category[
  match(sales$product_id, products$product_id)
]

sales$average_daily_demand <- products$average_daily_demand[
  match(sales$product_id, products$product_id)
]

# Add a weekday sales pattern
weekday_factor <- ifelse(
  weekdays(sales$date) %in% c("Saturday", "Sunday"),
  1.15,
  1
)

# Generate realistic sample daily demand
sales$demand <- rpois(
  nrow(sales),
  lambda = sales$average_daily_demand *
    weekday_factor
)

# Keep dates in chronological order
sales <- sales[order(sales$date, sales$product_id), ]

# Remove helper column
sales$average_daily_demand <- NULL

# Create a separate inventory master table
inventory <- products

# Save both datasets as CSV files
write.csv(
  sales,
  "data/sales_history.csv",
  row.names = FALSE
)

write.csv(
  inventory,
  "data/inventory_master.csv",
  row.names = FALSE
)

# Display confirmation
cat("StockSense AI datasets created successfully!\n")
cat("Sales records:", nrow(sales), "\n")
cat("Products:", nrow(inventory), "\n")

# Display sample records
head(sales)
print(inventory)



