
# ============================================================
# StockSense AI
# Step 3: Exploratory Data Analysis (EDA)
# ============================================================

# 1. Load cleaned datasets
sales <- read.csv(
  "data/cleaned_sales.csv",
  stringsAsFactors = FALSE
)

inventory <- read.csv(
  "data/cleaned_inventory.csv",
  stringsAsFactors = FALSE
)

# Convert date column
sales$date <- as.Date(sales$date)

# 2. Display dataset information
cat("\n--- DATASET INFORMATION ---\n")

cat("Total sales records:", nrow(sales), "\n")
cat("Total products:", nrow(inventory), "\n")

cat(
  "Date range:",
  as.character(min(sales$date)),
  "to",
  as.character(max(sales$date)),
  "\n"
)

# 3. Calculate total demand per product
product_demand <- aggregate(
  demand ~ product_id + product_name + category,
  data = sales,
  FUN = sum
)

names(product_demand)[4] <- "total_demand"

# 4. Calculate average daily demand per product
average_demand <- aggregate(
  demand ~ product_id,
  data = sales,
  FUN = mean
)

names(average_demand)[2] <-
  "observed_average_daily_demand"

# 5. Combine product demand with inventory data
analysis <- merge(
  inventory,
  product_demand,
  by = c("product_id", "product_name", "category")
)

analysis <- merge(
  analysis,
  average_demand,
  by = "product_id"
)

# 6. Calculate estimated days of stock remaining
analysis$days_of_stock <- ifelse(
  analysis$observed_average_daily_demand > 0,
  analysis$current_stock /
    analysis$observed_average_daily_demand,
  Inf
)

# 7. Identify low-stock products
analysis$stock_status <- ifelse(
  analysis$current_stock <= analysis$reorder_level,
  "Low Stock",
  "Stock Available"
)

# 8. Identify possible stock-out risk
analysis$stockout_risk <- ifelse(
  analysis$days_of_stock <= analysis$lead_time_days,
  "High Risk",
  "Lower Risk"
)

# 9. Calculate suggested replenishment quantity
# Target stock = lead-time demand + 7 days of demand

analysis$recommended_order <- pmax(
  0,
  ceiling(
    analysis$observed_average_daily_demand *
      (analysis$lead_time_days + 7) -
      analysis$current_stock
  )
)

# 10. Display product-wise demand analysis
cat("\n--- PRODUCT DEMAND ANALYSIS ---\n")

print(
  analysis[, c(
    "product_name",
    "total_demand",
    "observed_average_daily_demand"
  )]
)

# 11. Display inventory status
cat("\n--- INVENTORY STATUS ---\n")

print(
  analysis[, c(
    "product_name",
    "current_stock",
    "reorder_level",
    "days_of_stock",
    "stock_status",
    "stockout_risk",
    "recommended_order"
  )]
)

# 12. Identify the highest-demand product
top_index <- which.max(analysis$total_demand)

cat("\n--- HIGHEST DEMAND PRODUCT ---\n")

cat(
  "Product:",
  analysis$product_name[top_index],
  "\n"
)

cat(
  "Total demand:",
  analysis$total_demand[top_index],
  "units\n"
)

# 13. Calculate category-wise demand
category_demand <- aggregate(
  demand ~ category,
  data = sales,
  FUN = sum
)

category_demand <- category_demand[
  order(-category_demand$demand),
]

cat("\n--- CATEGORY-WISE DEMAND ---\n")
print(category_demand)

# 14. Calculate total daily demand
daily_demand <- aggregate(
  demand ~ date,
  data = sales,
  FUN = sum
)

daily_demand <- daily_demand[
  order(daily_demand$date),
]

cat("\n--- DAILY DEMAND SUMMARY ---\n")
print(summary(daily_demand$demand))

# 15. Calculate monthly demand
sales$month <- format(sales$date, "%Y-%m")

monthly_demand <- aggregate(
  demand ~ month,
  data = sales,
  FUN = sum
)

cat("\n--- MONTHLY DEMAND ---\n")
print(monthly_demand)

# 16. Save analysis results
write.csv(
  analysis,
  "data/product_analysis.csv",
  row.names = FALSE
)

write.csv(
  category_demand,
  "data/category_demand.csv",
  row.names = FALSE
)

write.csv(
  daily_demand,
  "data/daily_demand.csv",
  row.names = FALSE
)

write.csv(
  monthly_demand,
  "data/monthly_demand.csv",
  row.names = FALSE
)

# 17. Chart 1: Total demand by product
barplot(
  analysis$total_demand,
  names.arg = analysis$product_name,
  las = 2,
  main = "Total Demand by Product",
  xlab = "Product",
  ylab = "Total Units Demanded",
  cex.names = 0.8
)

# 18. Chart 2: Demand by category
barplot(
  category_demand$demand,
  names.arg = category_demand$category,
  las = 2,
  main = "Demand by Category",
  xlab = "Category",
  ylab = "Total Units Demanded"
)

# 19. Chart 3: Daily demand trend
plot(
  daily_demand$date,
  daily_demand$demand,
  type = "l",
  main = "Daily Inventory Demand Trend",
  xlab = "Date",
  ylab = "Total Daily Demand"
)

# 20. Completion message
cat("\nEDA completed successfully!\n")
cat("Four analysis CSV files have been saved.\n")
