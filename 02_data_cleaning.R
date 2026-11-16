
# StockSense AI
# Step 2: Data Cleaning and Validation

# 1. Load the datasets
sales <- read.csv(
  "data/sales_history.csv",
  stringsAsFactors = FALSE
)

inventory <- read.csv(
  "data/inventory_master.csv",
  stringsAsFactors = FALSE
)

# 2. Check the dataset dimensions
cat("Sales dataset dimensions:\n")
print(dim(sales))

cat("Inventory dataset dimensions:\n")
print(dim(inventory))

# 3. Check column names
cat("Sales columns:\n")
print(names(sales))

cat("Inventory columns:\n")
print(names(inventory))

# 4. Check missing values
cat("Missing values in sales:\n")
print(colSums(is.na(sales)))

cat("Missing values in inventory:\n")
print(colSums(is.na(inventory)))

# 5. Convert the date column
sales$date <- as.Date(sales$date)

# 6. Check duplicate sales records
duplicate_sales <- duplicated(
  sales[c("date", "product_id")]
)

cat("Duplicate sales records:\n")
print(sum(duplicate_sales))

# Remove duplicate date-product records if any exist
sales <- sales[!duplicate_sales, ]

# 7. Check duplicate product IDs
cat("Duplicate product IDs:\n")
print(sum(duplicated(inventory$product_id)))

# 8. Check invalid numerical values
cat("Negative demand records:\n")
print(sum(sales$demand < 0, na.rm = TRUE))

cat("Negative stock records:\n")
print(sum(inventory$current_stock < 0, na.rm = TRUE))

cat("Negative lead time records:\n")
print(sum(inventory$lead_time_days < 0, na.rm = TRUE))

# 9. Check whether all sales products exist in inventory
unknown_products <- setdiff(
  sales$product_id,
  inventory$product_id
)

cat("Products missing from inventory:\n")
print(unknown_products)

# 10. Display a statistical summary
cat("Sales summary:\n")
print(summary(sales$demand))

cat("Inventory summary:\n")
print(summary(inventory$current_stock))

# 11. Save cleaned datasets
write.csv(
  sales,
  "data/cleaned_sales.csv",
  row.names = FALSE
)

write.csv(
  inventory,
  "data/cleaned_inventory.csv",
  row.names = FALSE
)

cat("Data cleaning and validation completed!\n")
