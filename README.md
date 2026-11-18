# 📦 StockSense AI
## Real-Time Inventory Demand Forecasting and Stock-Out Prediction
Live dashboard : <https://stocksense-ai.shinyapps.io/stocksense-ai/>

StockSense AI is an inventory analytics project developed using R and Shiny. It analyzes historical sales, forecasts product demand, identifies potential stock-out risks, and recommends replenishment quantities through an interactive dashboard.

## 🎯 Project Objectives

- Analyze historical product demand.
- Forecast demand for the next seven days.
- Identify products at risk of stock-outs.
- Estimate days of inventory remaining.
- Compare expected demand with supplier lead times.
- Recommend replenishment quantities.
- Evaluate demand forecasting models.
- Present inventory insights through an interactive dashboard.

## ✨ Key Features

- **Inventory Overview:** View product stock and inventory details.
- **Demand Analysis:** Explore product demand trends and category summaries.
- **Demand Forecasting:** View predicted demand for upcoming days.
- **Stock-Out Alerts:** Identify products that may require immediate attention.
- **Replenishment Recommendations:** Review suggested order quantities.
- **Model Evaluation:** Compare forecasting models using MAE and RMSE.
- **Downloadable Reports:** Export inventory, sales, forecasts, alerts, and model results.

## 🛠️ Technologies Used

- R
- RStudio
- Shiny
- CSV datasets
- Base R graphics

## 📁 Project Structure

```text
StockSenseAI/
├── data/
│   ├── sales_history.csv
│   ├── inventory_master.csv
│   ├── cleaned_sales.csv
│   ├── cleaned_inventory.csv
│   ├── product_analysis.csv
│   ├── category_demand.csv
│   ├── daily_demand.csv
│   ├── monthly_demand.csv
│   ├── demand_forecast_improved.csv
│   ├── model_evaluation_improved.csv
│   └── stockout_predictions.csv
├── 01_create_dataset.R
├── 02_data_cleaning.R
├── 03_exploratory_analysis.R
├── 04_demand_forecasting.R
├── 05_stockout_prediction.R
├── 06_dashboard.R
├── 07_weekly_forecasting.R
└── README.md
```

*The filenames above reflect the expected project structure. Adjust them if your actual files differ.*

## 🚀 How to Run the Project

1. Install R and RStudio.
2. Open the `StockSenseAI.Rproj` project in RStudio.
3. Install Shiny if it is not already installed:

   ```r
   install.packages("shiny")
   ```

4. Run the scripts in numerical order to generate and process the datasets.
5. Ensure the required CSV files are available in the `data/` folder.
6. Open `06_dashboard.R`.
7. Click **Run App** to launch the interactive dashboard.

## 📊 Forecast Model Evaluation

The project compares three forecasting approaches:

- Mean Baseline
- Weekly Seasonal Naive
- 7-Day Moving Average

Forecast performance is evaluated using Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE). Lower values generally indicate smaller prediction errors.

## ⚠️ Limitations

This project is a decision-support prototype that uses available historical sales and inventory data. Forecasts, stock-out dates, risk classifications, and replenishment quantities are estimates rather than guarantees. Real-world deployment would require validation against actual business data, supplier constraints, and operational requirements.

## 🔮 Future Enhancements

- Integrate live inventory data.
- Add automated low-stock notifications.
- Incorporate supplier and purchase-order information.
- Improve forecasting with additional models.
- Deploy the dashboard online.
- Add user authentication and database integration.

## 👩‍💻 Project Information

**Project Name:** StockSense AI  
**Domain:** Data Analytics and Inventory Management  
**Programming Language:** R  
**Dashboard Framework:** Shiny

