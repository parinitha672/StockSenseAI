
# StockSense AI - Interactive Inventory Dashboard

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

# 1. Load project data
inventory <- read.csv("data/cleaned_inventory.csv")
sales <- read.csv("data/cleaned_sales.csv")
forecast <- read.csv("data/demand_forecast.csv")
stockout <- read.csv("data/stockout_predictions.csv")

# 2. Make sure product IDs use the same format
inventory$product_id <- as.character(inventory$product_id)
sales$product_id <- as.character(sales$product_id)
forecast$product_id <- as.character(forecast$product_id)
stockout$product_id <- as.character(stockout$product_id)

# Helper to find a column safely
find_col <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

# 3. Dashboard layout
ui <- fluidPage(
  titlePanel("StockSense AI: Inventory Forecasting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "product",
        "Select Product",
        choices = setNames(
          inventory$product_id,
          paste(inventory$product_name, inventory$product_id, sep = " - ")
        ),
        selected = inventory$product_id[1]
      ),
      helpText("Select a product to update the dashboard.")
    ),
    
    mainPanel(
      h3(textOutput("selected_name")),
      
      fluidRow(
        column(4, wellPanel(
          h4("Current Stock"),
          h3(textOutput("current_stock"))
        )),
        column(4, wellPanel(
          h4("Reorder Level"),
          h3(textOutput("reorder_level"))
        )),
        column(4, wellPanel(
          h4("Days of Stock"),
          h3(textOutput("days_stock"))
        ))
      ),
      
      tabsetPanel(
        tabPanel(
          "Inventory Overview",
          br(),
          tableOutput("inventory_table"),
          h4("Stock-Out Risk"),
          tableOutput("risk_table")
        ),
        
        tabPanel(
          "Demand Analysis",
          br(),
          plotOutput("sales_plot"),
          tableOutput("sales_table")
        ),
        
        tabPanel(
          "Demand Forecast",
          br(),
          plotOutput("forecast_plot"),
          tableOutput("forecast_table")
        ),
        
        tabPanel(
          "Stock-Out Alerts",
          br(),
          tableOutput("alert_table")
        ),
        
        tabPanel(
          "Replenishment",
          br(),
          tableOutput("replenishment_table")
        )
      )
    )
  )
)

# 4. Dashboard calculations and outputs
server <- function(input, output, session) {
  
  # Every reactive dataset depends on the selected product
  selected_inventory <- reactive({
    req(input$product)
    inventory[inventory$product_id == input$product, , drop = FALSE]
  })
  
  selected_sales <- reactive({
    req(input$product)
    sales[sales$product_id == input$product, , drop = FALSE]
  })
  
  selected_forecast <- reactive({
    req(input$product)
    forecast[forecast$product_id == input$product, , drop = FALSE]
  })
  
  selected_stockout <- reactive({
    req(input$product)
    stockout[stockout$product_id == input$product, , drop = FALSE]
  })
  
  output$selected_name <- renderText({
    row <- selected_inventory()
    as.character(row$product_name[1])
  })
  
  output$current_stock <- renderText({
    row <- selected_inventory()
    as.character(row$current_stock[1])
  })
  
  output$reorder_level <- renderText({
    row <- selected_inventory()
    as.character(row$reorder_level[1])
  })
  
  output$days_stock <- renderText({
    row <- selected_stockout()
    col <- find_col(row, c("days_of_stock", "days_stock"))
    
    if (is.na(col) || is.na(row[[col]][1])) {
      "Not available"
    } else {
      round(as.numeric(row[[col]][1]), 1)
    }
  })
  
  output$inventory_table <- renderTable({
    row <- selected_inventory()
    cols <- intersect(
      c("product_id", "product_name", "category",
        "current_stock", "reorder_level", "lead_time_days"),
      names(row)
    )
    row[, cols, drop = FALSE]
  }, rownames = FALSE)
  
  output$risk_table <- renderTable({
    row <- selected_stockout()
    cols <- intersect(
      c("product_id", "stockout_risk", "stock_status",
        "days_of_stock", "estimated_stockout_date"),
      names(row)
    )
    
    if (length(cols) == 0) {
      data.frame(Message = "Risk details are unavailable.")
    } else {
      row[, cols, drop = FALSE]
    }
  }, rownames = FALSE)
  
  output$sales_plot <- renderPlot({
    df <- selected_sales()
    req(nrow(df) > 0)
    
    date_col <- find_col(df, c("date", "sale_date", "order_date"))
    demand_col <- find_col(
      df, c("quantity_sold", "units_sold", "sales_quantity", "demand")
    )
    
    validate(
      need(!is.na(date_col), "No date column found in sales data."),
      need(!is.na(demand_col), "No sales quantity column found.")
    )
    
    df[[date_col]] <- as.Date(df[[date_col]])
    df <- df[order(df[[date_col]]), ]
    
    plot(
      df[[date_col]], df[[demand_col]],
      type = "l",
      xlab = "Date",
      ylab = "Quantity Sold",
      main = paste("Historical Demand -", input$product)
    )
  })
  
  output$sales_table <- renderTable({
    df <- selected_sales()
    date_col <- find_col(df, c("date", "sale_date", "order_date"))
    demand_col <- find_col(
      df, c("quantity_sold", "units_sold", "sales_quantity", "demand")
    )
    
    if (is.na(date_col) || is.na(demand_col)) {
      return(data.frame(Message = "Sales columns could not be identified."))
    }
    
    df[, c(date_col, demand_col), drop = FALSE]
  }, rownames = FALSE, striped = TRUE)
  
  output$forecast_plot <- renderPlot({
    df <- selected_forecast()
    req(nrow(df) > 0)
    
    date_col <- find_col(df, c("date", "forecast_date"))
    demand_col <- find_col(
      df, c("predicted_demand", "forecast_demand", "forecast")
    )
    
    validate(
      need(!is.na(date_col), "No forecast date column found."),
      need(!is.na(demand_col), "No predicted demand column found.")
    )
    
    df[[date_col]] <- as.Date(df[[date_col]])
    df <- df[order(df[[date_col]]), ]
    
    plot(
      df[[date_col]], df[[demand_col]],
      type = "b",
      xlab = "Forecast Date",
      ylab = "Predicted Demand",
      main = paste("7-Day Demand Forecast -", input$product)
    )
  })
  
  output$forecast_table <- renderTable({
    df <- selected_forecast()
    df
  }, rownames = FALSE, striped = TRUE)
  
  output$alert_table <- renderTable({
    row <- selected_stockout()
    row
  }, rownames = FALSE, striped = TRUE)
  
  output$replenishment_table <- renderTable({
    row <- selected_stockout()
    cols <- intersect(
      c("product_id", "product_name", "current_stock",
        "average_daily_demand", "next_7_day_demand",
        "lead_time_demand", "recommended_order_quantity",
        "stock_status"),
      names(row)
    )
    
    if (length(cols) == 0) {
      data.frame(Message = "Replenishment details are unavailable.")
    } else {
      row[, cols, drop = FALSE]
    }
  }, rownames = FALSE, striped = TRUE)
}

# 5. Launch the dashboard
shinyApp(ui = ui, server = server)
