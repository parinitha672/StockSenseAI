
# ============================================================
# STOCKSENSE AI
# Real-Time Inventory Demand Forecasting and Stock-Out Prediction
# Complete Interactive Shiny Dashboard
# ============================================================

library(shiny)

# ============================================================
# 1. DATA LOADING
# ============================================================

read_project_data <- function(file_name) {
  path <- file.path("data", file_name)
  
  if (!file.exists(path)) {
    warning(paste("File not found:", path))
    return(data.frame())
  }
  
  tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) {
      warning(paste("Error reading", path, ":", e$message))
      data.frame()
    }
  )
}

load_all_data <- function() {
  forecast_file <- if (
    file.exists("data/demand_forecast_improved.csv")
  ) {
    "demand_forecast_improved.csv"
  } else {
    "demand_forecast.csv"
  }
  
  evaluation_file <- if (
    file.exists("data/model_evaluation_improved.csv")
  ) {
    "model_evaluation_improved.csv"
  } else {
    "model_evaluation.csv"
  }
  
  list(
    inventory = read_project_data("cleaned_inventory.csv"),
    sales = read_project_data("cleaned_sales.csv"),
    stockout = read_project_data("stockout_predictions.csv"),
    product_analysis = read_project_data("product_analysis.csv"),
    category_demand = read_project_data("category_demand.csv"),
    daily_demand = read_project_data("daily_demand.csv"),
    monthly_demand = read_project_data("monthly_demand.csv"),
    forecast = read_project_data(forecast_file),
    evaluation = read_project_data(evaluation_file)
  )
}

# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

find_col <- function(df, candidates) {
  if (is.null(df) || ncol(df) == 0) return(NULL)
  
  positions <- match(tolower(candidates), tolower(names(df)))
  positions <- positions[!is.na(positions)]
  
  if (length(positions) == 0) return(NULL)
  
  names(df)[positions[1]]
}

to_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

safe_sum <- function(x) {
  x <- to_number(x)
  x <- x[is.finite(x)]
  
  if (length(x) == 0) return(0)
  
  sum(x)
}

format_number <- function(x, digits = 0) {
  x <- to_number(x)
  x <- x[is.finite(x)]
  
  if (length(x) == 0) return("N/A")
  
  format(
    round(x[1], digits),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

risk_column <- function(df) {
  find_col(df, c("stockout_risk", "risk_level", "risk"))
}

status_column <- function(df) {
  find_col(df, c("stock_status", "status", "inventory_status"))
}

filter_data <- function(df, selected_product, selected_category,
                        inventory_data) {
  if (nrow(df) == 0) return(df)
  
  id_col <- find_col(df, c("product_id", "productid", "sku"))
  
  if (!is.null(id_col) &&
      !is.null(selected_product) &&
      selected_product != "All Products") {
    df <- df[
      as.character(df[[id_col]]) == selected_product,
      ,
      drop = FALSE
    ]
  }
  
  if (selected_category != "All Categories" &&
      nrow(inventory_data) > 0) {
    
    inventory_id <- find_col(
      inventory_data, c("product_id", "productid", "sku")
    )
    
    category_col <- find_col(
      inventory_data, c("category", "product_category")
    )
    
    if (!is.null(id_col) &&
        !is.null(inventory_id) &&
        !is.null(category_col)) {
      
      valid_ids <- as.character(
        inventory_data[[inventory_id]][
          as.character(inventory_data[[category_col]]) ==
            selected_category
        ]
      )
      
      df <- df[
        as.character(df[[id_col]]) %in% valid_ids,
        ,
        drop = FALSE
      ]
    }
  }
  
  df
}

get_alerts <- function(df) {
  if (nrow(df) == 0) return(df)
  
  risk_col <- risk_column(df)
  status_col <- status_column(df)
  
  keep <- rep(FALSE, nrow(df))
  
  if (!is.null(risk_col)) {
    risk_text <- tolower(as.character(df[[risk_col]]))
    keep <- keep | grepl("high|critical", risk_text)
  }
  
  if (!is.null(status_col)) {
    status_text <- tolower(as.character(df[[status_col]]))
    keep <- keep | grepl("reorder|required|low stock", status_text)
  }
  
  df[keep, , drop = FALSE]
}

# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  tags$head(
    tags$title("StockSense AI Dashboard"),
    
    tags$style(HTML("
      body {
        background: #f2f5fa;
        color: #243247;
        font-family: 'Segoe UI', Arial, sans-serif;
      }

      .main-title {
        background: linear-gradient(120deg, #102a43, #176b87);
        color: white;
        padding: 25px;
        border-radius: 14px;
        margin: 12px 0 20px 0;
        box-shadow: 0 5px 18px rgba(16,42,67,0.16);
      }

      .main-title h2 {
        margin-top: 0;
        font-weight: 700;
      }

      .main-title p {
        color: #dcecf4;
        margin-bottom: 4px;
      }

      .sidebar-panel {
        background: white;
        padding: 18px;
        border-radius: 12px;
        box-shadow: 0 3px 12px rgba(30,50,80,0.07);
      }

      .kpi-row {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        margin-bottom: 20px;
      }

      .kpi-card {
        flex: 1 1 145px;
        min-width: 0;
        background: white;
        border-radius: 12px;
        padding: 17px;
        border-top: 4px solid #178ca4;
        box-shadow: 0 3px 12px rgba(30,50,80,0.07);
      }

      .kpi-card.red { border-top-color: #dc3545; }
      .kpi-card.orange { border-top-color: #f0a202; }
      .kpi-card.green { border-top-color: #159957; }
      .kpi-card.blue { border-top-color: #178ca4; }

      .kpi-label {
        color: #68788d;
        font-size: 12px;
        margin-bottom: 8px;
      }

      .kpi-value {
        color: #102a43;
        font-size: 27px;
        font-weight: 700;
        overflow-wrap: anywhere;
      }

      .section-panel {
        background: white;
        border-radius: 12px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 3px 12px rgba(30,50,80,0.06);

        /* IMPORTANT: keep contents inside the panel */
        width: 100%;
        max-width: 100%;
        min-width: 0;
        box-sizing: border-box;
      }

      .section-panel h4 {
        color: #176b87;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 14px;
      }

      /* FIX: tables scroll inside their own containers */
      .table-scroll {
        display: block;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        -webkit-overflow-scrolling: touch;
        box-sizing: border-box;
      }

      .table-scroll .shiny-table {
        width: max-content;
        min-width: 100%;
        max-width: none;
        white-space: nowrap;
        margin-bottom: 0;
      }

      .section-panel .shiny-table {
        font-size: 12px;
      }

      .section-panel th,
      .section-panel td {
        padding: 8px 10px !important;
        vertical-align: middle;
      }

      .section-panel .shiny-plot-output {
        max-width: 100%;
      }

      .nav-tabs > li > a {
        font-weight: 600;
        color: #176b87;
      }

      .nav-tabs > li.active > a {
        color: #102a43;
      }

      .btn-primary {
        background-color: #176b87;
        border-color: #176b87;
        border-radius: 7px;
        margin: 3px;
      }

      .btn-primary:hover {
        background-color: #102a43;
      }

      .btn-default {
        border-radius: 7px;
        margin: 3px;
      }

      .small-note {
        color: #68788d;
        font-size: 12px;
      }

      .footer-text {
        text-align: center;
        color: #68788d;
        padding: 18px;
        font-size: 12px;
      }

      @media (max-width: 768px) {
        .main-title {
          padding: 18px;
        }

        .kpi-value {
          font-size: 23px;
        }

        .section-panel {
          padding: 12px;
        }
      }
    "))
  ),
  
  # Header
  div(
    class = "main-title",
    h2("📦 StockSense AI"),
    p("Real-Time Inventory Demand Forecasting and Stock-Out Prediction"),
    p("Inventory Intelligence | Demand Planning | Replenishment Decisions")
  ),
  
  sidebarLayout(
    
    # --------------------------------------------------------
    # SIDEBAR
    # --------------------------------------------------------
    
    sidebarPanel(
      width = 3,
      
      div(
        class = "sidebar-panel",
        
        h4("⚙️ Dashboard Controls"),
        
        selectInput(
          "category_filter",
          "Select Category",
          choices = "All Categories"
        ),
        
        selectInput(
          "product_filter",
          "Select Product",
          choices = "All Products"
        ),
        
        actionButton(
          "refresh_data",
          "Refresh Dashboard",
          class = "btn-primary",
          icon = icon("refresh")
        ),
        
        tags$hr(),
        
        h4("📌 Risk Guide"),
        
        p("🔴 High Risk: Potential stock shortage soon."),
        p("🟠 Reorder Required: Stock needs attention."),
        p("🟢 Lower Risk: Current stock appears sufficient."),
        
        tags$hr(),
        
        p(
          class = "small-note",
          "Risk estimates depend on available sales history, inventory levels and supplier lead times."
        )
      )
    ),
    
    # --------------------------------------------------------
    # MAIN CONTENT
    # --------------------------------------------------------
    
    mainPanel(
      width = 9,
      
      uiOutput("kpi_cards"),
      
      tabsetPanel(
        
        # ====================================================
        # TAB 1: OVERVIEW
        # ====================================================
        
        tabPanel(
          "🏠 Overview",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Inventory Overview"),
            
            div(
              class = "table-scroll",
              tableOutput("inventory_table")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Selected Product Details"),
            
            div(
              class = "table-scroll",
              tableOutput("product_details")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Stock-Out Prediction Summary"),
            
            # FIX: the table is contained inside this scrollable box
            div(
              class = "table-scroll",
              tableOutput("prediction_summary")
            )
          )
        ),
        
        # ====================================================
        # TAB 2: DEMAND ANALYSIS
        # ====================================================
        
        tabPanel(
          "📊 Demand Analysis",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Daily Demand Trend"),
            plotOutput("daily_demand_plot", height = "320px")
          ),
          
          div(
            class = "section-panel",
            h4("Product Demand Comparison"),
            plotOutput("product_demand_plot", height = "320px")
          ),
          
          div(
            class = "section-panel",
            h4("Category Demand"),
            
            div(
              class = "table-scroll",
              tableOutput("category_table")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Monthly Demand Summary"),
            
            div(
              class = "table-scroll",
              tableOutput("monthly_table")
            )
          )
        ),
        
        # ====================================================
        # TAB 3: DEMAND FORECAST
        # ====================================================
        
        tabPanel(
          "🔮 Demand Forecast",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Forecasted Demand"),
            plotOutput("forecast_plot", height = "350px")
          ),
          
          div(
            class = "section-panel",
            h4("Forecast Data"),
            
            div(
              class = "table-scroll",
              tableOutput("forecast_table")
            )
          )
        ),
        
        # ====================================================
        # TAB 4: STOCK-OUT ALERTS
        # ====================================================
        
        tabPanel(
          "🚨 Stock-Out Alerts",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Products Requiring Attention"),
            
            div(
              class = "table-scroll",
              tableOutput("alerts_table")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Stock-Out Risk Details"),
            
            div(
              class = "table-scroll",
              tableOutput("risk_table")
            )
          )
        ),
        
        # ====================================================
        # TAB 5: REPLENISHMENT
        # ====================================================
        
        tabPanel(
          "🛒 Replenishment",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Recommended Replenishment"),
            
            div(
              class = "table-scroll",
              tableOutput("replenishment_table")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Download Reports"),
            
            p("Download the filtered data for reporting and further analysis."),
            
            downloadButton(
              "download_inventory",
              "Download Inventory"
            ),
            
            downloadButton(
              "download_alerts",
              "Download Alerts"
            ),
            
            downloadButton(
              "download_forecast",
              "Download Forecast"
            ),
            
            downloadButton(
              "download_sales",
              "Download Sales"
            ),
            
            downloadButton(
              "download_evaluation",
              "Download Model Results"
            ),
            
            downloadButton(
              "download_summary",
              "Download Project Summary"
            )
          )
        ),
        
        # ====================================================
        # TAB 6: MODEL EVALUATION
        # ====================================================
        
        tabPanel(
          "🧠 Model Evaluation",
          
          br(),
          
          div(
            class = "section-panel",
            h4("Forecast Model Evaluation"),
            plotOutput("evaluation_plot", height = "320px")
          ),
          
          div(
            class = "section-panel",
            h4("Model Evaluation Results"),
            
            div(
              class = "table-scroll",
              tableOutput("evaluation_table")
            )
          ),
          
          div(
            class = "section-panel",
            h4("Understanding the Metrics"),
            
            p("MAE: Mean Absolute Error. Lower values generally indicate smaller average forecast errors."),
            
            p("RMSE: Root Mean Squared Error. Lower values generally indicate smaller forecast errors, with larger errors receiving more weight."),
            
            p("MAPE: Mean Absolute Percentage Error. Interpret cautiously when actual demand contains zeros or very small values."),
            
            p("Compare models only when they are evaluated using comparable data and test periods.")
          )
        ),
        
        # ====================================================
        # TAB 7: ABOUT PROJECT
        # ====================================================
        
        tabPanel(
          "ℹ️ About Project",
          
          br(),
          
          div(
            class = "section-panel",
            
            h4("About StockSense AI"),
            
            p(
              "StockSense AI is an inventory analytics application designed to analyze historical product demand, review demand forecasts, identify possible stock-outs and support replenishment decisions."
            ),
            
            h4("Project Objectives"),
            
            tags$ul(
              tags$li("Analyze historical product demand."),
              tags$li("Estimate future demand using the project's forecasting workflow."),
              tags$li("Identify products that may run out of stock."),
              tags$li("Estimate days of stock remaining."),
              tags$li("Compare expected lead-time demand with available inventory."),
              tags$li("Recommend potential replenishment quantities."),
              tags$li("Present inventory insights through an interactive dashboard.")
            ),
            
            h4("Technology Used"),
            
            tags$ul(
              tags$li("R programming language"),
              tags$li("RStudio"),
              tags$li("Shiny"),
              tags$li("CSV data files"),
              tags$li("Base R graphics")
            ),
            
            h4("Important Note"),
            
            p(
              "This application is a decision-support prototype. Forecasts, stock-out dates and order quantities are estimates, not guarantees. Verify actual stock, demand, supplier lead times and business requirements before placing an order."
            )
          )
        )
      ),
      
      div(
        class = "footer-text",
        "StockSense AI | Inventory Intelligence Dashboard"
      )
    )
  )
)

# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  project_data <- reactiveVal(load_all_data())
  
  # ----------------------------------------------------------
  # REFRESH DATA
  # ----------------------------------------------------------
  
  observeEvent(input$refresh_data, {
    project_data(load_all_data())
    
    showNotification(
      "Dashboard data refreshed successfully.",
      type = "message"
    )
  })
  
  # ----------------------------------------------------------
  # CATEGORY FILTER
  # ----------------------------------------------------------
  
  observeEvent(project_data(), {
    d <- project_data()
    inv <- d$inventory
    
    category_col <- find_col(
      inv, c("category", "product_category")
    )
    
    categories <- character()
    
    if (!is.null(category_col) && nrow(inv) > 0) {
      categories <- sort(unique(
        as.character(inv[[category_col]])
      ))
      
      categories <- categories[
        !is.na(categories) & nzchar(categories)
      ]
    }
    
    updateSelectInput(
      session,
      "category_filter",
      choices = c("All Categories", categories),
      selected = "All Categories"
    )
  }, ignoreInit = FALSE)
  
  # ----------------------------------------------------------
  # PRODUCT FILTER
  # ----------------------------------------------------------
  
  observe({
    d <- project_data()
    inv <- d$inventory
    
    id_col <- find_col(
      inv, c("product_id", "productid", "sku")
    )
    
    name_col <- find_col(
      inv, c("product_name", "product", "name")
    )
    
    category_col <- find_col(
      inv, c("category", "product_category")
    )
    
    if (nrow(inv) == 0 || is.null(id_col)) {
      updateSelectInput(
        session,
        "product_filter",
        choices = "All Products",
        selected = "All Products"
      )
      return()
    }
    
    available <- inv
    
    if (!is.null(category_col) &&
        !is.null(input$category_filter) &&
        input$category_filter != "All Categories") {
      
      available <- available[
        as.character(available[[category_col]]) ==
          input$category_filter,
        ,
        drop = FALSE
      ]
    }
    
    ids <- as.character(available[[id_col]])
    
    if (!is.null(name_col)) {
      labels <- paste0(
        as.character(available[[name_col]]),
        " (", ids, ")"
      )
    } else {
      labels <- ids
    }
    
    choices <- c(
      "All Products",
      stats::setNames(ids, labels)
    )
    
    current <- input$product_filter
    
    if (is.null(current) || !current %in% choices) {
      current <- "All Products"
    }
    
    updateSelectInput(
      session,
      "product_filter",
      choices = choices,
      selected = current
    )
  })
  
  # ----------------------------------------------------------
  # FILTERED DATASETS
  # ----------------------------------------------------------
  
  filtered_inventory <- reactive({
    d <- project_data()
    
    filter_data(
      d$inventory,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_stockout <- reactive({
    d <- project_data()
    
    filter_data(
      d$stockout,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_sales <- reactive({
    d <- project_data()
    
    filter_data(
      d$sales,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_forecast <- reactive({
    d <- project_data()
    
    filter_data(
      d$forecast,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_product_analysis <- reactive({
    d <- project_data()
    
    filter_data(
      d$product_analysis,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_daily <- reactive({
    d <- project_data()
    
    filter_data(
      d$daily_demand,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_monthly <- reactive({
    d <- project_data()
    
    filter_data(
      d$monthly_demand,
      input$product_filter,
      input$category_filter,
      d$inventory
    )
  })
  
  filtered_category <- reactive({
    d <- project_data()
    d$category_demand
  })
  
  filtered_evaluation <- reactive({
    project_data()$evaluation
  })
  
  # ----------------------------------------------------------
  # KPI CARDS
  # ----------------------------------------------------------
  
  output$kpi_cards <- renderUI({
    
    inv <- filtered_inventory()
    pred <- filtered_stockout()
    
    risk_col <- risk_column(pred)
    status_col <- status_column(pred)
    
    high_risk <- 0
    lower_risk <- 0
    reorder_count <- 0
    
    if (!is.null(risk_col) && nrow(pred) > 0) {
      risk <- tolower(as.character(pred[[risk_col]]))
      
      high_risk <- sum(
        grepl("high|critical", risk),
        na.rm = TRUE
      )
      
      lower_risk <- sum(
        grepl("lower|low", risk),
        na.rm = TRUE
      )
    }
    
    if (!is.null(status_col) && nrow(pred) > 0) {
      status <- tolower(as.character(pred[[status_col]]))
      
      reorder_count <- sum(
        grepl("reorder|required|low stock", status),
        na.rm = TRUE
      )
    }
    
    div(
      class = "kpi-row",
      
      div(
        class = "kpi-card blue",
        div(class = "kpi-label", "TOTAL PRODUCTS"),
        div(class = "kpi-value", nrow(inv))
      ),
      
      div(
        class = "kpi-card red",
        div(class = "kpi-label", "HIGH-RISK PRODUCTS"),
        div(class = "kpi-value", high_risk)
      ),
      
      div(
        class = "kpi-card orange",
        div(class = "kpi-label", "PRODUCTS TO REORDER"),
        div(class = "kpi-value", reorder_count)
      ),
      
      div(
        class = "kpi-card green",
        div(class = "kpi-label", "LOWER-RISK PRODUCTS"),
        div(class = "kpi-value", lower_risk)
      )
    )
  })
  
  # ----------------------------------------------------------
  # INVENTORY TABLE
  # ----------------------------------------------------------
  
  output$inventory_table <- renderTable({
    df <- filtered_inventory()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No inventory data available"))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # SELECTED PRODUCT DETAILS
  # ----------------------------------------------------------
  
  output$product_details <- renderTable({
    df <- filtered_stockout()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No prediction data available"))
    }
    
    if (input$product_filter == "All Products") {
      return(data.frame(
        Message = "Select one product in the sidebar to view its details."
      ))
    }
    
    row <- df[1, , drop = FALSE]
    
    wanted <- c(
      "product_id", "product_name", "category",
      "current_stock", "lead_time_days", "reorder_level",
      "average_daily_demand", "next_7_days_demand",
      "days_of_stock", "expected_lead_time_demand",
      "stockout_risk", "stock_status",
      "estimated_stockout_date",
      "estimated_days_until_stockout",
      "recommended_order"
    )
    
    actual <- names(row)[tolower(names(row)) %in% wanted]
    
    if (length(actual) == 0) return(row)
    
    data.frame(
      Field = actual,
      Value = vapply(
        actual,
        function(nm) as.character(row[[nm]][1]),
        character(1)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # STOCK-OUT PREDICTION SUMMARY
  # ----------------------------------------------------------
  
  output$prediction_summary <- renderTable({
    df <- filtered_stockout()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No stock-out predictions available"))
    }
    
    wanted <- c(
      "product_id",
      "product_name",
      "current_stock",
      "average_daily_demand",
      "days_of_stock",
      "stockout_risk",
      "stock_status",
      "estimated_stockout_date",
      "recommended_order"
    )
    
    actual <- names(df)[tolower(names(df)) %in% wanted]
    
    if (length(actual) == 0) return(df)
    
    df[, actual, drop = FALSE]
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # DAILY DEMAND PLOT
  # ----------------------------------------------------------
  
  output$daily_demand_plot <- renderPlot({
    
    df <- filtered_daily()
    
    if (nrow(df) == 0) {
      df <- filtered_sales()
    }
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No daily demand data available")
      return()
    }
    
    demand_col <- find_col(
      df, c("total_demand", "daily_demand",
            "quantity_sold", "demand",
            "sales_quantity", "quantity")
    )
    
    date_col <- find_col(
      df, c("date", "sale_date", "order_date")
    )
    
    if (is.null(demand_col)) {
      numeric_cols <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      if (length(numeric_cols) > 0) {
        demand_col <- numeric_cols[1]
      }
    }
    
    if (is.null(demand_col)) {
      plot.new()
      text(0.5, 0.5, "Demand column not found")
      return()
    }
    
    y <- to_number(df[[demand_col]])
    valid <- is.finite(y)
    y <- y[valid]
    
    if (length(y) == 0) {
      plot.new()
      text(0.5, 0.5, "No numeric demand values")
      return()
    }
    
    x <- seq_along(y)
    
    plot(
      x, y,
      type = "l",
      lwd = 3,
      col = "#178ca4",
      main = "Demand Over Time",
      xlab = "Observation",
      ylab = demand_col
    )
    
    grid()
  })
  
  # ----------------------------------------------------------
  # PRODUCT DEMAND COMPARISON
  # ----------------------------------------------------------
  
  output$product_demand_plot <- renderPlot({
    
    df <- filtered_product_analysis()
    
    if (nrow(df) == 0) {
      df <- filtered_category()
    }
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No demand analysis available")
      return()
    }
    
    label_col <- find_col(
      df, c("product_name", "product", "category", "product_id")
    )
    
    value_col <- find_col(
      df, c("average_daily_demand", "total_demand",
            "total_quantity", "quantity_sold", "demand",
            "average_demand")
    )
    
    if (is.null(value_col)) {
      numeric_cols <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      if (length(numeric_cols) > 0) {
        value_col <- numeric_cols[1]
      }
    }
    
    if (is.null(value_col)) {
      plot.new()
      text(0.5, 0.5, "Demand measure not found")
      return()
    }
    
    values <- to_number(df[[value_col]])
    
    labels <- if (is.null(label_col)) {
      paste("Item", seq_len(nrow(df)))
    } else {
      as.character(df[[label_col]])
    }
    
    valid <- is.finite(values)
    values <- values[valid]
    labels <- labels[valid]
    
    if (length(values) == 0) {
      plot.new()
      text(0.5, 0.5, "No numeric demand values")
      return()
    }
    
    ord <- order(values, decreasing = TRUE)
    values <- values[ord]
    labels <- labels[ord]
    
    barplot(
      values,
      names.arg = labels,
      las = 2,
      col = "#2596be",
      border = NA,
      main = "Demand Comparison",
      ylab = value_col,
      cex.names = 0.8
    )
    
    grid(nx = NA, ny = NULL)
  })
  
  # ----------------------------------------------------------
  # CATEGORY AND MONTHLY TABLES
  # ----------------------------------------------------------
  
  output$category_table <- renderTable({
    df <- filtered_category()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No category data available"))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$monthly_table <- renderTable({
    df <- filtered_monthly()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No monthly data available"))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # DEMAND FORECAST PLOT
  # ----------------------------------------------------------
  
  output$forecast_plot <- renderPlot({
    
    df <- filtered_forecast()
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No forecast data available")
      return()
    }
    
    predicted_col <- find_col(
      df, c("predicted_demand", "forecast_demand",
            "forecast", "forecasted_demand",
            "prediction", "predicted_quantity", "yhat")
    )
    
    actual_col <- find_col(
      df, c("actual_demand", "actual", "actual_quantity")
    )
    
    if (is.null(predicted_col)) {
      numeric_cols <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      if (length(numeric_cols) > 0) {
        predicted_col <- numeric_cols[1]
      }
    }
    
    if (is.null(predicted_col)) {
      plot.new()
      text(0.5, 0.5, "Forecast column not found")
      return()
    }
    
    predicted <- to_number(df[[predicted_col]])
    valid <- is.finite(predicted)
    
    if (!any(valid)) {
      plot.new()
      text(0.5, 0.5, "No numeric forecast values")
      return()
    }
    
    predicted <- predicted[valid]
    x <- seq_along(predicted)
    
    plot(
      x, predicted,
      type = "b",
      pch = 19,
      lwd = 2,
      col = "#176b87",
      main = "Demand Forecast",
      xlab = "Forecast Observation",
      ylab = "Demand"
    )
    
    if (!is.null(actual_col)) {
      actual <- to_number(df[[actual_col]][valid])
      
      if (length(actual) == length(predicted) &&
          any(is.finite(actual))) {
        
        lines(
          x, actual,
          type = "b",
          pch = 17,
          lty = 2,
          col = "#e67e22"
        )
        
        legend(
          "topleft",
          legend = c("Forecast", "Actual"),
          col = c("#176b87", "#e67e22"),
          lty = c(1, 2),
          pch = c(19, 17),
          bty = "n"
        )
      }
    }
    
    grid()
  })
  
  output$forecast_table <- renderTable({
    df <- filtered_forecast()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No forecast data available"))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # STOCK-OUT ALERTS
  # ----------------------------------------------------------
  
  output$alerts_table <- renderTable({
    df <- get_alerts(filtered_stockout())
    
    if (nrow(df) == 0) {
      return(data.frame(
        Message = "No high-risk or reorder-required products found"
      ))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$risk_table <- renderTable({
    df <- filtered_stockout()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No risk data available"))
    }
    
    wanted <- c(
      "product_id", "product_name", "current_stock",
      "lead_time_days", "average_daily_demand",
      "expected_lead_time_demand", "days_of_stock",
      "estimated_stockout_date",
      "estimated_days_until_stockout",
      "stockout_risk", "stock_status"
    )
    
    actual <- names(df)[tolower(names(df)) %in% wanted]
    
    if (length(actual) == 0) return(df)
    
    df[, actual, drop = FALSE]
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # REPLENISHMENT
  # ----------------------------------------------------------
  
  output$replenishment_table <- renderTable({
    df <- filtered_stockout()
    
    if (nrow(df) == 0) {
      return(data.frame(Message = "No replenishment data available"))
    }
    
    wanted <- c(
      "product_id", "product_name", "category",
      "current_stock", "reorder_level",
      "average_daily_demand", "expected_lead_time_demand",
      "stock_status", "stockout_risk", "recommended_order"
    )
    
    actual <- names(df)[tolower(names(df)) %in% wanted]
    
    if (length(actual) == 0) return(df)
    
    df <- df[, actual, drop = FALSE]
    
    order_col <- find_col(df, c("recommended_order"))
    
    if (!is.null(order_col)) {
      order_values <- to_number(df[[order_col]])
      
      df <- df[
        order(order_values, decreasing = TRUE, na.last = TRUE),
        ,
        drop = FALSE
      ]
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # MODEL EVALUATION
  # ----------------------------------------------------------
  
  output$evaluation_table <- renderTable({
    df <- filtered_evaluation()
    
    if (nrow(df) == 0) {
      return(data.frame(
        Message = "No model evaluation data available"
      ))
    }
    
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$evaluation_plot <- renderPlot({
    
    df <- filtered_evaluation()
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No model evaluation data available")
      return()
    }
    
    model_col <- find_col(
      df, c("model", "model_name", "algorithm", "method")
    )
    
    metric_col <- find_col(
      df, c("rmse", "mae", "mape", "accuracy")
    )
    
    if (is.null(metric_col)) {
      numeric_cols <- names(df)[
        vapply(df, is.numeric, logical(1))
      ]
      
      if (length(numeric_cols) > 0) {
        metric_col <- numeric_cols[1]
      }
    }
    
    if (is.null(metric_col)) {
      plot.new()
      text(0.5, 0.5, "No numeric evaluation metric available")
      return()
    }
    
    values <- to_number(df[[metric_col]])
    valid <- is.finite(values)
    
    if (!any(valid)) {
      plot.new()
      text(0.5, 0.5, "No numeric evaluation values")
      return()
    }
    
    labels <- if (is.null(model_col)) {
      paste("Model", seq_len(nrow(df)))
    } else {
      as.character(df[[model_col]])
    }
    
    barplot(
      values[valid],
      names.arg = labels[valid],
      las = 2,
      col = "#159957",
      border = NA,
      main = paste("Model Comparison:", metric_col),
      ylab = metric_col,
      cex.names = 0.8
    )
    
    grid(nx = NA, ny = NULL)
  })
  
  # ----------------------------------------------------------
  # DOWNLOAD INVENTORY
  # ----------------------------------------------------------
  
  output$download_inventory <- downloadHandler(
    filename = function() {
      paste0("StockSense_Inventory_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        filtered_inventory(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD STOCK-OUT ALERTS
  # ----------------------------------------------------------
  
  output$download_alerts <- downloadHandler(
    filename = function() {
      paste0("StockSense_Stockout_Alerts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        get_alerts(filtered_stockout()),
        file,
        row.names = FALSE
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD FORECAST
  # ----------------------------------------------------------
  
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("StockSense_Demand_Forecast_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        filtered_forecast(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD SALES
  # ----------------------------------------------------------
  
  output$download_sales <- downloadHandler(
    filename = function() {
      paste0("StockSense_Cleaned_Sales_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        filtered_sales(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD MODEL EVALUATION
  # ----------------------------------------------------------
  
  output$download_evaluation <- downloadHandler(
    filename = function() {
      paste0("StockSense_Model_Evaluation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        filtered_evaluation(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # ----------------------------------------------------------
  # DOWNLOAD PROJECT SUMMARY
  # ----------------------------------------------------------
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("StockSense_Project_Summary_", Sys.Date(), ".txt")
    },
    content = function(file) {
      
      inv <- filtered_inventory()
      pred <- filtered_stockout()
      fc <- filtered_forecast()
      
      risk_col <- risk_column(pred)
      status_col <- status_column(pred)
      
      high_risk <- 0
      reorder_count <- 0
      
      if (!is.null(risk_col)) {
        high_risk <- sum(
          grepl(
            "high|critical",
            tolower(as.character(pred[[risk_col]]))
          ),
          na.rm = TRUE
        )
      }
      
      if (!is.null(status_col)) {
        reorder_count <- sum(
          grepl(
            "reorder|required|low stock",
            tolower(as.character(pred[[status_col]]))
          ),
          na.rm = TRUE
        )
      }
      
      report_lines <- c(
        "STOCKSENSE AI",
        "Inventory Demand Forecasting and Stock-Out Prediction",
        "======================================================",
        paste("Report generated:", Sys.time()),
        paste("Selected category:", input$category_filter),
        paste("Selected product:", input$product_filter),
        "",
        "PROJECT SUMMARY",
        "----------------",
        paste("Products in current selection:", nrow(inv)),
        paste("Products with predictions:", nrow(pred)),
        paste("High-risk products:", high_risk),
        paste("Products requiring reorder:", reorder_count),
        paste("Forecast records available:", nrow(fc)),
        "",
        "PROJECT DATA FILES",
        "------------------",
        "Inventory: data/cleaned_inventory.csv",
        "Sales: data/cleaned_sales.csv",
        "Stock-out predictions: data/stockout_predictions.csv",
        "Demand forecast: demand_forecast.csv or demand_forecast_improved.csv",
        "Model evaluation: model_evaluation.csv or model_evaluation_improved.csv",
        "",
        "IMPORTANT",
        "Forecasts and stock-out estimates are decision-support outputs.",
        "Verify actual inventory, demand and supplier lead times before ordering."
      )
      
      writeLines(report_lines, file)
    }
  )
}

# ============================================================
# 5. START APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
