# ==============================================================================
# File: app.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Interactive R Shiny Web Application featuring descriptive statistics,
#              conditional probability theory, data visualization,
#              and binary logistic regression prediction.
#              Designed with clear, intuitive English explanations for college labs.
# ==============================================================================

# Suppress sass cache warnings in restricted sandbox/user environments
options(sass.cache = FALSE)

# Ensure local project library path is registered
if (dir.exists("r_libs")) {
  .libPaths(c(file.path(getwd(), "r_libs"), .libPaths()))
}

library(shiny)
library(bslib)
library(ggplot2)
library(scales)

# Source modular components
source("R/data_cleaning.R")
source("R/statistics.R")
source("R/probability.R")
source("R/visualization.R")
source("R/prediction.R")

# Declare modular functions for static code analyzers
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "plot_churn_distribution", "plot_tenure_distribution",
    "calc_numerical_summary_table", "calc_grouped_descriptive_stats",
    "calc_frequency_table", "calc_contingency_table",
    "get_key_conditional_probabilities", "calc_conditional_probability",
    "calc_churn_by_categorical_probability",
    "plot_monthly_charges_distribution", "plot_churn_vs_categorical",
    "plot_boxplot_by_churn", "plot_correlation_matrix",
    "plot_roc_curve", "predict_individual_customer"
  ))
}

# ------------------------------------------------------------------------------
# 1. Global Setup & Model Training
# ------------------------------------------------------------------------------
raw_dataset <- load_raw_data("data/customer_churn.csv")
churn_df <- clean_churn_data(raw_dataset)

# 80/20 train/test split and logistic regression fitting
data_splits <- split_data(churn_df, train_ratio = 0.80, seed = 42)
churn_model <- train_logistic_regression(data_splits$train)
model_eval <- evaluate_model(churn_model, data_splits$test)
model_coefs <- get_model_coefficients(churn_model)

# Precompute individual predicted churn probability for all customers
churn_df$PredictedProb <- round(predict(churn_model, newdata = churn_df, type = "response") * 100, 1)
churn_df$PredictedRiskTier <- ifelse(churn_df$PredictedProb >= 70, "Very High Risk",
                               ifelse(churn_df$PredictedProb >= 50, "High Risk",
                               ifelse(churn_df$PredictedProb >= 30, "Moderate Risk", "Low Risk")))

# Precompute baseline metrics for Dashboard KPI cards
n_total <- nrow(churn_df)
n_churned <- sum(churn_df$Churn == "Yes")
pct_churn <- (n_churned / n_total) * 100
avg_monthly <- mean(churn_df$MonthlyCharges)
median_tenure <- median(churn_df$tenure)

# Helper function to format probability tables with clear, simple column names
format_prob_table <- function(tbl) {
  names(tbl) <- c(
    "Customer Segment", "Total Customers", "Customers Left",
    "Customers Stayed", "Probability (0 to 1)", "Churn Rate (%)", "Risk Multiplier"
  )
  tbl
}

# ------------------------------------------------------------------------------
# 2. Modern UI Definition
# ------------------------------------------------------------------------------
ui <- bslib::page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$span(class = "brand-badge-icon", icon("chart-line")),
    tags$span(style = "font-weight: 700; font-size: 1.15rem; color: #0f172a;", "Predicting Customer Churn"),
    tags$span(style = "font-size: 0.72rem; background-color: #eff6ff; color: #2563eb; border: 1px solid #bfdbfe; padding: 2px 8px; border-radius: 9999px; font-weight: 600;", "Applied Statistics & ML")
  ),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#2563eb",
    font_scale = 0.95
  ),
  id = "main_nav",

  # Custom CSS for polished, professional SaaS aesthetic without overdoing it
  header = tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');

      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif !important;
        background-color: #f8fafc !important;
        color: #1e293b !important;
      }

      /* Navbar enhancements */
      .navbar {
        border-bottom: 1px solid #e2e8f0 !important;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.03) !important;
        padding: 0.6rem 1.25rem !important;
        background: #ffffff !important;
      }
      .brand-badge-icon {
        width: 30px;
        height: 30px;
        border-radius: 8px;
        background: linear-gradient(135deg, #2563eb, #1d4ed8);
        color: white;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-size: 0.9rem;
      }
      .nav-link {
        font-weight: 500 !important;
        color: #64748b !important;
        padding: 0.5rem 0.85rem !important;
        border-radius: 8px !important;
        transition: all 0.15s ease !important;
      }
      .nav-link:hover {
        color: #1e293b !important;
        background-color: #f1f5f9 !important;
      }
      .nav-link.active {
        color: #2563eb !important;
        background-color: #eff6ff !important;
        font-weight: 600 !important;
      }

      /* Modern Cards */
      .card {
        border: 1px solid #e2e8f0 !important;
        border-radius: 12px !important;
        box-shadow: 0 1px 3px rgba(15, 23, 42, 0.04), 0 1px 2px rgba(15, 23, 42, 0.02) !important;
        background-color: #ffffff !important;
        margin-bottom: 1.25rem !important;
      }
      .card-header {
        background-color: #ffffff !important;
        border-bottom: 1px solid #f1f5f9 !important;
        font-weight: 600 !important;
        font-size: 0.92rem !important;
        color: #1e293b !important;
        padding: 12px 18px !important;
        border-top-left-radius: 12px !important;
        border-top-right-radius: 12px !important;
      }

      /* Executive KPI Metric Cards */
      .kpi-card {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        padding: 18px 20px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(15, 23, 42, 0.04);
        position: relative;
        overflow: hidden;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      .kpi-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 14px rgba(15, 23, 42, 0.07);
      }
      .kpi-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
      }
      .kpi-card-blue::before   { background: #2563eb; }
      .kpi-card-red::before    { background: #ef4444; }
      .kpi-card-purple::before { background: #8b5cf6; }
      .kpi-card-green::before  { background: #10b981; }

      .kpi-top-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 6px;
      }
      .kpi-label {
        font-size: 0.75rem;
        font-weight: 700;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        color: #64748b;
      }
      .kpi-icon-pill {
        width: 34px;
        height: 34px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 0.95rem;
      }
      .icon-pill-blue   { background-color: #eff6ff; color: #2563eb; }
      .icon-pill-red    { background-color: #fef2f2; color: #ef4444; }
      .icon-pill-purple { background-color: #faf5ff; color: #8b5cf6; }
      .icon-pill-green  { background-color: #f0fdf4; color: #10b981; }

      .kpi-val {
        font-size: 2.1rem;
        font-weight: 800;
        color: #0f172a;
        letter-spacing: -0.02em;
        margin: 4px 0;
        line-height: 1.15;
      }
      .kpi-subtext {
        font-size: 0.84rem;
        color: #64748b;
        display: flex;
        align-items: center;
        gap: 6px;
        margin: 0;
      }
      .kpi-indicator-dot {
        width: 7px;
        height: 7px;
        border-radius: 50%;
        display: inline-block;
      }
      .dot-blue   { background-color: #2563eb; }
      .dot-red    { background-color: #ef4444; }
      .dot-purple { background-color: #8b5cf6; }
      .dot-green  { background-color: #10b981; }

      /* Dossier Mini Stat Cards */
      .dossier-stat-card {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 10px;
        padding: 14px 10px;
        text-align: center;
        transition: all 0.2s ease;
      }
      .dossier-stat-card:hover {
        background: #ffffff;
        border-color: #cbd5e1;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
      }
      .dossier-stat-num {
        margin: 0;
        font-weight: 800;
        font-size: 1.35rem;
        letter-spacing: -0.01em;
      }
      .dossier-stat-lbl {
        margin: 4px 0 0 0;
        font-size: 0.72rem;
        text-transform: uppercase;
        color: #64748b;
        font-weight: 700;
        letter-spacing: 0.05em;
      }

      /* Status Badges */
      .stat-badge {
        font-size: 0.88rem;
        font-weight: 600;
        padding: 6px 14px;
        border-radius: 20px;
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .badge-churn {
        background-color: #fef2f2;
        color: #b91c1c;
        border: 1px solid #fecaca;
      }
      .badge-stay {
        background-color: #f0fdf4;
        color: #15803d;
        border: 1px solid #bbf7d0;
      }

      /* Risk Multiplier Badges */
      .badge-risk-pill {
        font-size: 0.78rem;
        font-weight: 600;
        padding: 3px 10px;
        border-radius: 9999px;
      }
      .badge-risk-high { background-color: #fee2e2; color: #991b1b; border: 1px solid #fca5a5; }
      .badge-risk-med  { background-color: #fef3c7; color: #92400e; border: 1px solid #fcd34d; }
      .badge-risk-low  { background-color: #dcfce7; color: #166534; border: 1px solid #86efac; }

      /* Progress Bar */
      .custom-progress {
        height: 22px;
        border-radius: 9999px;
        background-color: #e2e8f0;
        overflow: hidden;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
      }
      .progress-bar-fill {
        height: 100%;
        line-height: 22px;
        color: white;
        text-align: center;
        font-weight: 600;
        font-size: 0.8rem;
        transition: width 0.4s ease;
        border-radius: 9999px;
      }

      /* Tables & Scrollable Responsive Wrappers */
      .table-responsive {
        border: 1px solid #e2e8f0;
        border-radius: 10px;
        overflow-x: auto;
        box-shadow: 0 1px 2px rgba(0,0,0,0.02);
        margin-bottom: 10px;
      }
      .table {
        color: #1e293b !important;
        vertical-align: middle !important;
        margin-bottom: 0 !important;
        font-size: 0.88rem !important;
      }
      .table thead th {
        background-color: #f8fafc !important;
        color: #475569 !important;
        font-weight: 700 !important;
        font-size: 0.78rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.04em !important;
        border-bottom: 1.5px solid #e2e8f0 !important;
        padding: 10px 14px !important;
        white-space: nowrap !important;
      }
      .table tbody td {
        padding: 9px 14px !important;
        border-bottom: 1px solid #f1f5f9 !important;
      }
      .table-striped tbody tr:nth-of-type(odd) {
        background-color: #fcfdfe !important;
      }
      .table-hover tbody tr:hover {
        background-color: #f8fafc !important;
      }

      /* Lab Formula Card */
      .lab-formula {
        background-color: #f8fafc;
        border-left: 4px solid #2563eb;
        padding: 14px 18px;
        font-family: inherit;
        margin: 12px 0;
        border-radius: 0 8px 8px 0;
        font-size: 0.92rem;
        line-height: 1.6;
      }

      /* Form controls */
      .form-control, .form-select, .selectize-input {
        border-radius: 8px !important;
        border: 1px solid #cbd5e1 !important;
        font-size: 0.9rem !important;
      }
      .form-control:focus, .form-select:focus, .selectize-input.focus {
        border-color: #2563eb !important;
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.15) !important;
      }
      .btn-primary {
        background-color: #2563eb !important;
        border-color: #2563eb !important;
        border-radius: 8px !important;
        font-weight: 600 !important;
        transition: all 0.15s ease !important;
      }
      .btn-primary:hover {
        background-color: #1d4ed8 !important;
        border-color: #1d4ed8 !important;
        box-shadow: 0 2px 6px rgba(37, 99, 235, 0.25) !important;
      }
      .btn-outline-primary {
        border-radius: 8px !important;
        font-weight: 600 !important;
      }
    "))
  ),

  # ----------------------------------------------------------------------------
  # Tab 1: Executive Dashboard
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Dashboard",
    icon = icon("tachometer-alt"),
    fluidRow(
      column(
        width = 3,
        div(
          class = "kpi-card kpi-card-blue",
          div(
            class = "kpi-top-row",
            span(class = "kpi-label", "Total Sample"),
            div(class = "kpi-icon-pill icon-pill-blue", icon("users"))
          ),
          div(class = "kpi-val", format(n_total, big.mark = ",")),
          p(class = "kpi-subtext", span(class = "kpi-indicator-dot dot-blue"), "Complete Customer Records")
        )
      ),
      column(
        width = 3,
        div(
          class = "kpi-card kpi-card-red",
          div(
            class = "kpi-top-row",
            span(class = "kpi-label", "Customers Who Left"),
            div(class = "kpi-icon-pill icon-pill-red", icon("user-minus"))
          ),
          div(class = "kpi-val", format(n_churned, big.mark = ",")),
          p(class = "kpi-subtext", span(class = "kpi-indicator-dot dot-red"), sprintf("Total Churned (n = %d)", n_churned))
        )
      ),
      column(
        width = 3,
        div(
          class = "kpi-card kpi-card-purple",
          div(
            class = "kpi-top-row",
            span(class = "kpi-label", "Overall Churn Rate"),
            div(class = "kpi-icon-pill icon-pill-purple", icon("chart-pie"))
          ),
          div(class = "kpi-val", sprintf("%.2f%%", pct_churn)),
          p(class = "kpi-subtext", span(class = "kpi-indicator-dot dot-purple"), "Baseline Customer Attrition")
        )
      ),
      column(
        width = 3,
        div(
          class = "kpi-card kpi-card-green",
          div(
            class = "kpi-top-row",
            span(class = "kpi-label", "Average Monthly Bill"),
            div(class = "kpi-icon-pill icon-pill-green", icon("dollar-sign"))
          ),
          div(class = "kpi-val", sprintf("$%.2f", avg_monthly)),
          p(class = "kpi-subtext", span(class = "kpi-indicator-dot dot-green"), sprintf("Typical Tenure: %.0f Months", median_tenure))
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bslib::card(
          bslib::card_header(div(icon("chart-bar", class = "me-2 text-primary"), "Customer Outcome Breakdown (Stayed vs Left)")),
          plotOutput("dash_plot_churn", height = "320px")
        )
      ),
      column(
        width = 6,
        bslib::card(
          bslib::card_header(div(icon("hourglass-half", class = "me-2 text-primary"), "How Long Customers Stay (Tenure in Months)")),
          plotOutput("dash_plot_tenure", height = "320px")
        )
      )
    )
  ),

  # ----------------------------------------------------------------------------
  # Tab 2: Individual Customers Explorer
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Individual Customers",
    icon = icon("id-card"),
    fluidRow(
      # Left column: Customer Lookup & Selection
      column(
        width = 4,
        bslib::card(
          bslib::card_header(div(icon("search", class = "me-2 text-primary"), "Search Customer")),
          selectizeInput(
            "indiv_cust_select",
            label = "Find Customer by Name or ID:",
            choices = NULL,
            options = list(
              placeholder = "Type name (e.g. James Price) or ID...",
              maxOptions = 50
            )
          ),
          actionButton("btn_random_cust", "Pick Random Customer", icon = icon("dice"), class = "btn-outline-primary w-100 mb-3"),
          p(
            style = "font-size: 0.88rem; color: #64748b;",
            "Search across all 7,032 customers in the database. Selecting any customer loads their complete profile, displays whether they stayed or left, and inspects their individual churn risk."
          ),
          tags$hr(),
          h6(class = "fw-bold", "Filter Directory Table Below:"),
          textInput("search_cust_text", "Search Customer Name or ID:", placeholder = "Filter directory table..."),
          selectInput(
            "filter_cust_churn",
            "Filter Outcome:",
            choices = c("All Customers" = "all", "Customers Who Left (Churn = Yes)" = "Yes", "Customers Who Stayed (Churn = No)" = "No"),
            selected = "all"
          ),
          selectInput("filter_cust_n", "Rows to Display:", choices = c(15, 30, 50, 100), selected = 15)
        )
      ),

      # Right column: Customer Dossier & Actual Churn Outcome
      column(
        width = 8,
        bslib::card(
          bslib::card_header(div(icon("user-check", class = "me-2 text-primary"), "Customer Account Dossier & Churn Outcome")),
          uiOutput("indiv_cust_dossier_ui")
        )
      )
    ),

    tags$br(),

    # Full Width Row: Searchable Customer Directory Table
    fluidRow(
      column(
        width = 12,
        bslib::card(
          bslib::card_header(div(icon("database", class = "me-2 text-primary"), "Customer Database Directory (Full Sample)")),
          uiOutput("cust_records_summary"),
          div(class = "table-responsive", tableOutput("table_cust_directory"))
        )
      )
    )
  ),

  # ----------------------------------------------------------------------------
  # Tab 3: What-If Churn Simulator (Prediction)
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Prediction",
    icon = icon("calculator"),
    fluidRow(
      # Input sidebar
      column(
        width = 4,
        bslib::card(
          bslib::card_header(div(icon("sliders-h", class = "me-2 text-primary"), "What-If Customer Simulator")),
          p(class = "text-muted small mb-2", "Adjust parameters below or choose a preset profile:"),
          div(
            class = "d-flex flex-wrap gap-1 mb-3",
            actionButton("btn_preset_churn", "High Risk Profile", class = "btn btn-sm btn-outline-danger", icon = icon("user-times")),
            actionButton("btn_preset_loyal", "Loyal Profile", class = "btn btn-sm btn-outline-success", icon = icon("user-shield")),
            actionButton("btn_preset_avg", "Average Profile", class = "btn btn-sm btn-outline-secondary", icon = icon("undo"))
          ),
          sliderInput("pred_tenure", "Tenure (Months with Company):", min = 1, max = 72, value = 6, step = 1),
          sliderInput("pred_monthly", "Monthly Charges ($):", min = 18, max = 120, value = 85, step = 1),
          numericInput("pred_total", "Total Charges ($):", value = 510, min = 18, max = 9000, step = 50),
          selectInput("pred_contract", "Contract Type:", choices = c("Month-to-month", "One year", "Two year"), selected = "Month-to-month"),
          selectInput("pred_internet", "Internet Service:", choices = c("Fiber optic", "DSL", "No"), selected = "Fiber optic"),
          selectInput("pred_payment", "Payment Method:", choices = c("Electronic check", "Mailed check", "Bank transfer (automatic)", "Credit card (automatic)"), selected = "Electronic check"),
          selectInput("pred_senior", "Senior Citizen:", choices = c("No", "Yes"), selected = "No"),
          actionButton("btn_predict", "Calculate Churn Probability", class = "btn-primary w-100 mt-2", icon = icon("calculator"))
        )
      ),

      # Output prediction report
      column(
        width = 8,
        bslib::card(
          bslib::card_header(div(icon("tachometer-alt", class = "me-2 text-primary"), "Simulated Customer Churn Risk Prediction")),
          uiOutput("prediction_badge_ui"),
          uiOutput("prediction_bar_ui"),
          uiOutput("prediction_explanation_ui"),
          tags$hr(),
          h6(class = "fw-bold", "How This Prediction Works:"),
          div(
            class = "lab-formula",
            tags$b("Risk Scoring in :"), tags$br(),
            "The model reviews the customer's account profile and evaluates each risk factor:", tags$br(),
            "• ", strong("High Risk Factors:"), " Month-to-month contracts, fiber optic internet, and electronic check payments increase risk.", tags$br(),
            "• ", strong("Strongest Protective Factors:"), " Two-year contracts, long tenure (> 24 months), and automatic bank payments increase loyalty.", tags$br(),
            "All factors are combined into an intuitive churn probability score from 0% to 100%."
          ),
          tags$hr(),
          h6(class = "fw-bold", "Model Reliability & Accuracy (Tested on 1,407 Unseen Customers):"),
          div(class = "table-responsive", tableOutput("pred_eval_metrics_table")),
          p(
            style = "font-size: 0.85rem; color: #64748b; margin-top: 15px;",
            icon("info-circle"),
            " Academic Disclaimer: This calculation is an empirical statistical prediction based on historical Telco sample data. It reflects statistical likelihood and does not guarantee an individual customer's future action."
          )
        )
      )
    )
  ),

  # ----------------------------------------------------------------------------
  # Tab 4: Data Visualization Gallery
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Visualizations",
    icon = icon("chart-bar"),
    fluidRow(
      column(
        width = 3,
        bslib::card(
          bslib::card_header(div(icon("list", class = "me-2 text-primary"), "Graph Selector")),
          radioButtons(
            "selected_plot",
            label = "Choose Visual Representation:",
            choices = list(
              "1. Overall Churn Distribution" = "p_churn",
              "2. Tenure Distribution Histogram" = "p_tenure",
              "3. Monthly Charges Histogram" = "p_charges",
              "4. Churn vs Contract Type" = "p_contract",
              "5. Churn vs Internet Service" = "p_internet",
              "6. Churn vs Payment Method" = "p_payment",
              "7. Boxplot: Monthly Charges by Churn" = "p_box_charges",
              "8. Boxplot: Tenure by Churn" = "p_box_tenure",
              "9. Correlation Heatmap Matrix" = "p_cor_heat",
              "10. Model ROC Curve (AUC)" = "p_roc"
            ),
            selected = "p_contract"
          )
        )
      ),
      column(
        width = 9,
        bslib::card(
          bslib::card_header(div(icon("image", class = "me-2 text-primary"), "Statistical Visualization Display")),
          plotOutput("main_visual_plot", height = "520px")
        )
      )
    )
  ),

  # ----------------------------------------------------------------------------
  # Tab 5: Data Analysis & Descriptive Statistics
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Data Analysis",
    icon = icon("table"),
    bslib::navset_card_tab(
      title = "Customer Profile & Summary Statistics",
      bslib::nav_panel(
        title = "Summary Statistics Table",
        p(class = "text-muted small", "Averages, typical middle values, common patterns, and spread across all continuous customer metrics:"),
        div(class = "table-responsive", tableOutput("table_summary_stats")),
        tags$hr(),
        h6(class = "fw-bold", "Comparing Customers Who Stayed vs Customers Who Left:"),
        p(class = "text-muted small", "Notice the stark differences in how long customers stayed and how much they paid:"),
        div(class = "table-responsive", tableOutput("table_grouped_stats"))
      ),
      bslib::nav_panel(
        title = "Frequency Tables",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "freq_cat_var",
              label = "Select Customer Feature:",
              choices = c("Contract", "InternetService", "PaymentMethod", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "PaperlessBilling"),
              selected = "Contract"
            )
          ),
          column(
            width = 8,
            h6(class = "fw-bold", "Customer Distribution Count"),
            div(class = "table-responsive", tableOutput("table_one_way_freq")),
            tags$hr(),
            h6(class = "fw-bold", "Breakdown: What percentage in each group stayed vs left?"),
            div(class = "table-responsive", tableOutput("table_two_way_freq"))
          )
        )
      ),
      bslib::nav_panel(
        title = "Dataset Preview",
        p(class = "text-muted small", "First 15 sample customer records with assigned unique names (total sample: 7,032 complete cases, zero missing values):"),
        div(class = "table-responsive", tableOutput("table_preview"))
      )
    )
  ),

  # ----------------------------------------------------------------------------
  # Tab 6: Probability Analysis
  # ----------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Probability Analysis",
    icon = icon("percentage"),
    fluidRow(
      column(
        width = 5,
        bslib::card(
          bslib::card_header(div(icon("lightbulb", class = "me-2 text-primary"), "Understanding Churn Probabilities")),
          div(
            class = "lab-formula",
            tags$b("Plain English Rule:"), tags$br(),
            "Group Churn Rate = (Customers in this group who left) ÷ (Total customers in this group)", tags$br(), tags$br(),
            tags$b("Risk Multiplier (vs Company Average):"), tags$br(),
            "Compares this specific group to the overall company average of 26.6%."
          ),
          p(
            strong("How to read Risk Multiplier:"), tags$br(),
            "• ", strong("Above 1.0x:"), " Higher churn risk (e.g. 1.6x means 60% more likely to leave than average).", tags$br(),
            "• ", strong("Below 1.0x:"), " Lower churn risk (e.g. 0.4x means 60% less likely to leave than average)."
          ),
          tags$hr(),
          h6(class = "fw-bold", "Select Customer Group to Analyze:"),
          selectInput(
            "cond_preset",
            label = "Choose Customer Group:",
            choices = list(
              "Contract: Month-to-month" = "m2m",
              "Contract: One year" = "oneyear",
              "Contract: Two year" = "twoyear",
              "Tenure: New customers (<= 12 Months)" = "tenure_short",
              "Tenure: Long-term loyalists (> 48 Months)" = "tenure_long",
              "Monthly Charges: High bills (> $70.35/mo)" = "charges_high",
              "Monthly Charges: Low/Moderate bills (<= $70.35/mo)" = "charges_low",
              "Internet Service: Fiber optic" = "fiber",
              "Internet Service: DSL" = "dsl",
              "Payment Method: Electronic check" = "echeck"
            ),
            selected = "m2m"
          ),
          uiOutput("prob_math_card_ui")
        )
      ),
      column(
        width = 7,
        bslib::card(
          bslib::card_header(div(icon("table", class = "me-2 text-primary"), "Churn Rates Across Key Categories")),
          bslib::navset_pill(
            bslib::nav_panel(
              title = "By Contract",
              div(class = "table-responsive mt-3", tableOutput("prob_tbl_contract"))
            ),
            bslib::nav_panel(
              title = "By Internet Service",
              div(class = "table-responsive mt-3", tableOutput("prob_tbl_internet"))
            ),
            bslib::nav_panel(
              title = "By Payment Method",
              div(class = "table-responsive mt-3", tableOutput("prob_tbl_payment"))
            ),
            bslib::nav_panel(
              title = "By Tenure Stage",
              div(class = "table-responsive mt-3", tableOutput("prob_tbl_tenure"))
            ),
            bslib::nav_panel(
              title = "By Spending Tier",
              div(class = "table-responsive mt-3", tableOutput("prob_tbl_charges"))
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. Server Logic Definition
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  # --- TAB 1: Dashboard Plots ---
  output$dash_plot_churn <- renderPlot({
    plot_churn_distribution(churn_df)
  })

  output$dash_plot_tenure <- renderPlot({
    plot_tenure_distribution(churn_df)
  })

  # --- TAB 5: Data Analysis & Descriptive Stats ---
  output$table_summary_stats <- renderTable(
    {
      tbl <- calc_numerical_summary_table(churn_df)
      names(tbl) <- c(
        "Metric", "Sample Size (N)", "Mean", "Median", "Mode",
        "Std Dev", "Variance", "Min", "25th Pct (Q1)", "75th Pct (Q3)",
        "Max", "Full Range", "Middle 50% (IQR)", "Pearson Skewness"
      )
      tbl
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$table_grouped_stats <- renderTable(
    {
      tbl <- calc_grouped_descriptive_stats(churn_df, "tenure", "Churn")
      names(tbl) <- c(
        "Customer Cohort", "Sample Size (N)", "Average (Mean)", "Middle (Median)", "Most Common (Mode)",
        "Std Dev", "Variance", "Minimum", "25th Pct (Q1)", "75th Pct (Q3)",
        "Maximum", "Full Range", "Middle 50% (IQR)", "Pearson Skewness"
      )
      tbl
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$table_one_way_freq <- renderTable(
    {
      tbl <- calc_frequency_table(churn_df, input$freq_cat_var)
      names(tbl) <- c("Category Level", "Customer Count", "Percentage of Base (%)", "Cumulative (%)")
      tbl
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$table_two_way_freq <- renderTable(
    {
      contingency <- calc_contingency_table(churn_df, input$freq_cat_var, "Churn")
      df_out <- as.data.frame.matrix(contingency$RowPercentages)
      colnames(df_out) <- c("Stayed (%)", "Left (%)")
      df_out
    },
    rownames = TRUE,
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  # --- TAB 5: Dataset Preview ---
  output$table_preview <- renderTable(
    {
      preview_df <- head(churn_df[, c(
        "CustomerName", "customerID", "Churn", "tenure", "MonthlyCharges",
        "TotalCharges", "Contract", "InternetService", "PaymentMethod", "SeniorCitizen"
      )], 15)

      preview_df$Churn <- ifelse(preview_df$Churn == "Yes", "Left (Churned)", "Stayed (Active)")
      preview_df$MonthlyCharges <- sprintf("$%.2f", preview_df$MonthlyCharges)
      preview_df$TotalCharges <- sprintf("$%.2f", preview_df$TotalCharges)

      names(preview_df) <- c(
        "Customer Name", "Customer ID", "Churn Status", "Tenure (mos)",
        "Monthly Bill", "Total Bill", "Contract", "Internet Service",
        "Payment Method", "Senior Citizen"
      )
      preview_df
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE
  )

  # --- TAB 6: Probability Analysis UI Card ---
  output$prob_math_card_ui <- renderUI({
    preset <- input$cond_preset
    key_probs <- get_key_conditional_probabilities(churn_df)

    res <- switch(preset,
      "m2m"          = key_probs$MonthToMonth,
      "oneyear"      = key_probs$OneYear,
      "twoyear"      = key_probs$TwoYear,
      "tenure_short" = key_probs$NewTenure,
      "tenure_long"  = key_probs$LongTenure,
      "charges_high" = key_probs$HighCharges,
      "charges_low"  = key_probs$LowCharges,
      "fiber"        = key_probs$FiberOptic,
      "dsl"          = calc_conditional_probability(churn_df, churn_df$InternetService == "DSL", "DSL Internet"),
      "echeck"       = key_probs$ElectronicCheck
    )

    risk_mult <- res$RelativeRisk
    is_high <- risk_mult >= 1.5
    is_elevated <- risk_mult > 1.0 && risk_mult < 1.5
    is_loyal <- risk_mult <= 0.6

    risk_badge_class <- if (is_high) "badge-risk-high" else if (is_elevated) "badge-risk-med" else "badge-risk-low"
    risk_badge_text <- if (is_high) sprintf("%.2fx Normal Churn (High Risk)", risk_mult) else if (is_elevated) sprintf("%.2fx Normal Churn (Elevated)", risk_mult) else sprintf("%.2fx Normal Churn (Highly Loyal)", risk_mult)

    div(
      class = "p-3 rounded mt-3",
      style = "background: #f8fafc; border: 1px solid #e2e8f0;",
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$h6(class = "m-0 fw-bold text-dark", res$Condition),
        tags$span(class = paste("badge-risk-pill", risk_badge_class), risk_badge_text)
      ),
      div(
        class = "row g-2 mb-3",
        div(
          class = "col-6",
          div(
            class = "p-2 bg-white rounded border text-center",
            div(class = "text-muted small fw-semibold", "TOTAL CUSTOMERS"),
            div(class = "fw-bold fs-5 text-primary", format(res$n_B, big.mark = ","))
          )
        ),
        div(
          class = "col-6",
          div(
            class = "p-2 bg-white rounded border text-center",
            div(class = "text-muted small fw-semibold", "CUSTOMERS WHO LEFT"),
            div(class = "fw-bold fs-5 text-danger", format(res$n_A_and_B, big.mark = ","))
          )
        )
      ),
      div(
        class = "mb-3",
        div(
          class = "d-flex justify-content-between small fw-semibold text-muted mb-1",
          span("Group Churn Rate:"),
          span(class = "fw-bold text-dark", sprintf("%.1f%%", res$Percentage))
        ),
        div(
          class = "custom-progress mb-2",
          div(
            class = "progress-bar-fill",
            style = sprintf("width: %.1f%%; background-color: %s;", res$Percentage, if(res$Percentage >= 35) "#ef4444" else if (res$Percentage >= 20) "#f59e0b" else "#10b981"),
            sprintf("%.1f%%", res$Percentage)
          )
        ),
        div(
          class = "d-flex justify-content-between small text-muted",
          span(icon("building", class = "me-1"), "Company-Wide Baseline:"),
          span(sprintf("%.1f%%", res$BaselinePercentage))
        )
      ),
      div(
        class = "alert alert-light border mb-0 p-2 rounded",
        style = "font-size: 0.88rem; color: #334155; border-left: 3px solid #2563eb !important;",
        tags$strong(icon("lightbulb", class = "text-primary me-1"), "Plain-English Meaning: "),
        if (is_high) {
          sprintf("Customers in this group face a very HIGH risk of leaving (%.2fx the normal company rate). Retention campaigns should prioritize this segment.", risk_mult)
        } else if (is_elevated) {
          sprintf("Customers in this segment are somewhat more likely to leave than average (%.2fx the normal company rate).", risk_mult)
        } else if (is_loyal) {
          sprintf("Customers in this group are exceptionally LOYAL and rarely churn (only %.2fx the normal rate).", risk_mult)
        } else {
          sprintf("Customers in this cohort exhibit below-average attrition hazards (%.2fx normal rate).", risk_mult)
        }
      )
    )
  })

  output$prob_tbl_contract <- renderTable(
    {
      format_prob_table(calc_churn_by_categorical_probability(churn_df, "Contract"))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$prob_tbl_internet <- renderTable(
    {
      format_prob_table(calc_churn_by_categorical_probability(churn_df, "InternetService"))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$prob_tbl_payment <- renderTable(
    {
      format_prob_table(calc_churn_by_categorical_probability(churn_df, "PaymentMethod"))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$prob_tbl_tenure <- renderTable(
    {
      format_prob_table(calc_churn_by_categorical_probability(churn_df, "TenureGroup"))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  output$prob_tbl_charges <- renderTable(
    {
      format_prob_table(calc_churn_by_categorical_probability(churn_df, "MonthlyChargeGroup"))
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    digits = 2
  )

  # --- TAB 4: Visualization Gallery ---
  output$main_visual_plot <- renderPlot({
    switch(input$selected_plot,
      "p_churn"       = plot_churn_distribution(churn_df),
      "p_tenure"      = plot_tenure_distribution(churn_df),
      "p_charges"     = plot_monthly_charges_distribution(churn_df),
      "p_contract"    = plot_churn_vs_categorical(churn_df, "Contract", "Contract Type"),
      "p_internet"    = plot_churn_vs_categorical(churn_df, "InternetService", "Internet Service"),
      "p_payment"     = plot_churn_vs_categorical(churn_df, "PaymentMethod", "Payment Method"),
      "p_box_charges" = plot_boxplot_by_churn(churn_df, "MonthlyCharges", "Monthly Charges ($)"),
      "p_box_tenure"  = plot_boxplot_by_churn(churn_df, "tenure", "Tenure (Months)"),
      "p_cor_heat"    = plot_correlation_matrix(churn_df),
      "p_roc"         = plot_roc_curve(model_eval)
    )
  })

  # ============================================================================
  # TAB 2: Dedicated Individual Customers Explorer
  # ============================================================================

  # Populate customer choices for search dropdown
  cust_choices <- setNames(
    churn_df$customerID,
    sprintf("%s (%s)", churn_df$CustomerName, churn_df$customerID)
  )
  updateSelectizeInput(
    session,
    "indiv_cust_select",
    choices = c("Search or choose a customer..." = "", cust_choices),
    selected = churn_df$customerID[1],
    server = TRUE
  )

  # Pick random customer button observer
  observeEvent(input$btn_random_cust, {
    rand_id <- sample(churn_df$customerID, 1)
    updateSelectizeInput(session, "indiv_cust_select", selected = rand_id)
  })

  # Render Comprehensive Customer Dossier & Churn Outcome
  output$indiv_cust_dossier_ui <- renderUI({
    cid <- input$indiv_cust_select
    if (is.null(cid) || !nzchar(cid) || !cid %in% churn_df$customerID) {
      cid <- churn_df$customerID[1]
    }

    cust <- churn_df[churn_df$customerID == cid, ][1, ]
    actual_left <- cust$Churn == "Yes"
    actual_badge <- if (actual_left) "badge-churn" else "badge-stay"
    actual_text  <- if (actual_left) "LEFT THE COMPANY (CHURNED)" else "STAYED WITH COMPANY (ACTIVE)"

    prob_val <- cust$PredictedProb
    risk_color <- if (prob_val >= 70) "#ef4444" else if (prob_val >= 40) "#f59e0b" else "#10b981"

    aligned <- (actual_left && prob_val >= 50) || (!actual_left && prob_val < 50)
    match_alert_class <- if (aligned) "alert alert-success" else "alert alert-warning"
    match_alert_icon  <- if (aligned) "check-circle" else "exclamation-triangle"
    match_alert_title <- if (aligned) "Prediction & Reality Match" else "Borderline / Counter-Trend Observation"
    match_alert_text  <- if (aligned) {
      sprintf("The model accurately flagged this customer: with an estimated %.1f%% churn likelihood, this customer indeed %s.",
              prob_val, ifelse(actual_left, "canceled their subscription", "remained active with the company"))
    } else {
      sprintf("The model estimated a %.1f%% churn likelihood (%s), but in historical reality this customer %s.",
              prob_val, cust$PredictedRiskTier, ifelse(actual_left, "canceled their contract early", "remained active"))
    }

    tagList(
      # Name, ID and Badges Header
      div(
        style = "border-bottom: 2px solid #f1f5f9; padding-bottom: 16px; margin-bottom: 18px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
          div(
            h4(style = "font-weight: 800; margin: 0; color: #0f172a;", icon("user-circle", class = "text-primary me-2"), cust$CustomerName),
            p(style = "color: #64748b; margin: 3px 0 0 0; font-size: 0.88rem;",
              sprintf("Customer ID: %s | Gender: %s | Senior Citizen: %s", cust$customerID, cust$gender, cust$SeniorCitizen))
          ),
          div(
            span(class = paste("stat-badge", actual_badge), icon(if (actual_left) "user-times" else "user-check"), " ", actual_text)
          )
        )
      ),

      # Accuracy Alert Banner
      div(
        class = match_alert_class,
        style = "margin-bottom: 18px; padding: 12px 16px; border-radius: 8px;",
        strong(icon(match_alert_icon), " ", match_alert_title, ": "), match_alert_text
      ),

      # 4 Summary Metric Boxes
      fluidRow(
        column(
          width = 3,
          div(
            class = "dossier-stat-card",
            div(class = "dossier-stat-num text-primary", sprintf("%d mos", cust$tenure)),
            div(class = "dossier-stat-lbl", "Tenure Length")
          )
        ),
        column(
          width = 3,
          div(
            class = "dossier-stat-card",
            div(class = "dossier-stat-num text-dark", sprintf("$%.2f", cust$MonthlyCharges)),
            div(class = "dossier-stat-lbl", "Monthly Bill")
          )
        ),
        column(
          width = 3,
          div(
            class = "dossier-stat-card",
            div(class = "dossier-stat-num text-dark", sprintf("$%.2f", cust$TotalCharges)),
            div(class = "dossier-stat-lbl", "Lifetime Billing")
          )
        ),
        column(
          width = 3,
          div(
            class = "dossier-stat-card",
            div(class = "dossier-stat-num", style = "color: #7c3aed;", as.character(cust$Contract)),
            div(class = "dossier-stat-lbl", "Contract Type")
          )
        )
      ),

      tags$hr(),

      # Model Risk Probability Gauge
      div(
        class = "d-flex justify-content-between align-items-center mb-1",
        h6(class = "fw-bold text-dark mb-0", "Individual Model Churn Risk Score:"),
        span(class = "fw-bold small", style = sprintf("color: %s;", risk_color), sprintf("%.1f%% (%s)", prob_val, cust$PredictedRiskTier))
      ),
      div(
        class = "custom-progress mb-3",
        div(
          class = "progress-bar-fill",
          style = sprintf("width: %.1f%%; background-color: %s;", prob_val, risk_color),
          sprintf("%.1f%% Churn Likelihood", prob_val)
        )
      ),

      tags$hr(),

      # Account Details Grid
      h6(class = "fw-bold text-dark mb-2", "Account & Plan Configuration:"),
      div(
        class = "row g-2",
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Internet Service"),
            div(class = "fw-semibold text-dark", cust$InternetService)
          )
        ),
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Payment Method"),
            div(class = "fw-semibold text-dark", cust$PaymentMethod)
          )
        ),
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Phone Service"),
            div(class = "fw-semibold text-dark", cust$PhoneService)
          )
        ),
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Paperless Billing"),
            div(class = "fw-semibold text-dark", cust$PaperlessBilling)
          )
        ),
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Partner Support"),
            div(class = "fw-semibold text-dark", cust$Partner)
          )
        ),
        div(
          class = "col-md-4 col-sm-6",
          div(
            class = "p-2 rounded bg-light border",
            div(class = "text-muted small fw-semibold", "Dependents"),
            div(class = "fw-semibold text-dark", cust$Dependents)
          )
        )
      )
    )
  })

  # Filtered Customer Directory Table for Tab 2
  filtered_customers <- reactive({
    df <- churn_df
    q <- if (!is.null(input$search_cust_text)) trimws(input$search_cust_text) else ""
    if (nzchar(q)) {
      matched <- grepl(q, df$CustomerName, ignore.case = TRUE) | grepl(q, df$customerID, ignore.case = TRUE)
      df <- df[matched, ]
    }
    if (!is.null(input$filter_cust_churn) && input$filter_cust_churn != "all") {
      df <- df[df$Churn == input$filter_cust_churn, ]
    }
    df
  })

  output$cust_records_summary <- renderUI({
    df <- filtered_customers()
    n_match <- nrow(df)
    n_display <- min(n_match, as.numeric(input$filter_cust_n %||% 15))
    p(
      style = "color: #64748b; font-size: 0.88rem; margin-top: 4px; margin-bottom: 12px;",
      sprintf("Showing top %d matching customer records (out of %s total matching):", n_display, format(n_match, big.mark = ","))
    )
  })

  output$table_cust_directory <- renderTable(
    {
      df <- filtered_customers()
      n_rows <- min(nrow(df), as.numeric(input$filter_cust_n %||% 15))
      if (n_rows == 0) {
        return(data.frame(Status = "No customer records matched your query."))
      }

      preview_df <- head(df[, c(
        "CustomerName", "customerID", "Churn", "PredictedProb",
        "tenure", "MonthlyCharges", "TotalCharges", "Contract",
        "InternetService", "PaymentMethod"
      )], n_rows)

      preview_df$Churn <- ifelse(preview_df$Churn == "Yes", "Left (Churned)", "Stayed (Active)")
      preview_df$PredictedProb <- sprintf("%.1f%%", preview_df$PredictedProb)
      preview_df$MonthlyCharges <- sprintf("$%.2f", preview_df$MonthlyCharges)
      preview_df$TotalCharges <- sprintf("$%.2f", preview_df$TotalCharges)

      names(preview_df) <- c(
        "Customer Name", "Customer ID", "Actual Status", "Predicted Risk",
        "Tenure (mos)", "Monthly Bill", "Total Bill", "Contract",
        "Internet Service", "Payment Method"
      )
      preview_df
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE
  )

  # ============================================================================
  # TAB 3: What-If Churn Simulator (Prediction)
  # ============================================================================

  # Auto-update Total Charges when tenure or monthly charges slider changes
  observe({
    calc_total <- round(input$pred_tenure * input$pred_monthly, 2)
    updateNumericInput(session, "pred_total", value = calc_total)
  })

  # Simulation quick presets
  observeEvent(input$btn_preset_churn, {
    updateSliderInput(session, "pred_tenure", value = 3)
    updateSliderInput(session, "pred_monthly", value = 95)
    updateSelectInput(session, "pred_contract", selected = "Month-to-month")
    updateSelectInput(session, "pred_internet", selected = "Fiber optic")
    updateSelectInput(session, "pred_payment", selected = "Electronic check")
    updateSelectInput(session, "pred_senior", selected = "Yes")
  })

  observeEvent(input$btn_preset_loyal, {
    updateSliderInput(session, "pred_tenure", value = 48)
    updateSliderInput(session, "pred_monthly", value = 45)
    updateSelectInput(session, "pred_contract", selected = "Two year")
    updateSelectInput(session, "pred_internet", selected = "DSL")
    updateSelectInput(session, "pred_payment", selected = "Credit card (automatic)")
    updateSelectInput(session, "pred_senior", selected = "No")
  })

  observeEvent(input$btn_preset_avg, {
    updateSliderInput(session, "pred_tenure", value = 29)
    updateSliderInput(session, "pred_monthly", value = 65)
    updateSelectInput(session, "pred_contract", selected = "One year")
    updateSelectInput(session, "pred_internet", selected = "DSL")
    updateSelectInput(session, "pred_payment", selected = "Bank transfer (automatic)")
    updateSelectInput(session, "pred_senior", selected = "No")
  })

  prediction_reactive <- eventReactive(input$btn_predict,
    {
      predict_individual_customer(
        model = churn_model,
        tenure = input$pred_tenure,
        MonthlyCharges = input$pred_monthly,
        TotalCharges = input$pred_total,
        Contract = input$pred_contract,
        InternetService = input$pred_internet,
        PaymentMethod = input$pred_payment,
        SeniorCitizen = input$pred_senior
      )
    },
    ignoreNULL = FALSE
  )

  output$prediction_badge_ui <- renderUI({
    pred <- prediction_reactive()
    badge_class <- if (pred$PredictedStatus == "Likely to Churn") "badge-churn" else "badge-stay"
    div(
      class = "d-flex align-items-center mb-3",
      span(class = paste("stat-badge", badge_class), pred$PredictedStatus),
      span(
        style = "font-size: 1.25rem; font-weight: 700; margin-left: 15px; color: #0f172a;",
        sprintf("Predicted Churn Probability: %.1f%% (%s)", pred$ProbabilityPercent, pred$RiskTier)
      )
    )
  })

  output$prediction_bar_ui <- renderUI({
    pred <- prediction_reactive()
    prob_val <- min(max(pred$ProbabilityPercent, 0), 100)
    bar_color <- if (prob_val >= 70) "#ef4444" else if (prob_val >= 40) "#f59e0b" else "#10b981"

    div(
      class = "custom-progress mb-3",
      div(
        class = "progress-bar-fill",
        style = sprintf("width: %.1f%%; background-color: %s;", prob_val, bar_color),
        sprintf("%.1f%% Churn Probability", prob_val)
      )
    )
  })

  output$prediction_explanation_ui <- renderUI({
    pred <- prediction_reactive()
    drivers <- pred$KeyDrivers

    driver_items <- if (length(drivers) > 0) {
      lapply(drivers, function(d) {
        is_protective <- grepl("protect|loyalty", d, ignore.case = TRUE)
        icon_name <- if (is_protective) "shield-alt" else "exclamation-triangle"
        icon_color <- if (is_protective) "#16a34a" else "#dc2626"
        badge_bg <- if (is_protective) "#f0fdf4" else "#fef2f2"
        badge_border <- if (is_protective) "#bbf7d0" else "#fecaca"

        div(
          class = "d-flex align-items-center gap-2 p-2 mb-2 rounded",
          style = sprintf("background-color: %s; border: 1px solid %s;", badge_bg, badge_border),
          tags$span(style = sprintf("color: %s;", icon_color), icon(icon_name)),
          tags$span(style = "font-size: 0.9rem; color: #1e293b;", d)
        )
      })
    } else {
      p(class = "text-muted small", "No strong directional risk drivers identified for this profile.")
    }

    tagList(
      h6(class = "fw-bold text-dark mt-3 mb-2", "Key Risk & Retention Drivers Identified:"),
      div(class = "mb-2", driver_items)
    )
  })

  output$pred_eval_metrics_table <- renderTable(
    {
      metrics_tbl <- data.frame(
        "Performance Metric" = c(
          "Accuracy (Overall Correctness)",
          "Precision (Reliability of Churn Warning)",
          "Recall (Catch Rate of Churned Customers)",
          "Specificity (Catch Rate of Loyal Customers)",
          "F1-Score (Balanced Quality Score)",
          "ROC-AUC (Separation Ability)"
        ),
        "Plain-English Meaning" = c(
          "Overall percentage of correct predictions across all test customers",
          "When the model predicts a customer will leave, how often is it right?",
          "Out of all customers who actually left, what percentage did the model catch?",
          "Out of all customers who stayed, what percentage were correctly recognized?",
          "Balanced overall score combining both Precision and Recall",
          "Overall ability to accurately distinguish customers who stay from those who leave"
        ),
        "Score" = sprintf("%.2f", model_eval$Metrics$Value),
        "Percentage" = model_eval$Metrics$Percentage,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      metrics_tbl
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE
  )
}

# Launch shinyApp
shinyApp(ui = ui, server = server)
