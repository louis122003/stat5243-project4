# ======================================================
# Project 4 Fraud Detection Dashboard
# R Shiny Version - Customized for Uploaded CSV Files
# ======================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(plotly)
library(tidyr)
library(scales)

# ------------------------------------------------------
# Load Data
# ------------------------------------------------------

fraud_data <- read_csv("Fraud_clean_for_shiny.csv", show_col_types = FALSE)
pca_data <- read_csv("pca_for_shiny.csv", show_col_types = FALSE)
model_results <- read_csv("model_results_for_shiny.csv", show_col_types = FALSE)
pca_loadings <- read_csv("pca_loadings_for_shiny.csv", show_col_types = FALSE)
pca_variance <- read_csv("pca_variance_for_shiny.csv", show_col_types = FALSE)
tree_importance <- read_csv("tree_group_importance_for_shiny.csv", show_col_types = FALSE)
xgb_importance <- read_csv("xgb_group_importance_for_shiny.csv", show_col_types = FALSE)

# ------------------------------------------------------
# Prepare Data
# ------------------------------------------------------

fraud_data <- fraud_data %>%
  mutate(
    fraud_label = factor(
      is_fraud,
      levels = c(0, 1),
      labels = c("Non-Fraud", "Fraud")
    ),
    late_night_label = factor(
      late_night,
      levels = c(0, 1),
      labels = c("Not Late Night", "Late Night")
    )
  )

pca_data <- pca_data %>%
  mutate(
    fraud_label = factor(
      is_fraud,
      levels = c(0, 1),
      labels = c("Non-Fraud", "Fraud")
    )
  )

# Standardize importance column names
if ("Variable_Group" %in% names(tree_importance)) {
  tree_importance <- tree_importance %>%
    rename(Feature_Group = Variable_Group)
}

tree_importance <- tree_importance %>%
  mutate(Model = "Decision Tree")

if ("Group" %in% names(xgb_importance)) {
  xgb_importance <- xgb_importance %>%
    rename(Feature_Group = Group)
}

xgb_importance <- xgb_importance %>%
  mutate(Model = "XGBoost")

importance_all <- bind_rows(tree_importance, xgb_importance)

numeric_vars <- c(
  "amt_thousand",
  "age",
  "city_pop_thousand",
  "median_household_income",
  "poverty_rate"
)

categorical_vars <- c(
  "category",
  "gender",
  "region",
  "late_night_label"
)

corr_vars <- c(
  "amt_thousand",
  "age",
  "city_pop_thousand",
  "median_household_income",
  "poverty_rate",
  "late_night"
)

# Round numeric columns only. This avoids errors when a table also contains
# character/factor columns such as fraud_label or Model.
round_numeric_cols <- function(df, digits = 4) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

# ------------------------------------------------------
# UI
# ------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Fraud Detection Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
      menuItem("PCA", tabName = "pca", icon = icon("project-diagram")),
      menuItem("Model Performance", tabName = "model", icon = icon("chart-line")),
      menuItem("Feature Importance", tabName = "importance", icon = icon("list-ol"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f7f9fb;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        }
        .small-box {
          border-radius: 10px;
        }
        h3, h4 {
          font-weight: 600;
        }
      "))
    ),
    
    tabItems(
      
      # --------------------------------------------------
      # Overview
      # --------------------------------------------------
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("total_obs", width = 3),
          valueBoxOutput("total_features", width = 3),
          valueBoxOutput("fraud_obs", width = 3),
          valueBoxOutput("fraud_rate", width = 3)
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Project Overview",
            status = "primary",
            solidHeader = TRUE,
            
            h3("Financial Fraud Detection Project"),
            p("This interactive dashboard presents the main workflow of the fraud detection project."),
            p("The final cleaned dataset contains 4,166 observations and 9 modeling features. The data includes transaction-level variables, demographic information, and external socioeconomic variables such as median household income and poverty rate."),
            
            h4("Dashboard Sections"),
            tags$ul(
              tags$li("EDA: compare fraud and non-fraud transaction patterns."),
              tags$li("Heatmap: examine correlations among numeric variables."),
              tags$li("PCA: visualize transactions in a two-dimensional PCA space."),
              tags$li("Model Performance: compare Logistic Regression, Decision Tree, and XGBoost."),
              tags$li("Feature Importance: compare important variable groups from Decision Tree and XGBoost.")
            ),
            
            h4("Main Goal"),
            p("The goal is to communicate the data science workflow and key findings in a clear, interactive, and user-friendly format.")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Dataset Preview",
            status = "info",
            solidHeader = TRUE,
            DTOutput("data_preview")
          )
        )
      ),
      
      # --------------------------------------------------
      # EDA
      # --------------------------------------------------
      
      tabItem(
        tabName = "eda",
        
        fluidRow(
          box(
            width = 4,
            title = "EDA Controls",
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              inputId = "eda_var",
              label = "Select numeric variable:",
              choices = numeric_vars,
              selected = "amt_thousand"
            ),
            
            selectInput(
              inputId = "cat_var",
              label = "Select categorical variable:",
              choices = categorical_vars,
              selected = "category"
            )
          ),
          
          box(
            width = 8,
            title = "Histogram by Fraud Status",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("hist_plot", height = "420px")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Boxplot by Fraud Status",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("box_plot", height = "380px")
          ),
          
          box(
            width = 6,
            title = "Fraud Rate by Category",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("cat_bar_plot", height = "380px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Summary Statistics by Fraud Status",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("summary_table")
          )
        )
      ),
      
      # --------------------------------------------------
      # Heatmap
      # --------------------------------------------------
      
      tabItem(
        tabName = "heatmap",
        
        fluidRow(
          box(
            width = 12,
            title = "Correlation Heatmap of Numeric Variables",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("corr_heatmap", height = "620px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Interpretation",
            status = "info",
            solidHeader = TRUE,
            p("The heatmap shows pairwise correlations among numeric variables. Strong positive or negative values suggest that two variables move together or contain related information.")
          )
        )
      ),
      
      # --------------------------------------------------
      # PCA
      # --------------------------------------------------
      
      tabItem(
        tabName = "pca",
        
        fluidRow(
          box(
            width = 12,
            title = "PCA Scatter Plot",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("pca_scatter", height = "550px")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Explained Variance Ratio",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("pca_variance_plot", height = "380px")
          ),
          
          box(
            width = 6,
            title = "PCA Summary by Fraud Status",
            status = "info",
            solidHeader = TRUE,
            DTOutput("pca_summary_table")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Top PCA Loadings",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("pca_loading_plot", height = "520px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "PCA Interpretation",
            status = "warning",
            solidHeader = TRUE,
            p("PCA is used here as an exploratory method to reduce the feature space into two principal components."),
            p("The scatter plot helps check whether fraud and non-fraud transactions show visible separation patterns after dimension reduction. This is not used as a direct prediction model.")
          )
        )
      ),
      
      # --------------------------------------------------
      # Model Performance
      # --------------------------------------------------
      
      tabItem(
        tabName = "model",
        
        fluidRow(
          box(
            width = 12,
            title = "Model Comparison Table",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("model_table")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "AUC Comparison",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("auc_plot", height = "420px")
          ),
          
          box(
            width = 6,
            title = "Balanced Accuracy Comparison",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("balanced_acc_plot", height = "420px")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Sensitivity Comparison",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("sensitivity_plot", height = "420px")
          ),
          
          box(
            width = 6,
            title = "Specificity Comparison",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("specificity_plot", height = "420px")
          )
        )
      ),
      
      # --------------------------------------------------
      # Feature Importance
      # --------------------------------------------------
      
      tabItem(
        tabName = "importance",
        
        fluidRow(
          box(
            width = 6,
            title = "Decision Tree Feature Importance",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("tree_importance_plot", height = "560px")
          ),
          
          box(
            width = 6,
            title = "XGBoost Feature Importance",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("xgb_importance_plot", height = "560px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Combined Feature Importance Table",
            status = "info",
            solidHeader = TRUE,
            DTOutput("importance_table")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------
# Server
# ------------------------------------------------------

server <- function(input, output, session) {
  
  # ------------------------------
  # Overview
  # ------------------------------
  
  output$total_obs <- renderValueBox({
    valueBox(
      value = comma(nrow(fraud_data)),
      subtitle = "Total Observations",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$total_features <- renderValueBox({
    valueBox(
      value = ncol(fraud_data) - 2,
      subtitle = "Modeling Features",
      icon = icon("columns"),
      color = "aqua"
    )
  })
  
  output$fraud_obs <- renderValueBox({
    fraud_count <- sum(fraud_data$is_fraud == 1, na.rm = TRUE)
    valueBox(
      value = comma(fraud_count),
      subtitle = "Fraud Observations",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$fraud_rate <- renderValueBox({
    rate <- mean(fraud_data$is_fraud == 1, na.rm = TRUE)
    valueBox(
      value = percent(rate, accuracy = 0.01),
      subtitle = "Fraud Rate",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$data_preview <- renderDT({
    datatable(
      head(fraud_data, 20),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # ------------------------------
  # EDA
  # ------------------------------
  
  output$hist_plot <- renderPlotly({
    req(input$eda_var)
    
    p <- ggplot(
      fraud_data,
      aes(x = .data[[input$eda_var]], fill = fraud_label)
    ) +
      geom_histogram(alpha = 0.65, bins = 35, position = "identity") +
      labs(
        title = paste("Distribution of", input$eda_var),
        x = input$eda_var,
        y = "Count",
        fill = "Fraud Status"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  output$box_plot <- renderPlotly({
    req(input$eda_var)
    
    p <- ggplot(
      fraud_data,
      aes(x = fraud_label, y = .data[[input$eda_var]], fill = fraud_label)
    ) +
      geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
      labs(
        title = paste(input$eda_var, "by Fraud Status"),
        x = "Fraud Status",
        y = input$eda_var
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$cat_bar_plot <- renderPlotly({
    req(input$cat_var)
    
    plot_df <- fraud_data %>%
      filter(!is.na(.data[[input$cat_var]])) %>%
      group_by(category_value = .data[[input$cat_var]]) %>%
      summarise(
        count = n(),
        fraud_rate = mean(is_fraud == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(fraud_rate))
    
    if (nrow(plot_df) > 15) {
      plot_df <- plot_df %>% slice_head(n = 15)
    }
    
    p <- ggplot(
      plot_df,
      aes(
        x = reorder(category_value, fraud_rate),
        y = fraud_rate,
        text = paste0(
          "Category: ", category_value,
          "<br>Fraud Rate: ", percent(fraud_rate, accuracy = 0.01),
          "<br>Count: ", count
        )
      )
    ) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Fraud Rate by", input$cat_var),
        x = input$cat_var,
        y = "Fraud Rate"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_table <- renderDT({
    req(input$eda_var)
    
    summary_df <- fraud_data %>%
      group_by(fraud_label) %>%
      summarise(
        Count = n(),
        Mean = mean(.data[[input$eda_var]], na.rm = TRUE),
        Median = median(.data[[input$eda_var]], na.rm = TRUE),
        SD = sd(.data[[input$eda_var]], na.rm = TRUE),
        Min = min(.data[[input$eda_var]], na.rm = TRUE),
        Max = max(.data[[input$eda_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    datatable(
      round_numeric_cols(summary_df, 4),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # ------------------------------
  # Heatmap
  # ------------------------------
  
  output$corr_heatmap <- renderPlotly({
    
    corr_data <- fraud_data %>%
      select(all_of(corr_vars))
    
    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    corr_long <- as.data.frame(as.table(corr_matrix))
    names(corr_long) <- c("Var1", "Var2", "Correlation")
    
    p <- ggplot(
      corr_long,
      aes(
        x = Var1,
        y = Var2,
        fill = Correlation,
        text = paste0(
          Var1, " vs ", Var2,
          "<br>Correlation: ", round(Correlation, 3)
        )
      )
    ) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "#2166AC",
        mid = "white",
        high = "#B2182B",
        midpoint = 0,
        limits = c(-1, 1)
      ) +
      labs(
        title = "Correlation Heatmap",
        x = "",
        y = "",
        fill = "Correlation"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # ------------------------------
  # PCA
  # ------------------------------
  
  output$pca_scatter <- renderPlotly({
    
    p <- ggplot(
      pca_data,
      aes(
        x = PC1,
        y = PC2,
        color = fraud_label,
        text = paste0(
          "Fraud Status: ", fraud_label,
          "<br>PC1: ", round(PC1, 3),
          "<br>PC2: ", round(PC2, 3)
        )
      )
    ) +
      geom_point(alpha = 0.7, size = 2.2) +
      labs(
        title = "PCA Visualization of Transactions",
        x = "Principal Component 1",
        y = "Principal Component 2",
        color = "Fraud Status"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$pca_variance_plot <- renderPlotly({
    
    p <- ggplot(
      pca_variance,
      aes(
        x = Component,
        y = Explained_Variance_Ratio,
        text = paste0(
          "Component: ", Component,
          "<br>Explained Variance: ", percent(Explained_Variance_Ratio, accuracy = 0.01)
        )
      )
    ) +
      geom_col(fill = "#2c7fb8", width = 0.6) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Explained Variance Ratio",
        x = "Principal Component",
        y = "Explained Variance Ratio"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$pca_summary_table <- renderDT({
    
    pca_summary <- pca_data %>%
      group_by(fraud_label) %>%
      summarise(
        Count = n(),
        Mean_PC1 = mean(PC1, na.rm = TRUE),
        Mean_PC2 = mean(PC2, na.rm = TRUE),
        SD_PC1 = sd(PC1, na.rm = TRUE),
        SD_PC2 = sd(PC2, na.rm = TRUE),
        .groups = "drop"
      )
    
    datatable(
      round_numeric_cols(pca_summary, 4),
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$pca_loading_plot <- renderPlotly({
    
    loading_long <- pca_loadings %>%
      pivot_longer(
        cols = c(PC1, PC2),
        names_to = "Component",
        values_to = "Loading"
      ) %>%
      group_by(Component) %>%
      slice_max(order_by = abs(Loading), n = 10) %>%
      ungroup()
    
    p <- ggplot(
      loading_long,
      aes(
        x = reorder(Feature, abs(Loading)),
        y = Loading,
        fill = Component,
        text = paste0(
          "Feature: ", Feature,
          "<br>Component: ", Component,
          "<br>Loading: ", round(Loading, 4)
        )
      )
    ) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = "Top PCA Loadings by Absolute Value",
        x = "Feature",
        y = "Loading"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ------------------------------
  # Model Performance
  # ------------------------------
  
  output$model_table <- renderDT({
    datatable(
      round_numeric_cols(model_results, 4),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  make_metric_plot <- function(metric_name, plot_title) {
    p <- ggplot(
      model_results,
      aes(
        x = reorder(Model, .data[[metric_name]]),
        y = .data[[metric_name]],
        text = paste0(
          "Model: ", Model,
          "<br>", metric_name, ": ", round(.data[[metric_name]], 4)
        )
      )
    ) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      ylim(0, 1) +
      labs(
        title = plot_title,
        x = "Model",
        y = metric_name
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  }
  
  output$auc_plot <- renderPlotly({
    make_metric_plot("AUC", "AUC Comparison across Models")
  })
  
  output$balanced_acc_plot <- renderPlotly({
    make_metric_plot("Balanced Accuracy", "Balanced Accuracy Comparison across Models")
  })
  
  output$sensitivity_plot <- renderPlotly({
    make_metric_plot("Sensitivity", "Sensitivity Comparison across Models")
  })
  
  output$specificity_plot <- renderPlotly({
    make_metric_plot("Specificity", "Specificity Comparison across Models")
  })
  
  # ------------------------------
  # Feature Importance
  # ------------------------------
  
  output$tree_importance_plot <- renderPlotly({
    
    plot_df <- tree_importance %>%
      arrange(desc(Importance))
    
    p <- ggplot(
      plot_df,
      aes(
        x = reorder(Feature_Group, Importance),
        y = Importance,
        text = paste0(
          "Feature Group: ", Feature_Group,
          "<br>Importance: ", round(Importance, 4)
        )
      )
    ) +
      geom_col(fill = "#1b9e77", width = 0.7) +
      coord_flip() +
      labs(
        title = "Decision Tree Group Importance",
        x = "Feature Group",
        y = "Importance"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 140, r = 30, t = 70, b = 70),
        title = list(text = "Decision Tree Group Importance", x = 0.5, xanchor = "center"),
        xaxis = list(automargin = TRUE),
        yaxis = list(automargin = TRUE)
      )
  })
  
  output$xgb_importance_plot <- renderPlotly({
    
    plot_df <- xgb_importance %>%
      arrange(desc(Importance))
    
    p <- ggplot(
      plot_df,
      aes(
        x = reorder(Feature_Group, Importance),
        y = Importance,
        text = paste0(
          "Feature Group: ", Feature_Group,
          "<br>Importance: ", round(Importance, 4)
        )
      )
    ) +
      geom_col(fill = "#d95f02", width = 0.7) +
      coord_flip() +
      labs(
        title = "XGBoost Group Importance",
        x = "Feature Group",
        y = "Importance"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 140, r = 30, t = 70, b = 70),
        title = list(text = "XGBoost Group Importance", x = 0.5, xanchor = "center"),
        xaxis = list(automargin = TRUE),
        yaxis = list(automargin = TRUE)
      )
  })
  
  output$importance_table <- renderDT({
    datatable(
      importance_all,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

# ------------------------------------------------------
# Run App
# ------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
