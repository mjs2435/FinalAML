library(shiny)
library(shinythemes)
library(shinycssloaders)
library(ggExtra)
library(data.table)

ui <- fluidPage(
  titlePanel("Machine Learning Model Explorer: Interactive Analysis & Predictions"),
  
  navbarPage(
    title = "STAT 3106",
    theme = shinytheme("flatly"),
    
    tabPanel("Overview", icon = icon("info-circle"),
             titlePanel("Welcome to the Machine Learning Model Explorer"),
             mainPanel(
               h4("App Overview"),
               p("This comprehensive tool is designed to assist students and researchers in analyzing and visualizing machine learning datasets. It offers capabilities ranging from data uploading and preprocessing to exploratory data analysis."),
               h5("Key Features:"),
               tags$ul(
                 tags$li("Upload Data: Securely import your datasets directly into the app. You can select from pre-loaded datasets like 'Credit Card Fraud Detection' for quick access or upload your own data in CSV format."),
                 tags$li("Data Exploration: Utilize various interactive visualizations to understand data distributions, correlations, and patterns. Tools such as scatterplots, histograms, and numeric summaries are available."),
                 tags$li("Data Preprocessing: Prepare your data for machine learning models by handling missing values, encoding categorical data, and splitting datasets into training and testing subsets."),
                 tags$li("Each tab is equipped with specific instructions and options to guide you through the process of machine learning data management.")
               ),
               p("Start by selecting a tab and following the prompts to load and process your data.")
             )
    ),# end of Overview
    
    tabPanel("Upload Data", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("Credit Card Fraud", "Upload your own file")),
                 conditionalPanel(
                   condition = "input.dataset == 'Upload your own file'",
                   fileInput("file", "Select your files:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                 ),
                 helpText("This page allows you to upload data. You have 2 options for uploading data:",
                          tags$br(),tags$br(),
                          "1.Use the default dataset provided on the website.", 
                          tags$br(),
                          "  No file upload or action is necessary for this option.", 
                          tags$br(),tags$br(),
                          "2.Upload a dataset from your desktop.", 
                          tags$br(),
                          "Please ensure your file meets the following requirements:", 
                          tags$br(),
                          "File format: CSV File(.csv) or Text File (.txt)")
               ),
               mainPanel(
                 dataTableOutput("data_preview")
               )
             )
    ),# end of Upload Data
    
    tabPanel("Data Exploration",icon = icon("chart-line"),
             titlePanel("Visualization"),
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.tabSelected == 'Scatterplot'",
                   selectInput("response", "Response Variable (Y)", choices = NULL), 
                   selectInput("explanatory", "Explanatory Variable (X)", choices = NULL),
                   sliderInput("shade", "Transparency Rate", min = 0, max = 1, value = 0.5, step = 0.1),
                   checkboxInput("marginal", "Marginal Distributions", value = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.tabSelected == 'Numeric Summary'",
                   h4("Understanding Numeric Summary"),
                   helpText("The numeric summary provides statistical measures such as mean, median, mode, and others for the selected response variable. Choose a response variable from the dropdown to view its statistics.")
                 ),
                 conditionalPanel(
                   condition = "input.tabSelected == 'Histogram'",
                   selectInput("var", "Variable", choices = NULL), 
                   numericInput("bins", "Number of bins", min = 1, max = 50, step = 1, value = 10),
                   radioButtons("color", "Color of bins:", choices = list("Blue" = "blue", "Red" = "red", "Green" = "green"), selected = "blue"),
                   actionButton("click", "Submit")
                 )
               ),
               mainPanel(
                 tabsetPanel(id = "tabSelected",
                             tabPanel("Scatterplot", plotOutput("plot1")),
                             tabPanel("Histogram", plotOutput("plot2")),
                             tabPanel("Numeric Summary", dataTableOutput("result1"))
                             
                 )
               )
             )
    ),
    tabPanel("Data Preprocessing", icon = icon("edit"),
             sidebarLayout(
               sidebarPanel(
                 # Text Input for NA Conversion
                 textInput("na_text", "Handle Sentinel Value ", value = ""),
                 helpText("Enter Placeholder value (e.g., 'XNA', 'none') that you would like to convert to 'NA' in the dataset. Insert one value at a time."),
                 actionButton("submit_na", "Convert"),
                 
                 # Selection Input for Imputation Method
                 selectInput("impute_method", "Select Imputation Method",
                             choices = c("None" = "none",
                                         "Mean Imputation" = "mean",
                                         "Median Imputation" = "median",
                                         "Mode Imputation" = "mode",
                                         "KNN Imputation" = "knn",
                                         "Drop Observations with NAs" = "drop_na")),
                 helpText("Choose a method to handle missing data in the dataset."),
                 
                 # Checkbox for Dropping Features with High Missing Values
                 checkboxInput("drop_features", "Drop Features with High Missing Values", value = FALSE),
                 helpText("TODO: change this into a slider to control the threshold"),
                 
                 # Slider for Training Data Percentage
                 sliderInput("data_split", "Percentage of Training Data", 
                             min = 50, max = 90, value = 70, step = 5, post = "%"),
                 actionButton("submit_pre", "Apply")
               ),
               mainPanel(
                 textOutput("na_message"),
                 textOutput("impute_message"), # Updated output ID
                 textOutput("drop_message"),
                 dataTableOutput("data_preview2"),
                 dataTableOutput("data_preview_train"),
                 dataTableOutput("data_preview_test")
               )
             )
    )
    
    ,
    tabPanel("Model Exploration", icon = icon("sliders"),
             titlePanel("Construct Model"),
             mainPanel(
               helpText("STAT 3106: Applied Machine Learning - Final Project ......")
             )
    ),
    tabPanel("Model Evaluation", icon = icon("sliders"),
             titlePanel("Construct Model"),
             mainPanel(
               helpText("STAT 3106: Applied Machine Learning - Final Project ......")
             )
    ),
  )
)
#