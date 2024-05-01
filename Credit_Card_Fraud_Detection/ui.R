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
                          "No file upload or action is necessary for this option.", 
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
    
    # #TODO: make a better design for this tab
    # #1. different visulization method for catagorical data
    # tabPanel("Data Exploration",icon = icon("chart-line"),
    #          titlePanel("Visualization"),
    #          sidebarLayout(
    #            sidebarPanel(
    #              conditionalPanel(
    #                condition = "input.tabSelected == 'Scatterplot'",
    #                selectInput("response", "Response Variable (Y)", choices = NULL), 
    #                selectInput("explanatory", "Explanatory Variable (X)", choices = NULL),
    #                sliderInput("shade", "Transparency Rate", min = 0, max = 1, value = 0.5, step = 0.1),
    #                checkboxInput("marginal", "Marginal Distributions", value = FALSE)
    #              ),
    #              conditionalPanel(
    #                condition = "input.tabSelected == 'Numeric Summary'",
    #                h4("Understanding Numeric Summary"),
    #                helpText("The numeric summary provides statistical measures such as mean, median, mode, and others for the selected response variable. Choose a response variable from the dropdown to view its statistics.")
    #              ),
    #              conditionalPanel(
    #                condition = "input.tabSelected == 'Histogram'",
    #                selectInput("var", "Variable", choices = NULL), 
    #                numericInput("bins", "Number of bins", min = 1, max = 50, step = 1, value = 10),
    #                radioButtons("color", "Color of bins:", choices = list("Blue" = "blue", "Red" = "red", "Green" = "green"), selected = "blue"),
    #                actionButton("click", "Submit")
    #              )
    #            ),
    #            mainPanel(
    #              tabsetPanel(id = "tabSelected",
    #                          tabPanel("Scatterplot", plotOutput("plot1")),
    #                          tabPanel("Histogram", plotOutput("plot2")),
    #                          tabPanel("Numeric Summary", dataTableOutput("result1"))
    #                          
    #              )
    #            )
    #          )
    # ),
    
    
    tabPanel("Data Preprocessing", icon = icon("edit"),
             sidebarLayout(
               sidebarPanel(
                 
                 # Text Input for NA Conversion
                 textInput("na_text", "Handle Sentinel Value ", value = "XNA"),#TODO:value = ""
                 helpText("Enter Placeholder value (e.g., 'XNA', 'none') that you would like to convert to 'NA' in the dataset. Insert one value at a time.Note:Empty entries will be converted to NA directly during model building."),
                 actionButton("submit_na", "Convert"),
                 
                 # Checkbox for Dropping Features with High Missing Values
                 sliderInput("drop_features", 
                             "Threshold for Dropping Features (%)", 
                             min = 30, max = 100, value = 30, step = 10, 
                             post = "%"),
                 actionButton("submit_drop", "Drop"),
                 # Slider for Training Data Percentage
                 sliderInput("data_split", "Training Data (%)", 
                             min = 50, max = 90, value = 70, step = 5, post = "%"),
                 selectInput("target", "Target (Y)", choices = NULL),#TODO:choices = NULL
                 actionButton("submit_split", "Split"),
                 
                 # Selection Input for Imputation Method
                 selectInput("impute_method", "Select Imputation Method",
                             choices = c(
                                         "KNN Imputation" = "knn",
                                         "Mean Imputation" = "mean",
                                         "Median Imputation" = "median",
                                         "Mode Imputation" = "mode",
                                         "Drop Observations with NAs" = "drop_na",
                                         "None" = "none")),
                 
                 #helpText("Choose an imputation method to handle missing data in the dataset."),
                 
                 # Checkboxes for Additional Preprocessing Options
                 #checkboxInput("remove_outliers", "Remove Outliers", value = FALSE),
                 checkboxInput("normalize_data", "Normalize Variables", value = TRUE),
                 checkboxInput("standardize_data", "Standardize Variables", value = TRUE),
                 checkboxInput("remove_zero_var", "Remove Near Zero Variance Variables", value = TRUE),
                 actionButton("submit_pre", "Apply"),
                 
                 
               ),
               mainPanel(
                 textOutput("na_message"),
                 textOutput("pre_message"),
                 textOutput("drop_message"),
                 dataTableOutput("data_preview2"),
                 tags$h3("Training Data", style = "font-weight: bold"),
                 dataTableOutput("data_preview_train"),
                 tags$h3("Testing Data", style = "font-weight: bold"),
                 dataTableOutput("data_preview_test")
               )
             )
    ),
    
    #cur
    tabPanel("Training", icon = icon("sliders"),
             titlePanel("Training"),
             sidebarLayout(
               sidebarPanel(
                 h3("Resampling Methods", style = "color: #337ab7;"),
                 selectInput("resample_method", "Choose Resampling Method",
                             choices = c("Cross-validation", "Bootstrap", "Repeated CV"),
                             selected = "Repeated CV"),
                 uiOutput("resample_params"),
                 
                 hr(),  # Horizontal line to visually separate sections
                 
                 h3("Hyperparameter Tuning", style = "color: #337ab7;"),
                 selectInput("model_type", "Choose Model for Tuning",
                             choices = c("Random Forest", "Support Vector Machine", "XGBoost", "Artificial Neural Networks"),
                             selected = "Random Forest"),
                 uiOutput("tuning_params"),
                 actionButton("train_model", "Start Training", icon = icon("play"))
               ),
               mainPanel(
                 tabsetPanel(
                   #tabPanel("Resampling Output", textOutput("resampling_output")),
                   #tabPanel("Tuning Output", textOutput("tuning_output")),
                   tabPanel("Training result", plotOutput("accuracyPlot"),verbatimTextOutput("bestAccuracy"),verbatimTextOutput("bestParameters"), verbatimTextOutput("modelSummary"))
                 )
               )
             )
    ),
    tabPanel("Model Evaluation", icon = icon("chart-bar"),
             tabsetPanel(
               tabPanel("Graphical Evaluation", 
                        tags$h3(strong("Training Result")),
                        plotOutput("train_plot"),
                        tags$h3(strong("Testing Result")),
                        plotOutput("test_plot")
               ),
               tabPanel("Numerical Evaluation", 
                        tags$h3(strong("Training Result")),
                        verbatimTextOutput("train_metrics"),
                        tags$h3(strong("Testing Result")),
                        verbatimTextOutput("test_metrics")
               )
             )
    ),
    selected = "Training"
  )
)
#