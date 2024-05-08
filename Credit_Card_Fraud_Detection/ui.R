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
               h3("Key Features"),
               p("This Shiny app provides an integrated environment to upload, preprocess, explore, and model your data effectively using a variety of machine learning algorithms. It is tailored for ease of use and supports extensive customization to cater to both novice and experienced data scientists."),
               h4("1. Data Upload and Initial Exploration"),
               p("Users can upload their own dataset or use a pre-loaded dataset. Data is displayed in a dynamic table allowing for immediate preview and interaction."),
               h4("2. Data Preprocessing"),
               p("Functionalities to handle missing values, feature selection, and data splitting into training and test sets. Provides various data imputation methods and options for feature encoding and scaling."),
               h4("3. Exploratory Data Analysis"),
               p("Generate histograms and scatter plots to understand data distributions and relationships. Interactive controls allow users to select variables and adjust plot parameters."),
               h4("4. Model Training and Tuning"),
               p("Supports multiple machine learning models including Decision Trees, Random Forests, SVM, and Neural Networks. Customizable hyperparameters for each model type with options like cross-validation and bootstrapping for performance evaluation."),
               h4("5. Model Evaluation"),
               p("Displays confusion matrices and ROC curves for training and test datasets to assess model performance. Best model parameters are displayed upon training completion."),
               h4("6. User Interface Customizations"),
               p("Utilizes shinythemes for aesthetic enhancements. Responsive design elements such as conditional panels and notifications enhance user interaction."),
               h3("Technologies Used"),
               p("R Packages: shiny, caret, tidymodels, ggplot2, DT, among others, providing a robust backend for data manipulation and modeling."),
               p("Interactive UI: Leverages shinyjs for enhanced JavaScript interactions within the R Shiny environment."),
               h3("Target Audience"),
               p("Data Science practitioners and educators looking for a flexible tool to teach, learn, and apply machine learning concepts effectively."),
               h3("FAQ"),
               h4("What type of analysis does this website do? And how do I format my dataset to process it?"),
               p("Only binary classification is supported. To format your dataset, simply have your target column contain exactly two distinct values and mark your response as such on the Data Preprocessing page."),
               h4("I can't access the model training tab! Is the website broken?"),
               p("Probably, but not because of that. You will first need to go to the Data Preprocessing tab and click Apply after adding useful changes. The same applies for the Model Evaluation after training."),
               h4("I am not seeing any training plots for my choices of hyperparameters. How can I visualize my results?"),
               p("First, check that you have entered more than one set of hyperparameters for tuning. This can be done through the drop-down menus of each respective factor. Also confirm that less than 4 have been chosen to be tuned, especially on XGBoost.")
               
             )
    ),# end of Overview
    
    tabPanel("Upload Data", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("Credit Card Fraud", "Upload your own file")),
                 conditionalPanel(
                   condition = "input.dataset == 'Upload your own file'",
                   fileInput("file", "Select your files:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), onclick = "$(tab).addClass('disabled')
                                                                                                                                                $(tab_eval).addClass('disabled')"
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
    

    #1. different visualization method for catagorical data
    tabPanel("Data Exploration",icon = icon("chart-line"),
             titlePanel("Visualization"),
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.tabSelected == 'Scatterplot'",
                   selectInput("response", "Response Variable (Y)", choices = NULL),
                   selectInput("explanatory", "Explanatory Variable (X)", choices = NULL),
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

                 )
               )
             )
    ),
    
    
    tabPanel("Data Preprocessing", icon = icon("edit"),
             sidebarLayout(
               sidebarPanel(
                 
                 # Text Input for NA Conversion
                 textInput("na_text", "Handle Sentinel Value ", value = "XNA"),
                 helpText("Enter Placeholder value (e.g., 'XNA', 'none') that you would like to convert to 'NA' in the dataset. Insert one value at a time. Note:Empty entries will be converted to NA directly during model building."),
                 
                 # Checkbox for Dropping Features with High Missing Values
                 sliderInput("drop_features", 
                             "Threshold for Dropping Features (%)", 
                             min = 30, max = 100, value = 30, step = 10, 
                             post = "%"),
                 # Slider for Training Data Percentage
                 sliderInput("data_split", "Training Data (%)", 
                             min = 50, max = 90, value = 70, step = 5, post = "%"),
                 selectInput("target", "Target (Y)", choices = NULL),

                 # Selection Input for Imputation Method
                 selectInput("impute_method", "Select Imputation Method",
                             choices = c(
                                         "KNN Imputation" = "knn",
                                         "Mean & Mode Imputation" = "mean_mode",
                                         "Median & Median Imputation" = "median_mode")),
                 
                 #helpText("Choose an imputation method to handle missing data in the dataset."),
                 
                 # Checkboxes for Additional Preprocessing Options
                 
                 #checkboxInput("remove_outliers", "Remove Outliers", value = FALSE),
                 checkboxInput("normalize_data", "Normalize Variables", value = TRUE),
                 checkboxInput("standardize_data", "Standardize Variables", value = TRUE),
                 checkboxInput("remove_zero_var", "Remove Near Zero Variance Variables", value = TRUE),
                 checkboxInput("remove_zero_var", "Remove Near Zero Variance Variables", value = TRUE),
                 checkboxInput("pca", "Principal Component Analysis", value = TRUE),
                 actionButton("submit_pre", "Apply", onclick = "$(tab).removeClass('disabled')"),
                 
                 
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
             ),
              tags$script(
                 '
                 var tab = $(\'a[data-value="Training"]\').parent().addClass("disabled");
                 $(function(){
                   $(tab.parent()).on("click", "li.disabled", function(e) {
                     e.preventDefault();
                     return false;
                   });
                 });
                 '
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
                 actionButton("train_model", "Start Training", icon = icon("play"), onclick = "$(tab_eval).removeClass('disabled')"),
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Training result", uiOutput("accuracyPlot"),verbatimTextOutput("bestAccuracy"),verbatimTextOutput("bestParameters"), verbatimTextOutput("modelSummary"))
                 )
               )
             ),          
             tags$script(
               '
               var tab_eval = $(\'a[data-value="Model Evaluation"]\').parent().addClass("disabled");
               $(function(){
                 $(tab_eval.parent()).on("click", "li.disabled", function(e) {
                   e.preventDefault();
                   return false;
                 });
               });
               '
             )
    ),
    tabPanel("Model Evaluation", icon = icon("chart-bar"),
             titlePanel("Model Evaluation"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Below, we see confusion matrices for the test and train set
                          for the best model from the tuning process before. If evaluation metrics which are described below differ, that means overfitting
                          may have occurred, meaning it would be in your best interest to step down the complexity
                          of your model.
                          
                          ",
                          tags$br(),
                          tags$br(),
                          "
                          Going to the Variable Importance tab, we see the VIP plot, which gives us the most relevant variables for the best model
                          trained. These are named by the columns of your input dataset. A higher score closer to 100 means that if that variable were removed
                          from training, it would drop ROC the most.
                          ",
                          tags$br(),
                          tags$br(),
                          "
                          Finally, we come across the line plots (if we are only tuning one variable) or a heatmap 
                          (if we are tuning multiple) for every hyperparameter in the training result folder. 
                          For all metrics, higher is better.
                          ROC is the metric which was used to determine what model is best, and is a
                          measure of the model performance at all levels of class splitting. Sensitivity
                          measures accuracy on the positive class, and specificity is on the negative (as
                          indicated by lexicographic order of binary classes). ROCSD is similar to ROC, but corrects for
                          unbalanced datasets.
                          "),
               ),
               mainPanel(
                 tabsetPanel(
                   tabsetPanel(
                     tabPanel("Numerical Evaluation", 
                              tags$h3(strong("Training Set Result")),
                              verbatimTextOutput("train_metrics"),
                              tags$h3(strong("Testing Set Result")),
                              verbatimTextOutput("test_metrics")
                     ),
                     tabPanel("Variable Importance", plotOutput("vipPlot")),
                     tabPanel("Graphical Evaluation",
                              tags$h3(strong("Detailed Training Result")),
                              plotOutput("roc"),
                              plotOutput("ses"),
                              plotOutput("spec"),
                              plotOutput("rocsd"),
                              
                              # tags$h3(strong("Training Result")),
                              # plotOutput("train_plot"),
                              # tags$h3(strong("Testing Result")),
                              # plotOutput("test_plot")
                     )

                   )
                 )
               )
             )

    )
  )
)
#