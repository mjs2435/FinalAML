library(shiny)
library(shinythemes)
library(shinycssloaders)
library(ggExtra)
library(data.table)

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Machine Learning Model Explorer: Interactive Analysis & Predictions"),
  
  sidebarLayout(
    sidebarPanel(
      # Data source and training set split percentage shown only on "Data Preview" tab
      conditionalPanel(
        condition = "input.mainTabset == 'DataPreview'",
        radioButtons("dataSource", "Data Source",
                     choices = list("Upload My Data" = "upload", 
                                    "Use Default Data" = "default"),
                     selected = "default"),
        sliderInput("trainSplit", "Training Set Split Percentage", 
                    min = 50, max = 90, value = 70, step = 5, 
                    post = "%"),
        selectInput('modelType', 'Select Model Type', 
                    choices = c('Decision Tree' = 'dt', 'Random Forest' = 'rf', 'SVM' = 'svm')),
        actionButton("trainBtn", "Train Model")
        
      ),
      
      # Model type selection shown on all tabs except "Data Preview"
      conditionalPanel(
        condition = "input.mainTabset != 'DataPreview'",
      ),
      
      
    ),
    
    mainPanel(
      tabsetPanel(id = "mainTabset", type = "tabs", 
                  tabPanel("Data Preview", div(style = 'overflow-x: scroll;', tableOutput("dataPreview")), value = "DataPreview"),
                  tabPanel("Plot", plotOutput("plotArea"), value = "Plot"),
                  tabPanel("Model Summary", verbatimTextOutput("modelSummary"), value = "ModelSummary"),
                  tabPanel("Predictions", tableOutput("predictions"), value = "Predictions")
                  
      )
    )
  )
)
