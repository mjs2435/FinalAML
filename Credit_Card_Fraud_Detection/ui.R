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
             titlePanel("Overview: User Instructions"),
             mainPanel(
               helpText("STAT 3106: Applied Machine Learning - Final Project ......")
             )
    ),
    
    tabPanel("Upload Data", icon = icon("folder-open"),
             titlePanel("Upload Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Dataset:", choices = c("Credit Card Fraud Detection", "Upload your own file")),
                 conditionalPanel(
                   condition = "input.dataset == 'Upload your own file'",
                   fileInput("file", "Select your files:", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                 )
               ),
               mainPanel(
                 DT::DTOutput("data_preview")
               )
             )
    ),
    
    tabPanel("Data Exploration",
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
                             tabPanel("Numeric Summary", DT::DTOutput("result1"))
                             
                 )
               )
             )
    )
  )
)
