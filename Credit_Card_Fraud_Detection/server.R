library(shiny)
library(caret)
library(rpart) # For Decision Tree
library(randomForest) # For Random Forest
library(e1071) # For SVM
library(shinythemes)
library(shinycssloaders)
library(ggExtra)
library(data.table)
library(ggplot2)
library(DT)

data_initial <- read.csv("data/application_data.csv", header = TRUE)

server <- function(input, output, session) {
  
  # Reactive expression to handle file upload or selection
  File <- reactive({
    if (input$dataset == 'Upload your own file') {
      req(input$file)
      File <- input$file
      df <- data.frame(rbindlist(lapply(File$datapath, fread), use.names = TRUE, fill = TRUE))
      return(df)
    } else {
      return(data_initial)
    }
  })
  
  # Update response and explanatory variable choices
  observeEvent(File(), {
    updateSelectInput(session, "response", choices = names(File()))
    updateSelectInput(session, "explanatory", choices = names(File()))
    updateSelectInput(session, "var", choices = names(File()))
  })
  
  # Render data preview with DT
  output$data_preview <- renderDT({
    datatable(File(), options = list(pageLength = 10))
  })
  
  # Render scatterplot
  output$plot1 <- renderPlot({
    p <- ggplot(data = File(), aes_string(x = input$explanatory, y = input$response)) +
      geom_point(alpha = input$shade) +
      theme_minimal()
    if (input$marginal) {
      p <- ggMarginal(p, type = "histogram")
    }
    p
  })
  
  # Render numeric summary table with DT
  output$result1 <- renderDT({
    summary_data <- summary(File()[[input$response]])
    datatable(data.frame(Measure = names(summary_data), Value = as.character(summary_data)), options = list(pageLength = 5))
  })
  
  # Histogram plot
  plot2 <- eventReactive(input$click, {
    ggplot(data = File(), aes_string(x = input$var)) +
      geom_histogram(binwidth = diff(range(File()[[input$var]]) / input$bins), fill = input$color, color = "black") +
      labs(x = input$var, y = "Frequency", title = "Histogram") +
      theme_minimal()
  })
  
  # Render histogram plot
  output$plot2 <- renderPlot({
    plot2()
  })
}
