library(shiny)
library(caret)
library(rpart) # For Decision Tree
library(randomForest) # For Random Forest
library(e1071) # For SVM

# Define server logic required for the app
server <- function(input, output) {
  
  # Reactive expression for handling data input
  data <- reactive({
    # Decide between uploaded data and default data
    if(input$dataSource == "upload") {
      req(input$dataFile)
      inFile <- input$dataFile
      df <- read.csv(inFile$datapath)
    } else {
      # Adjust the path to your default CSV file as necessary
      defaultFilePath <- "sample_data.csv"#TODO:change to the real dataset 
      df <- read.csv(defaultFilePath)
    }
    return(df)
  })
  
  output$dataPreview <- renderTable({
    df <- data() # This uses the 'data' reactive expression already defined
    if(is.null(df)) return()
    head(df, 10) # Return the first 10 rows of the dataset
  })
  
  # Observes when the 'Train Model' button is clicked
  observeEvent(input$trainBtn, {
    df <- data()
    if(is.null(df)) return()
    
    # Split data
    set.seed(123) # For reproducibility
    trainIndex <- createDataPartition(df[,ncol(df)], p = input$trainSplit / 100, list = FALSE)
    trainingData <- df[trainIndex, ]
    testingData <- df[-trainIndex, ]
    
    # Train model based on selected type
    model <- switch(input$modelType,
                    dt = rpart(as.formula(paste("y ~", paste(names(df)[-ncol(df)], collapse = "+"))), 
                               data = trainingData, method = "class"),
                    rf = randomForest(as.formula(paste("y ~", paste(names(df)[-ncol(df)], collapse = "+"))), 
                                      data = trainingData),
                    svm = svm(as.formula(paste("y ~", paste(names(df)[-ncol(df)], collapse = "+"))), 
                              data = trainingData))
    output$dataPreview <- renderTable({
    df <- data() # This uses the 'data' reactive expression already defined
    if(is.null(df)) return()
    head(df, 10) # Return the first 10 rows of the dataset
})
    
    # Plot the model (if applicable)
    output$plotArea <- renderPlot({
      plot(model)
    })
    
    # Model summary
    output$modelSummary <- renderPrint({
      summary(model)
    })
    
    # Predictions
    output$predictions <- renderTable({
      predictions <- predict(model, newdata = testingData)
      data.frame(Actual = testingData[,ncol(testingData)], Predicted = predictions)
    })
  })
}

