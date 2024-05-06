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
library(dplyr)  # For data manipulation
library(recipes)
library(future)
library(shinycssloaders)
library(rsample)
library(DT)
library(tidymodels)
library(tidyverse)
library(ranger)
library(shinyjs)
library(xgboost)
library("kernlab")

data_initial <- read.csv("data/subset_application_data.csv", header = TRUE)

data_train <- reactiveVal()
data_test <- reactiveVal()

transformed_train <- reactiveVal()
transformed_test <- reactiveVal()

finalModel<- reactiveVal()
server <- function(input, output, session) {
  plan(multisession)
  
  #--------------------------- Upload Data ---------------------------
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
  
  #Render data pre_view
  output$data_preview <- DT::renderDT({
    datatable(
      File(),
      options = list(
        scrollY = TRUE,   
        scrollX = TRUE,
        paging = TRUE 
      )
    )
  })
  #--------------------------- Data Exploration ---------------------------
  # Update response and explanatory variable choices
  observeEvent(File(), {
    data = File()
    numerical_var = names(data)[sapply(data, is.numeric)]
    
    updateSelectInput(session, "response", choices = names(File()))
    updateSelectInput(session, "explanatory", choices = names(File()))
    updateSelectInput(session, "var", choices = numerical_var)
    updateSelectInput(session, "target", choices = names(File()))
    
  })
  
  # Render scatterplot
  output$plot1 <- renderPlot({
    # Load the data and ensure it is a dataframe
    data <- File()
    
    # Remove rows with NA values
    data_clean <- na.omit(data)
    
    # Use aes_string to specify variables and create the plot
    p <- ggplot(data_clean, aes_string(x = input$explanatory, y = input$response)) +
      geom_point(alpha = input$shade) +
      theme_minimal()
    
    # Add marginal histograms if selected
    if (input$marginal) {
      p <- ggMarginal(p, type = "histogram")
    }
    
    p  # Return the plot
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
  

  
  #--------------------------- Data Preprocessing ---------------------------
  
  reactive_dataset <- reactiveVal(data_initial)
  
  # TODO: this is still rendering the original file 
  
  output$data_preview2 <- DT::renderDT({
    datatable(
      File(),
      options = list(
        scrollY = TRUE,   # Sets the height of the scrollable area
        scrollX = TRUE,
        paging = TRUE       # Disables pagination, adjust as needed
      )
    )
  })
  

  
  
  observeEvent(input$submit_na, {
    req(input$na_text) # placeholder:unable to handle empty string at this moment
    value_to_convert <- input$na_text  # Use the text input directly
    data <- reactive_dataset()# Access the current dataset
    converted_count <- 0
    
    # Convert specific value to NA
    for (col_name in names(data)) {
      matching_indices <- which(data[[col_name]] == value_to_convert)
      data[[col_name]][matching_indices] <- NA
      converted_count <- converted_count + length(matching_indices)
    }
    if (converted_count > 0) {
      na_message <- sprintf("Converted all entries matching '%s' to NA across all columns.\n Total: %d entries modified.", value_to_convert, converted_count)
    } else {
      na_message <- sprintf("The input '%s' does not exist in the dataset.", value_to_convert)
    }
    
    # Update the reactive dataset with changes
    reactive_dataset(data)
    # Send the message to the UI
    output$na_message <- renderText({ na_message })
    # Update the data preview
    output$data_preview2 <- DT::renderDT({
      datatable(
        data,
        options = list(
          scrollY = TRUE,   # Sets the height of the scrollable area
          scrollX = TRUE,
          paging = TRUE       # Disables pagination, adjust as needed
        )
      )
    })
  })
  
  observeEvent(input$submit_drop, {
    data <- reactive_dataset()
    
    drop_message <- ""
    
    # Drop rows where the target column has NA values
    target_column <- input$target  # replace 'target' with the name of your target column
    data <- data[!is.na(data[[target_column]]), ]
    
    # Drop features based on the threshold set by the slider
    threshold <- input$drop_features / 100  # Convert percentage to proportion
    if (threshold > 0) {
      initial_features <- ncol(data)
      data <- data[, colSums(is.na(data)) / nrow(data) < threshold]
      dropped_features <- initial_features - ncol(data)
      drop_message <- sprintf("Dropped %d features with more than %d%% missing values.", dropped_features, input$drop_features)
    } else {
      drop_message <- "No feature dropping threshold set."
    }
    
    # Render the imputation method message
    output$drop_message <- renderText({ drop_message })
    # Update the reactive dataset with changes
    reactive_dataset(data)
    # Update the data preview
    output$data_preview2 <- DT::renderDT({
      datatable(
        data,
        options = list(
          scrollY = TRUE,   # Sets the height of the scrollable area
          scrollX = TRUE,
          paging = TRUE       # Disables pagination, adjust as needed
        )
      )
    })
  })
  
  observeEvent(input$submit_split, {
    data <- reactive_dataset()  # Fetch the current dataset
    
    # Ensure that there is data to split
    if (is.null(data)) {
      return()
    }
    
    # Use rsample to split the data
    split <- initial_split(data, prop = input$data_split / 100, strata = input$target)  # Split based on slider input
    
    # Assign training and test sets
    data_train(training(split))
    data_test(testing(split))
    
    transformed_train(training(split))
    transformed_test(testing(split))
    # Display the selected percentage of training data
    split_message <- sprintf("Selected training data percentage: %d%%", input$data_split)
    output$data_split_message <- renderText({ split_message })
  })
  
  # Render the training data table
  output$data_preview_train <- renderDT({
    
    datatable(
      transformed_train(),  # Use the reactive getter to access the data
      options = list(
        scrollY = "200px",   # Sets the height of the scrollable area
        scrollX = TRUE,
        paging = TRUE        # Enables pagination
      )
    )
  })
  
  # Render the test data table
  output$data_preview_test <- renderDT({
    print("Outputting Reactive Element for Test")
    print(head(data_test()))
    datatable(
      transformed_test(),  # Use the reactive getter to access the data
      options = list(
        scrollY = "200px",   # Sets the height of the scrollable area
        scrollX = TRUE,
        paging = TRUE        # Enables pagination
      )
    )
  })
  
  observeEvent(input$submit_pre, {
    train_data <- data_train()
    test_data <- data_test()
    
    train_data[[input$target]]<- factor(train_data[[input$target]], levels = c("0", "1"),
                       labels = c("Faud", "NonFaud"))
    test_data[[input$target]] <- factor(test_data[[input$target]], levels = c("0", "1"),
                                labels = c("Faud", "NonFaud"))
    
    # Start with checking data availability
    if (is.null(train_data) || is.null(test_data)) {
      showNotification("Training or test data is not available. Make sure you split the data before you apply preprocessing step.", type = "error")
      return()
    }
    
    # Initialize the message with basic information
    pre_message <- "Preprocessing steps applied: "
    
    # Initialize the recipe with the training data
    formula <- as.formula(paste(input$target, "~ ."))
    blueprint <- recipe(formula, data = train_data) %>%
      step_string2factor(all_nominal(), -all_outcomes())
    pre_message <- paste(pre_message, "Convert all nominal predictors to factors,")
    
    if (input$remove_zero_var) {
      blueprint <- blueprint %>% step_nzv(all_predictors())
      pre_message <- paste(pre_message, "Remove near zero variance predictors,")
    }
    
    # Imputation steps and messages
    blueprint <- switch(input$impute_method,
                        "mean" = { blueprint <-blueprint %>% step_impute_mean(all_numeric_predictors()); pre_message <- paste(pre_message, "Mean imputation for numeric predictors,"); blueprint },
                        "median" = { blueprint <-blueprint %>% step_impute_median(all_numeric_predictors()); pre_message <- paste(pre_message, "Median imputation for numeric predictors,"); blueprint },
                        "mode" = { blueprint <-blueprint %>% step_impute_mode(all_nominal_predictors()); pre_message <- paste(pre_message, "Mode imputation for nominal predictors,"); blueprint },
                        "knn" = { blueprint <-blueprint %>% step_impute_knn(all_predictors()); pre_message <- paste(pre_message, "KNN imputation for all predictors,"); blueprint },
                        "drop_na" = { train_data <<- na.omit(train_data); test_data <<- na.omit(test_data); pre_message <- paste(pre_message, "Drop all rows with NAs,"); blueprint },
                        blueprint # Default case to handle 'none' or unexpected input
    )
    
    if (input$normalize_data) {
      blueprint <- blueprint %>% step_normalize(all_numeric_predictors())
      pre_message <- paste(pre_message, "Normalize all numeric predictors,")
    }
    
    if (input$standardize_data) {
      blueprint <- blueprint %>% step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors())
      pre_message <- paste(pre_message, "Center and scale all numeric predictors,")
    }
    
    if (input$pca){
      # print("In the PCA category")
      blueprint <- blueprint %>% step_pca(all_numeric_predictors()) 
      pre_message <- paste(pre_message, "Performed PCA on all numeric predictors,")
      
    }
    
    # Finalize and apply the preprocessing blueprint
    blueprint<- blueprint%>%step_dummy(all_nominal(), -all_outcomes())
    
    blueprint_prep <- prep(blueprint, training = train_data)
    
    transformed_train<-(bake(blueprint_prep, new_data = train_data))
    transformed_test<-(bake(blueprint_prep, new_data = test_data))
    # print(head(transformed_train))
    # Removing trailing comma and space
    pre_message <- sub(",\\s*$", "", pre_message)
    
    # Output the preprocessing message
    output$pre_message <- renderText({ pre_message })
    
    transformed_train(transformed_train)
    transformed_test(transformed_test)
    
  })
  
  
  #--------------------------- Model Exploration ---------------------------
  output$resample_params <- renderUI({
    switch(input$resample_method,
           "Cross-validation" = numericInput("fold", "Number of Folds", min = 2, max = 10, value = 5),
           "Repeated CV" = tagList(
             numericInput("fold", "Number of Folds", min = 2, max = 10, value = 5),
             numericInput("repeats", "Number of Repeats", min = 1, max = 10, value = 1)
           ),
           "Bootstrap" = numericInput("n_boot", "Number of Bootstrap Replicates", min = 10, max = 1000, value = 100)
  
    )
  })
  
  output$tuning_params <- renderUI({
    switch(input$model_type,
           "Random Forest" = tagList(
             selectizeInput("mtry", "mtry (Number of Variables)", choices = as.character(1:20), options = list(create = TRUE), multiple = TRUE),
             selectInput("splitrule", "Splitting Rule", choices = c("Gini" = "gini", "ExtraTrees" = "extratrees", "Hellinger" = "hellinger")),
             selectizeInput("min_node_size", "Minimum Node Size", choices = as.character(1:10), options = list(create = TRUE), multiple = TRUE),
           ),
           "Support Vector Machine" = tagList(
             selectInput("kernel_type", "Kernel Type", choices = c("Linear" = "linear", "Polynomial" = "polynomial", "RBF" = "rbf")),
             
             conditionalPanel(
               condition = "input.kernel_type === 'linear'",
               selectizeInput("C_linear", "Regularization Parameter (C)", choices = as.character(seq(0.1, 10, by = 0.1)), options = list(create = TRUE), multiple = TRUE)
             ),
             
             conditionalPanel(
               condition = "input.kernel_type === 'polynomial'",
               tagList(
                 selectizeInput("C_poly", "Regularization Parameter (C)", choices = as.character(seq(0.1, 10, by = 0.1)), options = list(create = TRUE), multiple = TRUE),
                 selectizeInput("degree", "Degree", choices = as.character(1:5), options = list(create = TRUE), multiple = TRUE),
                 selectizeInput("scale_poly", "Scale", choices = as.character(seq(0.1, 2, by = 0.1)), options = list(create = TRUE), multiple = TRUE)
               )
             ),
             
             conditionalPanel(
               condition = "input.kernel_type === 'rbf'",
               tagList(
                 selectizeInput("C_rbf", "Regularization Parameter (C)", choices = as.character(seq(0.1, 10, by = 0.1)), options = list(create = TRUE), multiple = TRUE),
                 selectizeInput("sigma", "Sigma", choices = as.character(seq(0.1, 2, by = 0.1)), options = list(create = TRUE), multiple = TRUE)
               )
             )
           )
           ,
           "XGBoost" = tagList(
             selectizeInput("nrounds", "Number of Boosting Rounds", choices = as.character(1:100), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("max_depth", "Max Depth", choices = as.character(1:15), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("eta", "Learning Rate", choices = as.character(seq(0.01, 0.3, by = 0.01)), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("min_child_weight", "Min Child Weight", choices = as.character(1:10), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("subsample", "Subsample Ratio of Training Instances", choices = as.character(seq(0.1, 1, by = 0.1)), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("gamma", "Minimum Loss Reduction", choices = as.character(seq(0, 5, by = 0.1)), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("colsample_bytree", "Subsample Ratio of Columns", choices = as.character(seq(0.1, 1, by = 0.1)), options = list(create = TRUE), multiple = TRUE)
           ),
           "Artificial Neural Networks" = tagList(
             selectizeInput("size", "Number of Units in Hidden Layers", choices = as.character(1:100), options = list(create = TRUE), multiple = TRUE),
             selectizeInput("decay", "Weight Decay to Prevent Overfitting", choices = as.character(seq(0.0001, 0.01, by = 0.0001)), options = list(create = TRUE), multiple = TRUE)
           )
    )
  })
  
  observeEvent(input$train_model, {
    transformed_train = transformed_train()
    transformed_test = transformed_test()
    ######## Checking input ########
    
    # 1.Checking Data
    if (is.null(transformed_train)||is.null(transformed_test)) {
      showNotification("Preprocessing has not yet been applied. Go back to the Preprocessing step and make sure you preprocess the data.", type = "error")
      return()
    }
    
    # 2.Checking Resample inputs
    
    # Define a function to gather input details
    getInputDetails <- function(inputName) {
      inputValue <- input[[inputName]]
      inputType <- typeof(inputValue)
      return(paste(inputName, "value:", inputValue, ", type:", inputType))
    }
    
    # Initialize an empty vector to store input details
    inputDetails <- c()
    
    # Check if the required inputs for the selected resampling method are provided
    if (input$resample_method == "Cross-validation") {
      inputDetails <- c(inputDetails, getInputDetails("fold"))
    } else if (input$resample_method == "Repeated CV") {
      inputDetails <- c(inputDetails, getInputDetails("fold"), getInputDetails("repeats"))
    } else if (input$resample_method == "Bootstrap") {
      inputDetails <- c(inputDetails, getInputDetails("n_boot"))
    }
    
    # Construct notification message
    message <- paste("Resampling Method:", input$resample_method, "\n", 
                     "Input details:", paste(inputDetails, collapse = "\n"))
    
    # Validate required inputs
    if (any(sapply(inputDetails, function(detail) grepl("value: 0", detail)))) {
      showNotification(paste("Missing required inputs for", input$resample_method), type = "error")
      return()
    }
    
    # Show detailed notification and proceed
    showNotification(message, type = "message")
    # Checking Model inputs 
    reqInputs <- list()
    if (input$model_type == "Random Forest") {
      reqInputs <- c("mtry", "splitrule", "min_node_size")
    } else if (input$model_type == "Support Vector Machine") {
      # Check specific to the kernel type selected
      switch(input$kernel_type,
             "linear" = {
               reqInputs <- c("C_linear")
             },
             "polynomial" = {
               reqInputs <- c("C_poly", "degree", "scale_poly")
             },
             "rbf" = {
               reqInputs <- c("C_rbf", "sigma")
             }
      )
    } else if (input$model_type == "XGBoost") {
      reqInputs <- c("nrounds", "max_depth", "eta", "min_child_weight", "subsample", "gamma", "colsample_bytree")
    } else if (input$model_type == "Artificial Neural Networks") {
      reqInputs <- c("size", "decay")
    }
    
    # Validate required inputs
    missingInputs <- sapply(reqInputs, function(x) is.null(input[[x]]) || length(input[[x]]) == 0)
    if (any(missingInputs)) {
      missingNames <- names(missingInputs)[missingInputs]
      showNotification(paste("Missing required inputs:", paste(missingNames, collapse=", ")), type = "error")
      return()
    }
    
    ######## Seting up input ########
    trainingControl <- switch(input$resample_method,
                          "Cross-validation" = trainControl(method = "cv", number = input$fold,classProbs = TRUE, summaryFunction = twoClassSummary),
                          "Repeated CV" = trainControl(method = "repeatedcv", number = input$fold, repeats = input$repeats,classProbs = TRUE, summaryFunction = twoClassSummary),
                          "Bootstrap" = trainControl(method = "boot", number = input$n_boot,classProbs = TRUE, summaryFunction = twoClassSummary),
                          trainControl(method = "none")  # Default fallback
    )
    modelMethod <- switch(input$model_type,
                      "Random Forest" = "ranger",
                      "Support Vector Machine" = switch(input$kernel_type,"linear" = "svmLinear","polynomial" = "svmPoly","rbf" = "svmRadial"),
                      "XGBoost" = "xgbTree",
                      "Artificial Neural Networks" = "nnet"
    )
    tuningParams <- switch(input$model_type,
                          "Random Forest" = 
                            expand.grid(
                              mtry = as.numeric(input$mtry),
                              splitrule =input$splitrule,
                              min.node.size = as.numeric(input$min_node_size)
                            ),
                          "Support Vector Machine" = switch(
                              input$kernel_type,
                                "linear" = 
                                    expand.grid(C = as.numeric(input$C_linear)
                                ),
                                "polynomial" = 
                                    expand.grid(
                                      C = as.numeric(input$C_poly),
                                      degree = as.numeric(input$degree),
                                      scale = as.numeric(input$scale_poly)
                                ),
                                "rbf" = 
                                    expand.grid(
                                    C = as.numeric(input$C_rbf),
                                    sigma = as.numeric(input$sigma)
                                )
                           ),
                          "XGBoost" = 
                            expand.grid(
                              nrounds = as.numeric(input$nrounds),
                              max_depth = as.numeric(input$max_depth),
                              eta = as.numeric(input$eta),
                              min_child_weight = as.numeric(input$min_child_weight),
                              subsample = as.numeric(input$subsample),
                              gamma = as.numeric(input$gamma),
                              colsample_bytree = as.numeric(input$colsample_bytree)
                            ),
                          "Artificial Neural Networks" = 
                            expand.grid(
                               size = as.numeric(input$size),
                               decay = as.numeric(input$decay)
                             )
    )
    
    ######## Training ########
    formula <- as.formula(paste(input$target, "~ ."))
    if(input$model_type =="Random Forest"){
      modelFit <- train(
        formula,
        data = transformed_train,
        method = modelMethod,
        trControl = trainingControl,
        tuneGrid = tuningParams,
        metric = "ROC",
        num.trees = 50,#TODO: make this a input 
      )
      showNotification(paste("Training Complete."),type = "message",duration = NULL)
    }else if (input$model_type =="SVM"){
      modelFit <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        verbose = FALSE,
        tuneGrid = tuningParams,
        metric = "ROC"
      )
      showNotification(paste("Training Complete."),type = "message",duration = NULL)
    }else {
      modelFit <-train(
            formula,
            data = transformed_train, 
            method = modelMethod,
            trControl = trainingControl,
            tuneGrid = tuningParams,
            metric = "ROC"
      )
      showNotification(paste("Training Complete."),type = "message",duration = NULL)
    }
    ######## Final Model ########
    # Best model parameters with names
    output$bestParameters <- renderText({
      best_params <- modelFit$bestTune
      # Create a readable string of parameter names and their best values
      params_text <- sapply(names(best_params), function(x) {
        paste(x, "=", best_params[[x]], sep="")
      })
      paste("Best Parameters:", paste(params_text, collapse=", "))
    })
    
    if(input$model_type =="Random Forest"){
      final_model <- train(
        formula,
        data = transformed_train,
        method = modelMethod,
        trControl = trainingControl,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC",
        num.trees = 50,#TODO: make this a input 
      )
      showNotification(paste("Final model saved! Move on to the model eveluation for detailed summary"),type = "message",duration = NULL)
    }else if (input$model_type =="SVM"){
      final_model <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        verbose = FALSE,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC"
      )
      showNotification(paste("Final model saved! Move on to the model eveluation for detailed summary"),type = "message",duration = NULL)
    }else {
      final_model <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC"
      )
      showNotification(paste("Final model saved! Move on to the model eveluation for detailed summary"),type = "message",duration = NULL)
    }

    ######## Visulize Training process ########
    output$accuracyPlot <- renderPlot({
      ggplot(modelFit)
    })
  
    train_pred <- predict(final_model, newdata = transformed_train)
    test_pred <- predict(final_model, newdata = transformed_test)
    #train_pred_factor <- factor(train_pred, levels = levels(transformed_train$TARGET))
    train_conf_matrix <- confusionMatrix(train_pred, transformed_train[[input$target]])
    test_conf_matrix <- confusionMatrix(test_pred, transformed_test[[input$target]])
    
    #--------------------------- Model Evaluation ---------------------------
    # Graphical evaluation (example with ROC curve)
    # output$train_plot <- renderPlot({
    #   train_roc <- roc(response = transformed_train()$TARGET, predictor = train_pred[,2])
    #   plot(train_roc, main = "ROC Curve - Training Data")
    # })
    # output$test_plot <- renderPlot({
    #   test_roc <- roc(response = transformed_test()$TARGET, predictor = test_pred[,2])
    #   plot(test_roc, main = "ROC Curve - Testing Data")
    # })
    
    # Numerical evaluation
    # output$train_metrics <- renderDataTable({
    #   train_data <- as.data.frame(transformed_train())
    #   train_pred <- predict(finalModel, data = train_data, type = "response")
    #   confusionMatrix(data = factor(train_pred), reference = factor(train_data$TARGET))
    # })
    output$train_metrics <- renderText({
      train_output <- capture.output(print(train_conf_matrix))
      paste(train_output, collapse = "\n")
      
    })
    output$test_metrics <- renderText({
      test_output <- capture.output(print(test_conf_matrix))
      paste(test_output, collapse = "\n")
      
    })
    # # Model summary
    # output$modelSummary <- renderText({
    #   summary(modelFit$finalModel)
    # })
    # ggplot(modelFit)
  })
    
  # observeEvent(input$train_model, {
  #   transformed_train = transformed_train()
  # 
  #   # Define training control conditionally
  #   trainingControl <- switch(input$resample_method,
  #                             "Cross-validation" = trainControl(method = "cv", number = input$fold,classProbs = TRUE, summaryFunction = twoClassSummary),
  #                             "Repeated CV" = trainControl(method = "repeatedcv", number = input$fold, repeats = input$repeats,classProbs = TRUE, summaryFunction = twoClassSummary),
  #                             "Bootstrap" = trainControl(method = "boot", number = input$n_boot,classProbs = TRUE, summaryFunction = twoClassSummary),
  #                             trainControl(method = "none")  # Default fallback
  #   )
  # 

  # 
  #   # Get tuning parameters dynamically based on user input

  # 
  # 

  # 
  #   # Output the model's training results
  #   output$training_result <- DT::renderDT({
  #     datatable(data.frame(Predictions = predict(modelFit, newdata = transformed_train)))
  #   })
  # 
  # })
  
}# end of server
