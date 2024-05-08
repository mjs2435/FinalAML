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
library(vip)
# my directory: "C:/Users/mjstr/Desktop/Git Repository Final Python Project/FinalAML/Credit_Card_Fraud_Detection"
# to deploy: 
# library(rsconnect)
# rsconnect::setAccountInfo(name='mjstr',token='761AA5873F75D14E28468EA1AFEF1D07',secret='GHErb0NTesBMQiGCD2nylXB0QrLRoe4nqqoROoAs')
# deployApp("C:/Users/mjstr/Desktop/Git Repository Final Python Project/FinalAML/Credit_Card_Fraud_Detection")
data_initial <- read.csv("data/subset_application_data.csv", header = TRUE)

data_train <- reactiveVal()
data_test <- reactiveVal()

transformed_train <- reactiveVal()
transformed_test <- reactiveVal()

finalModel<- reactiveVal()

prob_yes <- function(object, newdata) {                        # wrapper function for SVM VIP below
  
  predict(object, newdata = newdata, type = "prob")[, "Positive"]
  
}

n_dist_grid <- function(grid) {
  unique_ls = sapply(grid, n_distinct)
  return(sum(unique_ls > 1))
  
  
}

server <- function(input, output, session) {
  plan(multisession)
  
  #--------------------------- Upload Data ---------------------------
  # Reactive expression to handle file upload or selection
  
  File <- reactive({
    transformed_train(NULL)
    transformed_test(NULL)
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
    all_names = sapply(File(), class)
    num_names = names(File())[all_names == "numeric"]
    #only make target options with binary classification
    
    count_unique = sapply(File(), n_distinct)
    bin_class = names(File())[count_unique == 2]
    
    
    
    updateSelectInput(session, "response", choices = num_names)
    updateSelectInput(session, "explanatory", choices = num_names)
    updateSelectInput(session, "var", choices = numerical_var)
    updateSelectInput(session, "target", choices = bin_class)
    
  })
  
  # Render scatterplot
  output$plot1 <- renderPlot({
    # Load the data and ensure it is a dataframe
    data <- File()
    
    # Remove rows with NA values
    data_clean <- na.omit(data)
    
    # Use aes_string to specify variables and create the plot
    # p <- ggplot(data_clean, aes_string(x = input$explanatory, y = input$response)) +
    #   geom_point(alpha = input$shade) +
    #   theme_minimal()
    if (input$explanatory != ""){
      p = plot(x=data_clean[[input$explanatory]], y = data_clean[[input$response]], xlab = input$explanatory, ylab = input$response)
    }
    else(return())
    
    # Add marginal histograms if selected
    # if(input$marginal){
    #   p <- ggMarginal(p, type = "histogram")
    # }
    
    p  # Return the plot
  })
  
  # Histogram plot
  plot2 <- eventReactive(input$click, {
    data_to_plot = File()[[input$var]]
    data_no_na = data_to_plot[!is.na(data_to_plot)]
    if (length(data_no_na) == 0 | sd(data_no_na) == 0){
      bw = 1
      
    } else {
      bw = diff(range(data_no_na) / input$bins)
    }
    ggplot(data = File(), aes_string(x = input$var)) +
      geom_histogram(binwidth = bw, fill = input$color, color = "black") +
      labs(x = input$var, y = "Frequency", title = "Histogram") +
      theme_minimal()
  })
  
  # Render histogram plot
  output$plot2 <- renderPlot({
    plot2()
  })
  

  
  #--------------------------- Data Preprocessing ---------------------------
  
  reactive_dataset <- reactiveVal(data_initial)
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
    ##### do na stuff
    req(input$na_text) # placeholder:unable to handle empty string at this moment
    value_to_convert <- input$na_text  # Use the text input directly
    data <- File() # Access the current dataset
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
        options = list(scrollY = TRUE, scrollX = TRUE, paging = TRUE)
      )
    })
    ##############################
    
    # do drop stuff #
    
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

    ####################
    
    # do split stuff #
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
    ##############################
    
    # do preprocessing stuff
    
    train_data <- data_train()
    test_data <- data_test()
    
    two_levels = unique(train_data[[input$target]])
    
    train_data[[input$target]]<- factor(train_data[[input$target]], levels = two_levels,
                       labels = c("Negative", "Positive"))
    test_data[[input$target]] <- factor(test_data[[input$target]], levels = two_levels,
                                labels = c("Negative", "Positive"))
    
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
    } else {
      blueprint <- blueprint %>% step_zv(all_predictors())
      pre_message <- paste(pre_message, "Remove equal to zero variance predictors,")
    }
    
    # Imputation steps and messages
    blueprint <- switch(input$impute_method,
                        "mean_mode" = { blueprint <-blueprint %>% step_impute_mean(all_numeric_predictors())%>%step_impute_mode(all_nominal_predictors()); pre_message <- paste(pre_message, "Mean imputation for numeric predictors,Mode imputation for nominal predictors,"); blueprint },
                        "median_mode" = { blueprint <-blueprint %>% step_impute_median(all_numeric_predictors()) %>% step_impute_mode(all_nominal_predictors()); pre_message <- paste(pre_message, "Median imputation for numeric predictors,Mode imputation for nominal predictors,"); blueprint },
                        "knn" = { blueprint <-blueprint %>% step_impute_knn(all_predictors()); pre_message <- paste(pre_message, "KNN imputation for all predictors,"); blueprint }
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
      blueprint <- blueprint %>% step_pca(all_numeric_predictors()) 
      pre_message <- paste(pre_message, "Performed PCA on all numeric predictors,")
      
    }
    
    # Finalize and apply the preprocessing blueprint
    blueprint<- blueprint%>%step_dummy(all_nominal(), -all_outcomes())
    
    blueprint_prep <- prep(blueprint, training = train_data)
    
    transformed_train<-(bake(blueprint_prep, new_data = train_data))
    transformed_test<-(bake(blueprint_prep, new_data = test_data))
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
             selectizeInput("mtry", "mtry (Number of Variables)", choices = as.character(1:min(ncol(transformed_train()) - 1, 20)), options = list(create = TRUE), multiple = TRUE),
             selectInput("splitrule", "Splitting Rule", choices = c("Gini" = "gini", "ExtraTrees" = "extratrees", "Hellinger" = "hellinger")),
             selectizeInput("min_node_size", "Minimum Node Size", choices = as.character(1:10), options = list(create = TRUE), multiple = TRUE),
             numericInput("num_tree", "Number of Trees", min = 1, max = 500, value = 50)
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
    
    output$accuracyPlot <- renderUI({
      if(n_dist_grid(tuningParams) == 0){
        showNotification(paste("Only one set of parameters chosen. For more plots, please choose multiple options from the drop down lists, eg mtry for RF. "), type = "warning")
        return()
      } else if(n_dist_grid(tuningParams) > 4) {
        showNotification(paste("Too many parameters varied for visualization. For plots, please vary less than 4 at once"), type = "warning")
        return()
      } else {
        return(withSpinner(plotOutput("plot")))
      }
    })
    
    output$plot <- renderPlot({      
    if(n_dist_grid(tuningParams) == 0){
      return()
    } else if(n_dist_grid(tuningParams) > 4) {
      return()
    } else {
      return(ggplot(modelFit))
    }

    })
    
    output$bestParameters <- renderText({
      best_params <- modelFit$bestTune
      # Create a readable string of parameter names and their best values
      params_text <- sapply(names(best_params), function(x) {
        paste(x, "=", best_params[[x]], sep="")
      })
      paste("Best Parameters:", paste(params_text, collapse=", "))
    })
    
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
        num.trees = input$num_tree,
        importance = 'impurity'
      )
    }else if (input$model_type =="Support Vector Machine"){
      modelFit <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        verbose = FALSE,
        tuneGrid = tuningParams,
        metric = "ROC"
      )
    }else {
      modelFit <-train(
            formula,
            data = transformed_train, 
            method = modelMethod,
            trControl = trainingControl,
            tuneGrid = tuningParams,
            metric = "ROC"
      )
    }
    
    ######## Final Model ########
    # Best model parameters with names
    if(input$model_type =="Random Forest"){
      final_model <- train(
        formula,
        data = transformed_train,
        method = modelMethod,
        trControl = trainingControl,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC",
        num.trees = input$num_tree,
        importance = "impurity"
        
      )
    }else if (input$model_type =="Support Vector Machine"){
      final_model <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        verbose = FALSE,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC"
      )
    }else {
      final_model <-train(
        formula,
        data = transformed_train, 
        method = modelMethod,
        trControl = trainingControl,
        tuneGrid = data.frame(modelFit$bestTune),
        metric = "ROC"
      )
    }
    
    showNotification(paste("Final model saved! Move on to the model eveluation for detailed summary"),type = "message")
    ######## Visulize Training process ########
    train_pred <- predict(final_model, newdata = transformed_train)
    test_pred <- predict(final_model, newdata = transformed_test)
    train_conf_matrix <- confusionMatrix(train_pred, transformed_train[[input$target]])
    test_conf_matrix <- confusionMatrix(test_pred, transformed_test[[input$target]])
    
    #--------------------------- Model Evaluation ---------------------------
    output$roc <- renderPlot({
      if (n_dist_grid(tuningParams) == 0){ # only 1 set of hyperparameters, no tuning
        showNotification(paste("Note there are no graphical evaluation plots because only 1 set of hyperparameters was chosen."),type = "message")
        return()
        
      } else if(n_dist_grid(tuningParams) == 1) { # i.e. there is only 1 variable which is being changed
        ggplot(modelFit, metric = "ROC")
      } else if(n_dist_grid(tuningParams) > 4){
        showNotification(paste("Note there are no graphical evaluation plots because more than 4 hyperparameters were varied."),type = "message")
        return()
      } else {
        plot(modelFit, metric = "ROC", plotType = "level")
      }
      
      
    })
    output$ses <- renderPlot({
      if (n_dist_grid(tuningParams) == 0){ # only 1 set of hyperparameters, no tuning
        return()
      } else if(n_dist_grid(tuningParams) == 1) { # i.e. there is only 1 variable which is being changed
        ggplot(modelFit, metric = "Sens")
      } else if(n_dist_grid(tuningParams) > 4){
        return()
      } else {
        plot(modelFit, metric = "Sens", plotType = "level")
      }
    })
    output$spec <- renderPlot({
      if (n_dist_grid(tuningParams) == 0){ # only 1 set of hyperparameters, no tuning
        return()
      } else if(n_dist_grid(tuningParams) == 1) { # i.e. there is only 1 variable which is being changed
        ggplot(modelFit, metric = "Spec")
      } else if(n_dist_grid(tuningParams) > 4){
        return()
      } else {
        plot(modelFit, metric = "Spec", plotType = "level")
      }
    })
    output$rocsd <- renderPlot({
      if (n_dist_grid(tuningParams) == 0){ # only 1 set of hyperparameters, no tuning
        return()
      } else if(n_dist_grid(tuningParams) == 1) { # i.e. there is only 1 variable which is being changed
        ggplot(modelFit, metric = "ROCSD")
      } else if(n_dist_grid(tuningParams) > 4){
        return()
      } else {
        plot(modelFit, metric = "ROCSD", plotType = "level")
      }
    })
    
    output$train_metrics <- renderText({
      train_output <- capture.output(print(train_conf_matrix))
      paste(train_output, collapse = "\n")
      
    })
    output$test_metrics <- renderText({
      test_output <- capture.output(print(test_conf_matrix))
      paste(test_output, collapse = "\n")
      
    })
    output$vipPlot <- renderPlot({
      if(input$model_type == "Support Vector Machine"){
        vip(final_model, method = "permute", train = transformed_train, target = input$target,
            metric = "roc_auc", reference_class = "Positive", pred_wrapper = prob_yes)
      }
      else {
        vip(final_model, num_features = 10)
      }
      
      
    })
  })#end of train model
}# end of server
