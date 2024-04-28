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



data_initial <- read.csv("data/testing.csv", header = TRUE)

data_train <- reactiveVal()
data_test <- reactiveVal()

transformed_train <- reactiveVal()
transformed_test <- reactiveVal()

server <- function(input, output, session) {
  
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
        scrollY = TRUE,   # Sets the height of the scrollable area
        scrollX = TRUE,
        paging = TRUE       # Disables pagination, adjust as needed
      )
    )
  })
  #--------------------------- Data Exploration ---------------------------
  # Update response and explanatory variable choices
  observeEvent(File(), {
    updateSelectInput(session, "response", choices = names(File()))
    updateSelectInput(session, "explanatory", choices = names(File()))
    updateSelectInput(session, "var", choices = names(File()))
    updateSelectInput(session, "target", choices = names(File()))
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
    
    # TODO:---not working
    if (value_to_convert == "EMPTY") {
      # Convert all empty or blank entries to NA
      for (col_name in names(data)) {
        is_blank <- data[[col_name]] == ""  # Check for empty strings
        data[[col_name]][is_blank] <- NA
        converted_count <- converted_count + sum(is_blank)
      }
      if (converted_count > 0) {
        na_message <- sprintf("Converted all empty entries to NA across all columns. Total: %d entries modified.", converted_count)
      } else {
        na_message <- "There are no empty entries in the dataset."
      }
    } else {
    #---not working
      
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
    
    # Display the selected percentage of training data
    split_message <- sprintf("Selected training data percentage: %d%%", input$data_split)
    output$data_split_message <- renderText({ split_message })
  })
  
  # Render the training data table
  output$data_preview_train <- renderDT({
    datatable(
      data_train(),  # Use the reactive getter to access the data
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
      data_test(),  # Use the reactive getter to access the data
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
    
    # Start with checking data availability
    if (is.null(train_data) || is.null(test_data)) {
      showNotification("Training or test data is not available. Make sure you split the data before you apply preprocessing step.", type = "error")
      return()
    }
    
    # Initialize the message with basic information
    pre_message <- "Preprocessing steps applied: "
    
    # Initialize the recipe with the training data
    blueprint <- recipe(~., data = train_data) %>%
      step_string2factor(all_nominal(), -all_outcomes())
    pre_message <- paste(pre_message, "Convert all nominal predictors to factors,")
    
    if (input$remove_zero_var) {
      blueprint <- blueprint %>% step_nzv(all_predictors())
      pre_message <- paste(pre_message, "Remove near zero variance predictors,")
    }
    
    # Imputation steps and messages
    blueprint <- switch(input$impute_method,
                        "mean" = { blueprint %>% step_impute_mean(all_numeric_predictors()); pre_message <- paste(pre_message, "Mean imputation for numeric predictors,"); blueprint },
                        "median" = { blueprint %>% step_impute_median(all_numeric_predictors()); pre_message <- paste(pre_message, "Median imputation for numeric predictors,"); blueprint },
                        "mode" = { blueprint %>% step_impute_mode(all_nominal_predictors()); pre_message <- paste(pre_message, "Mode imputation for nominal predictors,"); blueprint },
                        "knn" = { blueprint %>% step_impute_knn(all_predictors()); pre_message <- paste(pre_message, "KNN imputation for all predictors,"); blueprint },
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
    
    # Finalize and apply the preprocessing blueprint
    blueprint_prep <- prep(blueprint, training = train_data)
    transformed_train(bake(blueprint_prep, new_data = train_data))
    transformed_test(bake(blueprint_prep, new_data = test_data))
    
    # Removing trailing comma and space
    pre_message <- sub(",\\s*$", "", pre_message)
    
    # Output the preprocessing message
    output$pre_message <- renderText({ pre_message })
  })
  
  
  #--------------------------- Model Exploration ---------------------------
  # Dynamic UI for resampling parameters
  output$resample_params <- renderUI({
    switch(input$resample_method,
           "Cross-validation" = numericInput("fold", "Number of Folds", min = 2, max = 10, value = 5),
           "Repeated CV" = tagList(
             numericInput("fold", "Number of Folds", min = 2, max = 10, value = 5),
             numericInput("repeats", "Number of Repeats", min = 1, max = 10, value = 1)
           ),
           "Bootstrap" = numericInput("n_boot", "Number of Bootstrap Replicates", min = 10, max = 1000, value = 50),
           "Time Series Split" = numericInput("n_splits", "Number of Splits", min = 2, max = 20, value = 5)
    )
  })
  
  # Dynamic UI for tuning parameters using selectizeInput for multiple values
  output$tuning_params <- renderUI({
    switch(input$model_type,
           "XGBoost" = tagList(
             selectizeInput("max_depth", "Max Depth", choices = NULL, options = list(create = TRUE), multiple = TRUE),
             selectizeInput("subsample", "Subsample", choices = NULL, options = list(create = TRUE), multiple = TRUE),
             selectizeInput("gamma", "Gamma", choices = NULL, options = list(create = TRUE), multiple = TRUE)
           ),
           "Random Forest" = tagList(
             selectizeInput("mtry", "Number of Variables (mtry)", choices = NULL, options = list(create = TRUE), multiple = TRUE),
             selectizeInput("ntree", "Number of Trees", choices = NULL, options = list(create = TRUE), multiple = TRUE)
           ),
           "SVM" = tagList(
             selectizeInput("cost", "Cost (C)", choices = NULL, options = list(create = TRUE), multiple = TRUE),
             selectizeInput("gamma", "Gamma", choices = NULL, options = list(create = TRUE), multiple = TRUE)
           )
    )
  })
  
  
}# end of server
