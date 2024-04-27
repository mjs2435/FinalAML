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


data_initial <- read.csv("data/subset_application_data.csv", header = TRUE)

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
   
  reactive_dataset <- reactiveVal(data_initial)
  
  observeEvent(input$submit_na, {
    req(input$na_text) # unable to handle empty string at this moment
    value_to_convert <- input$na_text  # Use the text input directly
    
    # Access the current dataset
    data <- reactive_dataset()
    
    converted_count <- 0
    #---not working 
    if (value_to_convert == "EMPTY") {
      # Convert all empty or blank entries to NA
      for (col_name in names(data)) {
        is_blank <- data[[col_name]] == ""  # Check for empty strings
        data[[col_name]][is_blank] <- NA
        converted_count <- converted_count + sum(is_blank)
      }
      if (converted_count > 0) {
        na_message <- sprintf("Converted all empty entries to NA across all columns.\n Total: %d entries modified.", converted_count)
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
    output$data_preview2 <- renderDataTable({
      datatable(
        File(),
        options = list(
          scrollY = TRUE,   # Sets the height of the scrollable area
          scrollX = TRUE,
          paging = TRUE       # Disables pagination, adjust as needed
        )
      )
    })
  })
  
  observeEvent(input$submit_pre, {
    data <- reactive_dataset()
    # Initialize messages for imputation and feature dropping
    impute_message <- ""
    drop_message <- ""
    
    # Determine which imputation method is selected and set a corresponding message
    if (input$impute_method == "none") {
      impute_message <- "No imputation will be applied to the dataset."
    } else if (input$impute_method == "mean") {
      impute_message <- "Applying Mean imputation to the dataset..."
    } else if (input$impute_method == "median") {
      impute_message <- "Applying Median imputation to the dataset..."
    } else if (input$impute_method == "mode") {
      impute_message <- "Applying Mode imputation to the dataset..."
    } else if (input$impute_method == "knn") {
      impute_message <- "Applying KNN imputation to the dataset..."
    } else if (input$impute_method == "drop_na") {
      impute_message <- "Dropping all observations with missing values from the dataset..."
    }
    
    # Render the imputation method message
    output$impute_message <- renderText({ impute_message })
    
    # Check if dropping features with high missing values is enabled
    if (input$drop_features) {
      drop_message <- "Dropping features with high missing values from the dataset..."
      output$drop_message <- renderText({ drop_message })
    }
    
    # Display the selected percentage of training data
    split_message <- sprintf("Selected training data percentage: %d%%", input$data_split)
    output$data_split_message <- renderText({ split_message })
    # Update the reactive dataset with changes
    reactive_dataset(data)
  })
  
  
  observeEvent(input$submit_pre, {
    data <- reactive_dataset()
    
    # Handle KNN imputation if checkbox is checked
    if (input$knn_impute) {
      knn_message <- sprintf("Applying Knn imputation to the dataset...")
      output$knn_message <- renderText({ knn_message })
    }
    
    # Drop features with high missing values if checkbox is checked
    if (input$drop_features) {
      drop_message <- sprintf("Dropping  to the dataset...")
      output$drop_message <- renderText({ drop_message })
    }
    
    # Adjust training data percentage based on slider value
    #training_percentage <- input$data_split / 100
    
    # Update the reactive dataset with changes
    #reactive_dataset(data)
  })
  
  
  
}# end of server
