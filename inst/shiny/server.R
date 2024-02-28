# Define server logic
server <- function(input, output, session) {
  # Function to read uploaded dataset
  
  dataset <- reactive({
    req(input$file1)  # Ensure file is uploaded
    
    # Read the uploaded file
    df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    
    # Update select input for distribution plot
    updateSelectInput(session, "distribution_plot", choices = names(df))
    
    # Update select inputs based on regression type
    
    updateSelectInput(session, "dependent_linear", choices = names(df))
    updateSelectInput(session, "status", choices = names(df))
    updateSelectInput(session, "time", choices = names(df))
    updateSelectInput(session, "dependent_glm", choices = names(df))
    
    observeEvent(input$dependent_linear, {
      updateSelectInput(session, "independent", choices = setdiff(names(df), input$dependent_linear))
    })
    
    observeEvent(input$status, {
      updateSelectInput(session, "time", choices = setdiff(names(df), input$status))
    })
    
    observeEvent(c(input$status, input$time), {
      updateSelectInput(session, "independent", choices = setdiff(names(df), c(input$status, input$time)))
    })
    
    observeEvent(input$dependent_glm, {
      updateSelectInput(session, "independent", choices = setdiff(names(df), input$dependent_glm))
    })
    
    observeEvent(input$independent, {
      updateSelectInput(session, "include", choices = input$independent)
    })
    
    # Return the dataframe
    return(df)
  })
  
  # Perform stepwise regression based on uploaded dataset
  stepwiseModel <- reactive({
    req(dataset())
    if (input$non_intercept == "FALSE") {
      intercept <- 1
    } else {
      intercept <- 0
    }
    
    metric <- switch(
      input$type,
      "linear" = ifelse(length(input$dependent_linear) > 1, input$metric_multivariate_linear, input$metric_univariate_linear),
      "cox" = input$metric_glm_cox,
      "logit" = input$metric_glm_cox,
      "poisson" = input$metric_glm_cox,
      "Gamma" = input$metric_glm_cox
    )
    
    formula <- switch(
      input$type,
      "linear" = {
        if (length(input$dependent_linear) > 1) {
          formula <- as.formula(paste(paste0("cbind(", paste(input$dependent_linear, collapse = ","), ")", collapse = ""), "~", paste(c(intercept, input$independent), collapse = "+")))
        } else {
          formula <- as.formula(paste(input$dependent_linear, "~", paste(c(intercept, input$independent), collapse = "+")))
        }
      },
      "cox" = as.formula(paste("Surv(", input$time, ",", input$status, ") ~", paste(input$independent, collapse = "+"))),
      "logit" = as.formula(paste(input$dependent_glm, "~", paste(c(intercept, input$independent), collapse = "+"))),
      "poisson" = as.formula(paste(input$dependent_glm, "~", paste(c(intercept, input$independent), collapse = "+"))),
      "Gamma" = as.formula(paste(input$dependent_glm, "~", paste(c(intercept, input$independent), collapse = "+")))
    )
    
    res <- stepwise(
      formula = formula,
      data = dataset(),
      type = input$type,
      include = input$include,
      strategy = input$strategy,
      metric = metric,
      sle = input$sle,
      sls = input$sls,
      test_method_linear = input$Approx_F,
      test_method_glm = input$glm_test,
      test_method_cox = input$cox_test
    )
    return(res)
  })
  
  observeEvent(input$run_analysis, {
    # Define the action you want to perform when the button is clicked
    
    # For example, you can update a reactive expression or call a function
    # In this case, let's update the stepwiseModel reactive expression
    result <- stepwiseModel()
    
    # Display stepwise regression results
    output$modelSelection <- renderPrint({
      stepwiseModel()
    })
    
    # Display plots of stepwise regression results
    output$selectionPlot <- renderPlot({
      plotList <- plot(stepwiseModel())
      gridExtra::grid.arrange(grobs = plotList)
    })
  })
  
  # Output variable selector based on dataset columns
  output$variable_selector <- renderUI({
    if(is.null(dataset())) return()
    selectInput("selected_variable", "Select Variable for Summary:",
                choices = colnames(dataset()))
  })
  
  # Generate summary table based on selected variable
  output$summary_table <- renderDT({
    req(input$selected_variable)
    summary_data <- summary(dataset()[,input$selected_variable])
    summary_data <- as.data.frame(as.matrix(summary_data)) # Ensure summary is a data frame
    colnames(summary_data) <- input$selected_variable
    datatable(summary_data, 
              options = list(paging = FALSE, searching = FALSE, ordering = FALSE))
  })
  
  # Generate summary table based on selected variable
  output$summary_table <- renderDT({
    req(input$selected_variable)
    summary_data <- summary(dataset()[, input$selected_variable])
    summary_data <- as.data.frame(as.matrix(summary_data))
    colnames(summary_data) <- input$selected_variable
    datatable(summary_data, 
              options = list(paging = FALSE, 
                             searching = FALSE, 
                             ordering = FALSE))
  })
  
  output$contents <- renderTable({
    req(dataset())
    
    if(input$disp == "head") {
      return(head(dataset()))
    }
    else {
      return(dataset())
    }
    
  })
}