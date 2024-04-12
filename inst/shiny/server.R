source("utils.R")

# Define server logic
server <- function(input, output, session) {
  # Disable download button upon page load:
  shinyjs::disable("download")
  shinyjs::disable("downloadPlot")
  shinyjs::disable("download_process_plot")
  
  # Function to read uploaded dataset
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$example_dataset, {
    req(input$example_dataset)
    # Read the selected example dataset
    data(creditCard, package = 'StepReg')
    data(remission, package = "StepReg")
    data(mtcars)
    survival::lung %>%
      mutate(sex = factor(sex, levels = c(1, 2))) %>% 
      na.omit() -> lung # get rid of incomplete records
    
    
    df <- switch(input$example_dataset,
                 "mtcars (for Linear regression)" = mtcars,
                 "remission (for Logistic regression)" = remission,
                 "lung (for Cox regression)" = lung,
                 "creditCard (for Poisson regression)" = creditCard)
    
    dataset(df)
  })
  
  # Function to upload user custom dataset:
  observeEvent(c(input$upload_file, input$header, input$sep, input$quote), {
    req(input$upload_file)
    # Read the uploaded file
    tryCatch(
      df <- read.table(input$upload_file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote),
      error = function(e) {
        warning("An error occurred uploading dataset:", e$message)
        return(NULL)
      })
    dataset(df)
  })
  ## Allow user to specify the variable types
  # First set default variable types according to dataset
  observe({
    req(dataset())
    numeric_var <- names(dataset())[sapply(dataset(), class) == "numeric"]
    character_var <- names(dataset())[sapply(dataset(), class) == "character"]
    factor_var <- names(dataset())[sapply(dataset(), class) == "factor"]
    other_var <- names(dataset())[!(names(dataset()) %in% c(numeric_var, character_var, factor_var))]
    
    all_var <- names(dataset())
    
    updateSelectInput(session, "data_numeric", choices = all_var, selected = numeric_var)
    updateSelectInput(session, "data_character", choices = all_var, selected = character_var)
    updateSelectInput(session, "data_factor", choices = all_var, selected = factor_var)
    updateSelectInput(session, "data_other", choices = all_var, selected = other_var)
  })
  
  # Then, update the selectInput upon user changing any of the four
  observeEvent(input$data_numeric, {
    updateSelectInput(session, "data_character", selected = setdiff(input$data_character, input$data_numeric))
    updateSelectInput(session, "data_factor", selected = setdiff(input$data_factor, input$data_numeric))
    updateSelectInput(session, "data_other", selected = setdiff(input$data_other, input$data_numeric))})
  
  observeEvent(input$data_character, {
    updateSelectInput(session, "data_numeric", selected = setdiff(input$data_numeric, input$data_character))
    updateSelectInput(session, "data_factor", selected = setdiff(input$data_factor, input$data_character))
    updateSelectInput(session, "data_other", selected = setdiff(input$data_other, input$data_character))})
  
  observeEvent(input$data_factor, {
    updateSelectInput(session, "data_character", selected = setdiff(input$data_character, input$data_factor))
    updateSelectInput(session, "data_numeric", selected = setdiff(input$data_numeric, input$data_factor))
    updateSelectInput(session, "data_other", selected = setdiff(input$data_other, input$data_factor))})
  
  observeEvent(input$data_other, {
    updateSelectInput(session, "data_character", selected = setdiff(input$data_character, input$data_other))
    updateSelectInput(session, "data_factor", selected = setdiff(input$data_factor, input$data_other))
    updateSelectInput(session, "data_numeric", selected = setdiff(input$data_numeric, input$data_other))})
  
  # Next, create a new dataset reflecting column types accordingly:
  dataset_m <- reactive({
    req(dataset())
    df <- dataset()
    col_numeric <- input$data_numeric
    col_character <- input$data_character
    col_factor <- input$data_factor
    col_other <- input$data_other
    
    df[,col_factor] <- lapply(df[,col_factor], factor)
    df[,col_numeric] <- lapply(df[,col_numeric], as.numeric)
    df[,col_character] <- lapply(df[,col_character], as.character)
    return(df)
  })
  ## End of "Allow user to specify the variable types"

  # Update select inputs based on regression type:
  observe({
    req(dataset())
    
    # Update select input for distribution plot
    updateSelectInput(session, "distribution_plot", choices = names(dataset()))
    
    updateSelectInput(session, "dependent_linear", choices = names(dataset()))
    updateSelectInput(session, "status", choices = names(dataset()))
    updateSelectInput(session, "time", choices = names(dataset()))
    updateSelectInput(session, "dependent_glm", choices = names(dataset()))
    
    observeEvent(input$dependent_linear, {
      updateSelectInput(session, "independent", choices = setdiff(names(dataset()), input$dependent_linear))
    })
    
    observeEvent(input$status, {
      updateSelectInput(session, "time", choices = setdiff(names(dataset()), input$status))
    })
    
    observeEvent(c(input$status, input$time), {
      updateSelectInput(session, "independent", choices = setdiff(names(dataset()), c(input$status, input$time)))
    })
    
    observeEvent(input$dependent_glm, {
      updateSelectInput(session, "independent", choices = setdiff(names(dataset()), input$dependent_glm))
    })
    
    observeEvent(input$independent, {
      updateSelectInput(session, "include", choices = input$independent)
    })
  })

  # Enable run button if all required fields are specified by user:
  run_analysis_enabled <- reactive({
    ## input$type, status, time: no need to check as selectInput default to use the first one

    ## input$independent:
    if (length(input$independent) == 0) return(FALSE)
    ## input$strategy:
    if (length(input$strategy) == 0) return(FALSE)
    ## input$metric_xxx:
    if (input$type == "linear") {
      if ((length(input$metric_multivariate_linear) == 0) && 
          (length(input$metric_univariate_linear) == 0)) return(FALSE) 
    } else if (input$type %in% c("logit", "cox", "poisson", "gamma")) {
      if (length(input$metric_glm_cox) == 0) return(FALSE)
    } else {
      stop("input$metric_xxx: not a valid input$type!")
    }
    ## input$dependent:
    if (input$type == "linear") {
      if (length(input$dependent_linear) == 0) return(FALSE)
    } else if (input$type %in% c("logit", "poisson", "gamma")) {
      if (length(input$dependent_glm) == 0) return(FALSE)
    } else if (input$type == "cox") {
      # no need to check input$status and input$time as they have default
    } else {
      stop("input$dependent: not a valid input$type!")
    }
    return(TRUE)
  })
  
  exploratory_plot_enabled <- reactive({
    if (length(input$var_plot) == 0){
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  observe({
    if (run_analysis_enabled()) {
      shinyjs::enable("run_analysis")
    } else {
      shinyjs::disable("run_analysis")
    }
    
    if (exploratory_plot_enabled()) {
      shinyjs::enable("make_plot")
    } else {
      shinyjs::disable("make_plot")
    }
  })
  
  # Perform stepwise regression based on uploaded dataset
  stepwiseModel <- eventReactive(input$run_analysis, {
    disable("download")
    disable("download_process_plot")
    req(dataset_m())
    if (input$intercept == TRUE) {
      intercept <- 1
    } else {
      intercept <- 0
    }
    
    metric <- switch(
      input$type,
      "linear" = {
        if (length(input$dependent_linear) > 1) {
          input$metric_multivariate_linear
        } else {
          input$metric_univariate_linear
        }
      },
      "cox" = input$metric_glm_cox,
      "logit" = input$metric_glm_cox,
      "poisson" = input$metric_glm_cox,
      "gamma" = input$metric_glm_cox
    )
    rv$nmetric <- length(metric)
    
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
      "gamma" = as.formula(paste(input$dependent_glm, "~", paste(c(intercept, input$independent), collapse = "+")))
    )
    
    res <- stepwise(
      formula = formula,
      data = dataset_m(),
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
    process_plot <- plot(res)
    model_vote <- vote(res)
    results <- list(res, process_plot, model_vote)
    enable("download")
    enable("download_process_plot")
    results
  })

  observeEvent(input$strategy, {
    updateSelectInput(
      session, 
      "strategy_plot", 
      choices = input$strategy
    )
  })
  
  # Generate output and enable download button:
  output$modelSelection <- renderPrint(stepwiseModel()[[1]])
  
  nmetric <- reactive({
    metric <- switch(
      input$type,
      "linear" = {
        if (length(input$dependent_linear) > 1) {
          input$metric_multivariate_linear
        } else {
          input$metric_univariate_linear
        }
      },
      "cox" = input$metric_glm_cox,
      "logit" = input$metric_glm_cox,
      "poisson" = input$metric_glm_cox,
      "gamma" = input$metric_glm_cox
    )
    if (is.null(metric) || length(metric) == 0) {
      return(1)  # Assign 1 if no metric selected
    } else {
      return(length(metric))
    }
  })
  
  rv <- reactiveValues()
  output$process_plot <- renderPlot({
    # Get the selected strategy plot from the list of plots
    selected_plot <- stepwiseModel()[[2]][[input$strategy_plot]]["summary"]
    rv$summary_plot <- selected_plot
    selected_plot
  }, res =72, 
  width = function() { (320 * 2) }, 
  height = function() { (320) })
 
  output$detail_plot <- renderPlot({
    req(nmetric())  # Ensure nmetric is available before proceeding
    # Get the selected strategy plot from the list of plots
    selected_plot <- stepwiseModel()[[2]][[input$strategy_plot]]["detail"]
    rv$detail_plot <- selected_plot
    selected_plot
  }, res =96, 
  width = function() { (320 * 2) }, 
  height = function() { (320 * nmetric()) }) 
  
  output$selectionPlotText <- renderUI({
    HTML("<b>Visualization of Variable Selection:\n</b>")
  })
  output$selectionStatText <- renderText({
    HTML("<b>Statistics of Variable Selection:\n</b>")
  })
  output$modelVoteText <- renderText({
    HTML("<b>Model Selection by Vote Across All Combinations of Strategy and Metric:\n</b>")
  })
  
  output$modelVote <- renderDataTable({ 
    DT::datatable(stepwiseModel()[[3]], options = list(scrollX = TRUE))
  })
  # Output Data
  output$tbl <- renderDataTable({
    req(dataset_m())
    DT::datatable(dataset_m(), options = list(scrollX = TRUE))
  })

  # Render the appropriate summary based on the selected type
  observe({
    output$summary <- renderPrint({
      req(dataset_m())
      pdf(file = NULL)
      summarytools::dfSummary(dataset_m(),graph.col = FALSE)
    })
  })
  
  observe({
    req(dataset_m())
    updateSelectInput(session, "var_plot", choices = colnames(dataset_m()))
  })
  
  plot_data <- eventReactive(input$make_plot, {
    disable("downloadPlot")
    req(input$plot_type, input$var_plot)
    plot_type <- createPlot(input$plot_type, input$var_plot, dataset_m())
    if (input$plot_type == "Pairs plot") {
      plot_result <- plot_type
    } else {
      #grid.arrange(grobs = plot_type)
      plot_result <- plot_grid(plotlist = plot_type)
    }
    enable("downloadPlot")
    return(plot_result)
  })
  
  output$Plot <- renderPlot({
   plot_data()
  })
  
  # Render the error message in the main panel
  output$error_message <- renderText({
    error_message()  # Display the stored error message
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$plot_type, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_data(), device = "png")
    }
  )
  
  output$download_process_plot <- downloadHandler(
    filename = function() { paste(input$strategy_plot, '_selection_process.png', sep='') },
    content = function(file) {
      ggsave(file, plot = rv$summary_plot, device = "png")
    }
  )
  
  output$download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("StepReg_report_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file('shiny/report.Rmd', package='StepReg'), tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(modelSelection = stepwiseModel()[[1]], 
                     selectionPlot = stepwiseModel()[[2]],
                     modelVote = stepwiseModel()[[3]])
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  session$onSessionEnded(function() { stopApp() }) 
}
