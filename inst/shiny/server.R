source("utils.R")

# Define server logic
server <- function(input, output, session) {
  # Disable download button upon page load:
  shinyjs::disable("download")
  shinyjs::disable("downloadPlot")
  
  # Function to read uploaded dataset
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$example_dataset, {
    req(input$example_dataset)
    # Read the selected example dataset
    data(creditCard, package = 'StepReg')
    data(remission, package = "StepReg")
    survival::lung %>%
      mutate(sex = factor(sex, levels = c(1, 2))) %>% 
      na.omit() -> lung # get rid of incomplete records
    
    
    df <- switch(input$example_dataset,
                 "base::mtcars" = mtcars,
                 "StepReg::remission" = remission,
                 "survival::lung" = lung,
                 "StepReg::creditCard" = creditCard)
    
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
    } else if (input$type %in% c("logit", "cox", "poisson", "Gamma")) {
      if (length(input$metric_glm_cox) == 0) return(FALSE)
    } else {
      stop("input$metric_xxx: not a valid input$type!")
    }
    ## input$dependent:
    if (input$type == "linear") {
      if (length(input$dependent_linear) == 0) return(FALSE)
    } else if (input$type %in% c("logit", "poisson", "Gamma")) {
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
    req(dataset())
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
    results <- list(res, plot(res))
    enable("download")
    results
  })

  # Generate output and enable download button:
  output$modelSelection <- renderPrint(stepwiseModel()[[1]])
  output$selectionPlot <- renderPlot({stepwiseModel()[[2]]})
  output$selectionPlotText <- renderUI({
    HTML("<b>Visualization of Variable Selection:</b>")
  })
  output$selectionStatText <- renderText({
    HTML("<b>Statistics of Variable Selection:</b>")
  })
  
  # Output Data
  output$tbl = renderDataTable({
    req(dataset())
    DT::datatable(dataset(), options = list(scrollX = TRUE))
  })

  # Render the appropriate summary based on the selected type
  observe({
    output$summary <- renderPrint({
      req(dataset())
      pdf(file = NULL)
      summarytools::dfSummary(dataset())
    })
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "var_plot", choices = colnames(dataset()))
  })
  
  plot_data <- eventReactive(input$make_plot, {
    disable("downloadPlot")
    req(input$plot_type, input$var_plot)
    plot_type <- createPlot(input$plot_type, input$var_plot, dataset())
    enable("downloadPlot")
    if (input$plot_type == "Pairs plot") {
      plot_type
    } else {
      #grid.arrange(grobs = plot_type)
      print(plot_grid(plotlist = plot_type))
    }
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
  
  output$download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("StepReg_report_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file('shiny/report.Rmd', package='StepReg'), tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(modelSelection = stepwiseModel()[[1]], 
                     selectionPlot = stepwiseModel()[[2]])
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  session$onSessionEnded(function() { stopApp() }) 
}
