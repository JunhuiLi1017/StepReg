# importFrom gridExtra grid.arrange
# importFrom dplyr %>% mutate
# importFrom summarytools dfSummary
# importFrom ggcorrplot ggcorrplot
# importFrom tidyr %>% 
# importFrom GGally ggpairs

require("shiny") || stop("unable to load shiny")
require("StepReg") || stop("unable to load StepReg")
require("gridExtra") || stop("unable to load gridExtra")
require("DT") || stop("unable to load DT")
require("shinythemes") || stop("unable to load shinythemes")
require("ggplot2") || stop("unable to load ggplot2")
require("dplyr") || stop("unable to load dplyr")
require("summarytools") || stop("unable to load summarytools")
require("ggcorrplot") || stop("unable to load ggcorrplot")
require("tidyr") || stop("unable to load tidyr")
require("GGally") || stop("unable to load GGally")

#source("bin/upload_or_select_dataset.R") #can not select example dataset
source("bin/plot_data_func.R")

# Define server logic
server <- function(input, output, session) {
  # Function to read uploaded dataset
  dataset <- reactive({
    req(input$example_dataset)
    
    if (input$example_dataset != " ") {
      # Read the selected example dataset
      data(CreditCard, package = 'AER')
      data(remission,package="StepReg")
      survival::lung %>%
        mutate(sex = factor(sex, levels = c(1,2))) %>% # make sex as factor
        na.omit() -> lung# get rid of incomplete records
      
      
      df <- switch(input$example_dataset,
                   "base::mtcars" = mtcars,
                   "StepReg::remission" = remission,
                   "survival::lung" = lung,
                   "AER::CreditCard" = CreditCard)
    }
    if (!is.null(input$upload_file)) {
      req(input$upload_file)
      # Read the uploaded file
      df <- read.table(input$upload_file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
    }
    
    # df <- upload_or_select_dataset(input$example_dataset, 
    #                                input$upload_file,
    #                                input$header,
    #                                input$sep,
    #                                input$quote)
    
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
    return(res)
  })
  
  observeEvent(input$run_analysis, {
    output$modelSelection <- renderPrint({
      withProgress(
        message = 'Calculation of variable selection in progress', 
        detail = "Please wait...",
        value = 0, 
        {
          # Perform the stepwise model selection
          stepwiseResult <- stepwiseModel()
          incProgress(1/2)
          stepwiseResult
        }
      )
    })
    
    # Display plots of stepwise regression results
    output$selectionPlot <- renderPlot({
      withProgress(
        message = 'Visualization of variable selecion in progress', 
        detail = "Please wait...",
        value = 0, 
        {
          # Perform the stepwise model selection
          plotList <- plot(stepwiseModel())
          grid.arrange(grobs = plotList)
          incProgress(1/2)
        }
      )
    })
    
    output$selectionPlotText <- renderText({
      "Visualization of Variable Selection:"
    })
    output$selectionStatText <- renderText({
      "Statistics of Variable Selection:"
    })
    
    shinyjs::enable("report")
  })
  
  # Output Data
  output$tbl = renderDataTable({
    req(dataset())
    DT::datatable(dataset(), options = list(scrollX = TRUE))
  })
  
  output$summaryText <- renderDataTable({
    DT::datatable(stat.desc(dataset()) %>% mutate_if(is.numeric,round,3))
  })
  
  # Render the appropriate summary based on the selected type
  output$summary <- renderPrint({
    summary_type = summarytools::dfSummary(dataset())
    # summary_type <- switch(input$summary_type,
    #                        "dfSummary" = summarytools::dfSummary(dataset()),
    #                        "base::summary" = summary(dataset()),
    #                        "base::str" = str(dataset()),
    #                        "pastecs::stat.desc" = pastecs::stat.desc(dataset()))
    summary_type
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "var_plot", choices = colnames(dataset()))
  })
  
  observeEvent(input$make_plot, {
    # Render plots based on user selection
    observeEvent(input$plot_type, {
      # Define plot rendering logic here
      req(input$var_plot)
      
      if (input$plot_type == "Pairs plot") {
        output$Plot <- renderPlot({
          withProgress(
            message = 'Plots in progress', 
            value = 0, 
            {
              plot_type <- plot_data_func(input$plot_type,input$var_plot,dataset())
              incProgress(1/2)
              plot_type
            })
        })
      } else {
        output$Plot <- renderPlot({
          withProgress(
            message = 'Plots in progress', 
            value = 0, 
            {
              plot_type <- plot_data_func(input$plot_type,input$var_plot,dataset())
              incProgress(1/2)
            })
          grid.arrange(grobs = plot_type)
        })
      }
    })
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("StepReg_report_",format(Sys.time(), "%Y%m%d%H%M%S"),".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("bin/report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(modelSelection = stepwiseModel())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  session$onSessionEnded(function() { stopApp() }) 
}