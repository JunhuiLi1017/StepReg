library(shiny)
library(StepReg)
library(gridExtra)
library(DT)
library(shinythemes)
#library(pastecs)
library(ggplot2)
library(dplyr)
library(summarytools)
library(ggcorrplot)
library(tidyr)
library(GGally)


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
#library(gpairs)

# importFrom gridExtra grid.arrange
# importFrom dplyr %>% mutate
# importFrom summarytools dfSummary
# importFrom ggcorrplot ggcorrplot
# importFrom tidyr %>% 
# importFrom GGally ggpairs

# 

# https://statsandr.com/blog/descriptive-statistics-in-r/
# Load mtcars dataset
#source("bin/upload_or_select_dataset.R") #can not select example dataset
source("bin/plot_data_func.R")

ui <- navbarPage(
  
  title = tags$a(href = "https://cran.r-project.org/web/packages/StepReg/index.html", "StepReg"),
  theme = shinythemes::shinytheme("flatly"),
  
  # Define tabs within the navbar
  tabPanel(
    
    title = "File", 
    
    p("On this page, you can conduct exploratory data analysis. In the dataset 
      panel, you have the option to explore either an example dataset or upload 
      your own dataset. In the main panel, you can delve into the data itself, 
      examine summary statistics and descriptive analysis, and visualize the 
      dataset using a variety of plots."),
    
    # add title
    titlePanel("Dataset"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        selectInput(
          "example_dataset",
          "Select an example dataset",
          choices = c(
            "",
            "base::mtcars", 
            "StepReg::remission", 
            "survival::lung",
            "AER::CreditCard"),
          selected = NULL
        ),
        # Horizontal line ----
        tags$hr(),
        br(),
        # Input: Select a file ----
        fileInput(
          "upload_file", 
          "Or upload your data",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header 
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(
          "sep", 
          "Separator",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ","),
        
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = ''),
        
        # Horizontal line ----
        tags$hr()
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        tabsetPanel(
          
          tabPanel(
            "Data",
            DT::dataTableOutput('tbl', width = 750)
          ),
          
          #tabPanel("stat.desc()", dataTableOutput("summaryText")),
          
          tabPanel(
            "Summary",
            verbatimTextOutput("summary")
            # fluidPage(
            #   sidebarLayout(
            #     sidebarPanel(
            #       selectInput(
            #         "summary_type",
            #         "select type of summary:",
            #         choices = c("dfSummary",
            #                     "base::summary",
            #                     "base::str",
            #                     "pastecs::stat.desc"),
            #         selected = "dfSummary"
            #       ),
            #     ),
            #     verbatimTextOutput("str")
            #   )
            # )
          ),
          tabPanel(
            title = "Plot",
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  selectInput("plot_type",
                              "select plot type:",
                              choices = c("Bar plot",
                                          "Box plot",
                                          "Correlation plot",
                                          "Density plot",
                                          "Dot plot",
                                          "Pairs plot",
                                          "Histogram",
                                          "QQ plot",
                                          "Scatter and Line plot"),
                              selected = "Pairs plot"),
                  
                  #h5("select variables for the Plot:"),
                  selectInput("var_plot", 
                              "Select variables for the Plot:", 
                              choices = "", 
                              multiple = TRUE),
                  actionButton("make_plot", "Make plot")
                ),
                mainPanel(
                  plotOutput("Plot")
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Analysis",
    p("To perform stepwise regression, you need to first specify the type of 
      stepwise procedure, which involves determining the dependent variables by 
      defining the scope of both dependent and independent variables. Next, 
      select one or more selection strategies and metrics to guide the stepwise 
      regression process."),
    sidebarLayout(
      sidebarPanel(
        # Select type (linear, logit, cox, poisson, or gamma)
        radioButtons(
          "type",
          "Regression type:",
          choices = c("linear",
                      "logit",
                      "cox",
                      "poisson",
                      "Gamma"),
          selected = "linear"
        ),
        # Select dependent variable
        conditionalPanel(
          condition = "input.type === 'cox'",
          selectInput("status", "Status variable:", choices = NULL),
          selectInput("time", "Time Variable:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = "input.type === 'linear'",
          selectInput("dependent_linear", "Dependent variable:", choices = NULL, multiple = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.type === 'logit' || input.type === 'poisson' || input.type === 'Gamma'",
          selectInput("dependent_glm", "Dependent variable:", choices = NULL)
        ),
        
        selectInput(
          "independent",
          "Independent Variables:",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          "include", 
          "Include Variables:",
          choices = NULL,
          multiple = TRUE
        ),
        conditionalPanel(
          condition = "input.type !== 'cox'",
          checkboxInput(
            "intercept", 
            "Include Intercept",
            TRUE
          )
        ),
        
        # Select method (forward, backward, or both)
        checkboxGroupInput(
          "strategy", 
          "Stepwise Strategy:",
          choices = c("forward", 
                      "backward",
                      "bidirection", 
                      "subset"),
          selected = "bidirection"
        ),
        
        # Select metric
        conditionalPanel(
          condition = "input.type === 'linear' && input.dependent_linear.length == 1",
          checkboxGroupInput(
            "metric_univariate_linear", 
            "Selection Metric:",
            choices = c("AIC", 
                        "AICc",
                        "BIC", 
                        "Cp",
                        "HQ",
                        "IC(1)",
                        "IC(3/2)",
                        "SBC",
                        "SL",
                        "Rsq",
                        "adjRsq"),
            selected = "AIC")
        ),
        
        conditionalPanel(
          condition = "input.type === 'linear' && input.dependent_linear.length > 1",
          checkboxGroupInput(
            "metric_multivariate_linear", 
            "Selection Metric:",
            choices = c("AIC", 
                        "AICc",
                        "HQ",
                        "IC(1)",
                        "IC(3/2)",
                        "SBC",
                        "SL"),
            selected = "AIC")
        ),
        
        conditionalPanel(
          condition = "input.type !== 'linear'",
          checkboxGroupInput(
            "metric_glm_cox", 
            "Selection Metric:",
            choices = c("AIC", 
                        "AICc",
                        "HQ",
                        "IC(1)",
                        "IC(3/2)",
                        "SBC",
                        "SL"),
            selected = "AIC"
          )
        ),
        
        # Display sliderInput for significant level only when SL is selected
        conditionalPanel(
          condition = "input.type === 'linear' && input.metric_multivariate_linear.indexOf('SL') != -1",
          radioButtons(
            "Approx_F", 
            label = "Approx F test statistic:",
            choices = c("Pillai", 
                        "Wilks", 
                        "Hotelling-Lawley", 
                        "Roy"),
            selected = "Pillai"
          )
        ),
        
        conditionalPanel(
          condition = "input.type === 'logit' || input.type === 'Gamma' || input.type === 'poisson'",
          radioButtons(
            "glm_test", 
            label = "Test Method",
            choices = c("Rao", 
                        "LRT"),
            selected = "Rao"
          )
        ),
        
        conditionalPanel(
          condition = "input.type === 'cox'",
          radioButtons(
            "cox_test", 
            label = "Test Method",
            choices = c('efron',
                        'breslow',
                        'exact'),
            selected = "efron"
          )
        ),
        
        # Display sliderInput for significant level only when SL is selected
        conditionalPanel(
          condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
          sliderInput(
            "sle", 
            label = "significant level for entry",
            min = 0, 
            max = 1,
            value = 0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
          sliderInput(
            "sls", 
            label = "significant level for stay", 
            min = 0, 
            max = 1, 
            value = 0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
          sliderInput(
            "sle", 
            label = "significant level for entry",
            min = 0, 
            max = 1,
            value = 0.05
          )
        ),
        
        conditionalPanel(
          condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
          sliderInput(
            "sls", 
            label = "significant level for stay", 
            min = 0, 
            max = 1, 
            value = 0.05)
        ),
        
        conditionalPanel(
          condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
          sliderInput(
            "sle", 
            label = "significant level for entry",
            min = 0, 
            max = 1,
            value = 0.05)
        ),
        
        conditionalPanel(
          condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
          sliderInput(
            "sls", 
            label = "significant level for stay", 
            min = 0, 
            max = 1, 
            value = 0.05)
        ),
        actionButton("run_analysis", "Run Analysis", class = "btn-lg btn-success", icon = icon("chart-line")),
        tags$hr(),
        downloadButton("report", "Generate report", icon = icon("download"))
      ),
      
      mainPanel(
        textOutput("selectionStatText"),
        verbatimTextOutput("modelSelection"),
        textOutput("selectionPlotText"),
        plotOutput("selectionPlot")
      )
    )
  ),
  tabPanel(
    title = "Help", 
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Q & A",
        ),
        tabPanel(
          "Report Bug",
          tags$p("Please report bugs "),
          tags$a(href = "https://github.com/JunhuiLi1017/StepReg/issues/new", "here")
        ),
        tabPanel(
          "Citation",
          tags$p("If you think 'StepReg' R package is helpful to your research, please cite:"),
          tags$ul(
            tags$li("Junhui Li (2024). StepReg: Stepwise Regression Analysis. R package version 1.5.0, https://CRAN.R-project.org/package=StepReg")
          )
        )
      )
    )
  )
)

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
        message = 'Calculation in progress', 
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
        message = 'Calculation in progress', 
        value = 0.5, 
        {
          # Perform the stepwise model selection
          plotList <- plot(stepwiseModel())
          grid.arrange(grobs = plotList)
          incProgress(1)
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
            message = 'Calculation in progress', 
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
            message = 'Calculation in progress', 
            value = 0, 
            {
            plot_type <- plot_data_func(input$plot_type,input$var_plot,dataset())
           
            incProgress(1/2)
            
            })
          p <- grid.arrange(grobs = plot_type)
          print(p)
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

# Run the application
shinyApp(ui = ui, server = server)