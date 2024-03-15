source("utils.R")

js <- "
$(document).ready(function() {
    $('.navbar .container-fluid .navbar-nav .dropdown .dropdown-menu').prepend(
        '<li><a href=\"https://mccbbioinfo.github.io/tutorials/StepReg\" target=\"_blank\">Tutorial</a></li>',
        '<li><a href=\"https://github.com/JunhuiLi1017/StepReg/issues/new\" target=\"_blank\">Report Bug</a></li>');
});
"

ui <- tagList(
  useShinyjs(),
  tags$script(HTML(js)),
  navbarPage(
    title = tags$a(href = "https://cran.r-project.org/web/packages/StepReg/index.html", "StepReg"),
    theme = shinythemes::shinytheme("flatly"),
    
    # Define tabs within the navbar
    tabPanel(
      
      title = "File", 
  
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
              div(style = "width: 100%;",
                  withSpinner(DT::dataTableOutput('tbl', width = 750))
                  )
            ), # Data dalam tabel
            tabPanel(
              "Summary",
              withSpinner(verbatimTextOutput("summary"))
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
                    withSpinner(plotOutput("Plot"))
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Analyze",
      # p("To perform stepwise regression, you need to first specify the type of 
      #   stepwise procedure, which involves determining the dependent variables by 
      #   defining the scope of both dependent and independent variables. Next, 
      #   select one or more selection strategies and metrics to guide the stepwise 
      #   regression process."),
      sidebarLayout(
        sidebarPanel(
          # Select type (linear, logit, cox, poisson, or gamma)
          selectInput(
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
          selectInput(
            "strategy", 
            "Stepwise Strategy:",
            choices = c("forward", 
                        "backward",
                        "bidirection", 
                        "subset"),
            selected = "bidirection",
            multiple = TRUE
          ),
          
          # Select metric
          conditionalPanel(
            condition = "input.type === 'linear' && input.dependent_linear.length == 1",
            selectInput(
              "metric_univariate_linear", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "BIC", 
                          "CP",
                          "HQ",
                          "IC(1)",
                          "IC(3/2)",
                          "SBC",
                          "SL",
                          "Rsq",
                          "adjRsq"),
              selected = "AIC",
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.type === 'linear' && input.dependent_linear.length > 1",
            selectInput(
              "metric_multivariate_linear", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "HQ",
                          "SL"),
              selected = "AIC",
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.type !== 'linear'",
            selectInput(
              "metric_glm_cox", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "HQ",
                          "IC(1)",
                          "IC(3/2)",
                          "SBC",
                          "SL"),
              selected = "AIC",
              multiple = TRUE
            )
          ),
          
          # Display sliderInput for significant level only when SL is selected
          conditionalPanel(
            condition = "input.type === 'linear' && input.metric_multivariate_linear.indexOf('SL') != -1",
            selectInput(
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
            selectInput(
              "glm_test", 
              label = "Test Method",
              choices = c("Rao", 
                          "LRT"),
              selected = "Rao"
            )
          ),
          
          conditionalPanel(
            condition = "input.type === 'cox'",
            selectInput(
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
          tags$div(
            style = "display: flex; justify-content: space-between;",
            actionButton("run_analysis", 
                         "Run", 
                         icon = icon("chart-line"), 
                         style = "width: 130px; font-size: 12px;"),
            downloadButton("report", 
                           "Download", 
                           icon = icon("download"),
                           style = "width: 130px; font-size: 12px;",
                           disabled = TRUE)
          )
        ),
        
        mainPanel(
          conditionalPanel(
            condition = "input.run_analysis",
            textOutput("selectionPlotText")
          ),
          withSpinner(plotOutput("selectionPlot")),
          conditionalPanel(
            condition = "input.run_analysis",
            textOutput("selectionStatText")
          ),
          withSpinner(verbatimTextOutput("modelSelection"))
        )
      )
    ),
    #https://stackoverflow.com/questions/55987238/add-external-hyperlink-to-tabpanel-or-navbarmenu-in-r-shiny
    #https://stackoverflow.com/questions/48307652/display-list-of-downloads-in-navbarmenu-in-shiny
    # empty line between Tutorial and Report Bug in ui
    
    navbarMenu(
      title = "Help",
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
