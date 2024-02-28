# Load mtcars dataset
ui <- navbarPage(
  title = "StepReg: an R package for stepwise regression",
  
  # Define tabs within the navbar
  tabPanel("step1: uploading dataset", 
           # App title ----
           titlePanel("Uploading Files"),
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Select a file ----
               fileInput("file1", "Choose your File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head"),
               
               # Horizontal line ----
               # tags$hr(),
               # Distribution of selected variables
               # selectInput("distribution_plot",
               #             "Distribution Plot based on Selected Variables",
               #             choices = NULL,
               #             multiple = TRUE)
               
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               # Output: Data file ----
               tableOutput("contents")#,
               #plotOutput("distPlot")
               
             )
             
           )
  ),
  tabPanel("step2: stepwise analysis",
           sidebarLayout(
             sidebarPanel(
               # Select type (linear, logit, cox, poisson, or gamma)
               radioButtons("type", "Regression Type:",
                            choices = c("linear", 
                                        "logit",
                                        "cox", 
                                        "poisson",
                                        "Gamma"),
                            selected = "linear"),
               
               # Select dependent variable
               conditionalPanel(
                 condition = "input.type === 'cox'",
                 selectInput("status", "Status Variable:", choices = NULL),
                 selectInput("time", "Time Variable:", choices = NULL)
               ),
               
               conditionalPanel(
                 condition = "input.type === 'linear'",
                 selectInput("dependent_linear", "Dependent Variable:", choices = NULL, multiple = TRUE)
               ),
               
               conditionalPanel(
                 condition = "input.type === 'logit' || input.type === 'poisson' || input.type === 'Gamma'",
                 selectInput("dependent_glm", "Dependent Variable:", choices = NULL)
               ),
               
               # Select independent variables
               #uiOutput("independentVariables"),
               selectInput("independent", "Independent Variables:",
                           choices = NULL,
                           multiple = TRUE),
               
               selectInput("include", "Include Variables:",
                           choices = NULL,
                           multiple = TRUE),
               
               conditionalPanel(
                 condition = "input.type !== 'cox'",
                 selectInput("non_intercept", "Non Intercept:",
                             choices = c("TRUE", "FALSE"), 
                             selected = "FALSE")
               ),
               
               # Select method (forward, backward, or both)
               checkboxGroupInput("strategy", "Stepwise Strategy:",
                                  choices = c("forward", 
                                              "backward",
                                              "bidirection", 
                                              "subset"),
                                  selected = "bidirection"),
               
               # Select metric
               conditionalPanel(
                 condition = "input.type === 'linear' && input.dependent_linear.length == 1",
                 checkboxGroupInput("metric_univariate_linear", "Selection Metric:",
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
                 checkboxGroupInput("metric_multivariate_linear", "Selection Metric:",
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
                 checkboxGroupInput("metric_glm_cox", "Selection Metric:",
                                    choices = c("AIC", 
                                                "AICc",
                                                "HQ",
                                                "IC(1)",
                                                "IC(3/2)",
                                                "SBC",
                                                "SL"),
                                    selected = "AIC")
               ),
               
               # Display sliderInput for significant level only when SL is selected
               conditionalPanel(
                 condition = "input.type === 'linear' && input.metric_multivariate_linear.indexOf('SL') != -1",
                 
                 radioButtons("Approx_F", 
                              label = "Approx F test statistic:",
                              choices = c("Pillai", 
                                          "Wilks", 
                                          "Hotelling-Lawley", 
                                          "Roy"),
                              selected = "Pillai")
               ),
               
               conditionalPanel(
                 condition = "input.type === 'logit' || input.type === 'Gamma' || input.type === 'poisson'",
                 radioButtons("glm_test", 
                              label = "Test Method",
                              choices = c("Rao", 
                                          "LRT"),
                              selected = "Rao")
               ),
               
               conditionalPanel(
                 condition = "input.type === 'cox'",
                 radioButtons("cox_test", 
                              label = "Test Method",
                              choices = c('efron',
                                          'breslow',
                                          'exact'),
                              selected = "efron")
               ),
               
               # Display sliderInput for significant level only when SL is selected
               conditionalPanel(
                 condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
                 sliderInput("sle", 
                             label = "significant level for entry",
                             min = 0, 
                             max = 1,
                             value = 0.05)
               ),
               
               conditionalPanel(
                 condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
                 sliderInput("sls", 
                             label = "significant level for stay", 
                             min = 0, 
                             max = 1, 
                             value = 0.05)
               ),
               
               conditionalPanel(
                 condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
                 sliderInput("sle", 
                             label = "significant level for entry",
                             min = 0, 
                             max = 1,
                             value = 0.05)
               ),
               
               conditionalPanel(
                 condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
                 sliderInput("sls", 
                             label = "significant level for stay", 
                             min = 0, 
                             max = 1, 
                             value = 0.05)
               ),
               
               conditionalPanel(
                 condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
                 sliderInput("sle", 
                             label = "significant level for entry",
                             min = 0, 
                             max = 1,
                             value = 0.05)
               ),
               
               conditionalPanel(
                 condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
                 sliderInput("sls", 
                             label = "significant level for stay", 
                             min = 0, 
                             max = 1, 
                             value = 0.05)
               ),
               
               actionButton("run_analysis", "Run Analysis")
               
             ),
             
             mainPanel(
               verbatimTextOutput("modelSelection"),
               plotOutput("selectionPlot")
             )
           )
  )
)