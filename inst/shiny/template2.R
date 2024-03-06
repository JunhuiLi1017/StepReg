library(shiny)
library(StepReg)
library(gridExtra)
library(DT)
library(shinythemes)
library(pastecs)
library(ggplot2)
library(dplyr)
library(summarytools)
library(ggcorrplot)
library(tidyr)
library(GGally)
library(gpairs)

ui <- fluidPage(
  navbarPage(
    "My Shiny App",
    tabPanel("Tab 1"),
    tabPanel("Tab 2"),
    navbarMenu("File",
               tabPanel("Open an example dataset",
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
                            )
                          ),
                          # Main panel for displaying outputs ----
                          mainPanel(
                            
                            tabsetPanel(
                              
                              tabPanel(
                                "Data",
                                div(style = "width: 100%;",
                                    DT::dataTableOutput('tbl'))
                              ),# Data dalam tabel
                              
                              #tabPanel("stat.desc()", dataTableOutput("summaryText")),
                              
                              tabPanel(
                                "Summary",
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(
                                        "summary_type",
                                        "select type of summary:",
                                        choices = c("summarytools::dfSummary",
                                                    "base::summary",
                                                    "base::str",
                                                    "pastecs::stat.desc"),
                                        selected = "summarytools::dfSummary"
                                      ),
                                    ),
                                    verbatimTextOutput("str")
                                  )
                                )
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
                                      plotOutput("Plot"),
                                      textOutput("plotStatus")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
               ),
               tabPanel("Upload your dataset",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(
                              "file1", 
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
                                         selected = '')
                          ),
                          # Main panel for displaying outputs ----
                          mainPanel(
                            
                            tabsetPanel(
                              
                              tabPanel(
                                "Data",
                                div(style = "width: 100%;",
                                    DT::dataTableOutput('tbl'))
                              ),# Data dalam tabel
                              
                              #tabPanel("stat.desc()", dataTableOutput("summaryText")),
                              
                              tabPanel(
                                "Summary",
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(
                                        "summary_type",
                                        "select type of summary:",
                                        choices = c("summarytools::dfSummary",
                                                    "base::summary",
                                                    "base::str",
                                                    "pastecs::stat.desc"),
                                        selected = "summarytools::dfSummary"
                                      ),
                                    ),
                                    verbatimTextOutput("str")
                                  )
                                )
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
                                      plotOutput("Plot"),
                                      textOutput("plotStatus")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                        
               )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$example_dataset)  # Make sure an example dataset is selected
    
    # Load the selected example dataset
    df <- switch(input$example_dataset,
                 "base::mtcars" = mtcars,
                 "StepReg::remission" = StepReg::remission,
                 "survival::lung" = survival::lung,
                 "AER::CreditCard" = AER::CreditCard)
    
    # If an example dataset is selected, return it
    if (!is.null(df)) {
      return(df)
    }
    
    # If a file is uploaded, read the file and return the dataset
    if (!is.null(input$file1)) {
      req(input$file1)  # Ensure file is uploaded
      df <- read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      return(df)
    }
    
    # If no dataset is available, return NULL
    return(NULL)
  })
  
  # Output Data
  output$tbl = renderDataTable({
    req(dataset())
    DT::datatable(dataset())
  })
}

shinyApp(ui, server)