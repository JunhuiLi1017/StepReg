#install.packages("tidyverse")
library(shiny)
library(tidyverse)
library(shinydashboard)
library(MASS)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", multiple = FALSE, accept = ".csv"),
      br(),
      actionButton("analyzeBtn", "Statistical Analysis Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       h5("select variables for the boxplot:"),
                       radioButtons("disp", "Display",
                                    choices = c(Head = "head",
                                                All = "all"),
                                    selected = "head")
                     ),
                     mainPanel(
                       DT::dataTableOutput('tbl')
                     )
                    )
                  )
                ), # Data dalam tabel
        tabPanel("Summary Data", verbatimTextOutput("summaryText")),
        tabPanel("Box Plot", 
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       h5("select variables for the boxplot:"),
                       selectInput("var_box", "Variabel:", ""),
                       
                     ),
                     plotOutput("boxPlot")))),
        tabPanel("Bar Plot", 
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       h5("select variables for the barplot:"),
                       selectInput("var_bar", "Variabel:", "")
                       
                     ),
                     plotOutput("barPlot")))),
        tabPanel("Data and ANOVA Test Results", 
                 fluidPage(
                   titlePanel("Analysis of Variance"),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("vardipen", label = h3("Dependen"),
                                   list("y" = "y"), selected = 1),
                       
                       selectInput("varindepen", label = h3("Independen"),
                                   list("group" = "group"), selected = 1)
                       
                     ),
                     
                     mainPanel(
                       titlePanel("Data"),
                       DT::dataTableOutput('tbl1'),
                       titlePanel("Result"),
                       verbatimTextOutput("anovaText")
                     )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.table(input$file$datapath, sep = "\t", header=T)
  })
  
  # Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(data(), options = list(lengthChange = FALSE))
  })
  
  output$summaryText <- renderPrint({
    if (input$analyzeBtn > 0) {
      summary(data())
    }
  })
  
  output$anovaText <- renderPrint({
    if (input$analyzeBtn > 0) {
      anova_result <- aov(y ~ group, data = data1)
      summary(anova_result)
    }
  })
  output$tbl1 = DT::renderDataTable({
    if (input$analyzeBtn > 0) {
      DT::datatable(data1, options = list(lengthChange = FALSE))
    }
  })
  
  output$boxPlot <- renderPlot({
    if (input$analyzeBtn > 0) {
      req(input$var_box)
      boxplot(t(data()[, input$var_box]), 
              main = paste("Boxplot of", input$var_box,"per Hari"),
              xlab = "Day", ylab = input$var_box)
    }
  })
  # Update pilihan variabel pada sidebar
  observe({
    req(data())
    updateSelectInput(session, "var_box", choices = colnames(data()))
  })
  
  output$barPlot <- renderPlot({
    if (input$analyzeBtn > 0) {
      req(input$var_bar)
      
      # Gambar bar plot
      barplot(t(data()[, input$var_bar]),
              main = paste("Bar Plot of", input$var_bar, "per Hari"),
              xlab = "Day", ylab = input$var_bar)
    }
  })
  # Update pilihan variabel pada sidebar
  observe({
    updateSelectInput(session, "var_bar", choices = c("Left.Sidebar", "Center.Page", "Right.Sidebar"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
