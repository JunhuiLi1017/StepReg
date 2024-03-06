library(shiny)
library(GGally)

library(shiny)
library(GGally)

# Define the createTabPanel function before using it
createTabPanel <- function(summaryOutput, plotOutput) {
  tabPanel(
    "Data",
    tabsetPanel(
      tabPanel("Summary", summaryOutput),
      tabPanel("Plot", plotOutput)
    )
  )
}

ui <- fluidPage(
  navbarPage(
    title = "My Shiny App",
    tabPanel("Tab 1"),
    tabPanel("Tab 2"),
    navbarMenu("File",
               tabPanel("Select Dataset",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("dataset", "Select an R dataset", 
                                        choices = c(data(package = "ggplot2")$results[, "Item"], "Uploaded Dataset"))
                          ),
                          mainPanel(
                            createTabPanel(verbatimTextOutput("summary"), plotOutput("plot"))
                          )
                        )
               ),
               tabPanel("Upload Your Dataset",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("file1", "Choose CSV File",
                                      multiple = FALSE,
                                      accept = c(".csv"))
                          ),
                          mainPanel(
                            createTabPanel(verbatimTextOutput("summary"), plotOutput("plot"))
                          )
                        )
               )
    ),
    navbarMenu("Analysis")
  )
)

server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$file1)  # Ensure file is uploaded
    tryCatch(
      {
        read.table(input$file1$datapath, sep = "\t")
      },
      error = function(e) {
        return(NULL)
      }
    )
  })
  
  data1 <- reactive({
    if (input$dataset == "Uploaded Dataset") {
      uploaded_data()
    } else {
      get(input$dataset, envir = asNamespace("ggplot2"))
    }
  })
  
  output$summary <- renderPrint({
    req(data1())  # Ensure data is available
    summary(data1())
  })
  
  output$plot <- renderPlot({
    req(data1())  # Ensure data is available
    ggpairs(data1())
  })
}

shinyApp(ui, server)