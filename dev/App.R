library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Mic Coordinate Data in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      fileInput("file2", "Upload Recording Data in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      dateRangeInput("dates", h3("Date range"))
      
      #sliderInput(inputId = "bins",
      #            label = "Number of bins:",
      #            min = 10,
      #            max = 50,
      #            value = 30)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "distPlot"),
      # leafletOutput(outputId = "leafletMap")
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # input$fileX will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  data1 <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = "")
    return(df)
  })
  
  data2 <- reactive({
    req(input$file2)
    df <- read.csv(input$file2$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = "")
    return(df)
  })
  
  output$contents <- renderTable({
    data1()
  })

  output$distPlot <- renderPlot({
    x <- data1()$X.measured_bearing
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    bins <- seq(min(x), max(x), length.out = 100)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)