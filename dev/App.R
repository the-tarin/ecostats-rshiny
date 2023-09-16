library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
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
  data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = "")
    
    
    return(df)
    
  })
  
  output$contents <- renderTable({
    data()
  })

  output$distPlot <- renderPlot({
    x <- data()$X.measured_bearing
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    bins <- seq(min(x), max(x), length.out = 10)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)