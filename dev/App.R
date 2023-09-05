library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose RData File",
                accept = c(".RData")),
      
      dateRangeInput("dates", h3("Date range"))
      
      #sliderInput(inputId = "bins",
      #            label = "Number of bins:",
      #            min = 10,
      #            max = 50,
      #            value = 30)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # plotOutput(outputId = "distPlot")
      leafletOutput(outputId = "leafletMap")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  output$distPlot <- renderPlot({
    
    x    <- data$bearing
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  # todo: implement server feature which can deal with .csv survey data format
  data <- reactive({
    infile <- input$file1
  
    if (is.null(infile)) {
      return(NULL)
    }
    
    load(infile$datapath)
    
    # get the last object that was loaded (excluding the "infile" object)
    get(ls()[ls() != "infile"])
  })
  
  output$table <- renderTable({
    data()
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)