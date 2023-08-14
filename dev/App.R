library(shiny)
library(leaflet)

# Define UI for app that draws a histogram ----
# fluidpage automatically adjust dimensions of page
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  
  leafletOutput(outputId = "leafletMap"),
  
  # Data Upload, only accept RData for now
  fileInput("file", "Choose RData File",
            accept = c(".RData")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 10,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
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