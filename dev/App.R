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
      # plotOutput(outputId = "distPlot"),
      # leafletOutput(outputId = "leafletMap")
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # todo: implement server feature which can deal with .csv survey data format
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  # output$table <- renderTable({
  #   df <- data()
  #   return(df)
  # })
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # output$distPlot <- renderPlot({
  #   x <- data()$bearing
  #   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   bins <- seq(min(x), max(x), length.out = 10)
  #   
  #   hist(x, breaks = bins, col = "#75AADB", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  # })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)