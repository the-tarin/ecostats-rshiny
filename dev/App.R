library(shiny)
library(leaflet)
library(htmltools)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  leafletOutput("mymap"),
  
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
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "distPlot"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  ### input dataframes
  mic_df <- reactive({
    req(input$file1)
    mic_df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = "")
    return(mic_df)
  })
  
  recording_df <- reactive({
    req(input$file2)
    recording_df <- read.csv(input$file2$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = "")
    
    # recorder by measured datetime of gibbon call
    recording_df <- recording_df[order(recording_df$X.measured_call_datetime), ]
    
    return(recording_df)
  })
  
  output$contents <- renderTable({
    recording_df()
  })

  ### output distribution plots for testing
  output$distPlot <- renderPlot({
    x <- recording_df()$X.measured_bearing
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    bins <- seq(min(x), max(x), length.out = 100)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  ### map output
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # addMarkers(data = mic_df(), lat = ~X.lat., lng = ~X.lon.)
      addAwesomeMarkers(data = mic_df(), lat = ~X.lat., lng = ~X.lon.,
        icon = awesomeIcon(
          icon = 'marker',
          markerColor = 'blue',
          iconColor = 'white',
          angle = 45  # Set the rotation angle based on the bearing
        )
      )
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)