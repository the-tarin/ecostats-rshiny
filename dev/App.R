library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(DT)

min_time_range = as.POSIXct("2023-01-01 04:00:00")
max_time_range = as.POSIXct("2023-01-01 05:00:00")

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  # map #
  leafletOutput("mymap"),
  ###
  sidebarLayout(
    sidebarPanel(
      # input files #
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
      
      fileInput("file3", "Upload Gibbon Group Data (Ground Truth) in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      ###
      # filters #
      # dateInput(inputId = "date", label = "Date Selected"), # this needs more work
      sliderInput(
        inputId = "selected_time_range", 
        label = "Time Range",
        min = min_time_range,
        max = max_time_range,
        value = c(min_time_range, max_time_range),
        step = 1,
        round = FALSE,
        ticks = TRUE,
        animate = FALSE,
        width = NULL,
        sep = ",",
        pre = NULL,
        post = NULL,
        timeFormat = "%F %T",
        timezone = NULL,
        dragRange = TRUE
      )
      # selectInput(
      #   inputId = "selected_mic_ID",
      #   label = "Filter Mic ID",
      #   choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      #   selected = NULL,
      #   multiple = FALSE,
      #   selectize = TRUE,
      #   width = NULL,
      #   size = NULL
      # )
      ###
    ),
    
    # Main panel for displaying outputs #
    mainPanel(
      plotOutput(outputId = "distPlot"),
      tableOutput("contents"),
      DT::dataTableOutput("mytable")
    )
    ###
  )
)

server <- function(input, output) {
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
  
  gibbon_df <- reactive({
    req(input$file3)
    gibbon_df <- read.csv(input$file3$datapath,
                       header = TRUE,
                       sep = ",",
                       quote = "")
    return(gibbon_df)
  })
  
  ### output distribution plots for testing
  # output$distPlot <- renderPlot({
  #   x <- recording_df()$X.measured_bearing
  #   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   bins <- seq(min(x), max(x), length.out = 100)
  # 
  #   hist(x, breaks = bins, col = "#75AADB", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  # })
  
  output$mytable = DT::renderDataTable({
    recording_df() %>%
      # filter(!!sym("X.mic_ID.") == input$selected_mic_ID) %>% # todo: figure out better interface for selecting by mic ID. Also having issues with default selection.
      filter(X.measured_call_datetime. >= as.POSIXct(input$selected_time_range[1]) &
               X.measured_call_datetime. <= as.POSIXct(input$selected_time_range[2]))
  })
  
  ### map output
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = mic_df(), lat = ~X.lat., lng = ~X.lon.)
      # addMarkers(data = gibbon_df(), lat = ~X.lat., lng = ~X.lon.)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)