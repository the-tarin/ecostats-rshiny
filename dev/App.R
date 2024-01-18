library(shiny)

library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

library(dplyr)
library(lubridate)

library(DT)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  # map #
  leafletOutput("map"),
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
      dateInput(inputId = "selected_date", label = "Select Date"),
      
      sliderInput(
        "selected_time_range",
        "Select Time Range",
        min = as.POSIXct("1970-01-01 00:00:00"),
        max = as.POSIXct("1970-01-01 23:59:59"),
        value = c(as.POSIXct("1970-01-01 08:00:00"), as.POSIXct("1970-01-01 17:00:00")),
        step = 60,
        timeFormat = "%H:%M"
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
      DT::dataTableOutput("recording_table")
    )
    ###
  )
)

server <- function(input, output, session) {
  ### change silderInput date based on select date output
  observeEvent(input$selected_date, {
    updateSliderInput(
      session,
      "selected_time_range",
      min = as.POSIXct(paste(input$selected_date, "00:00:00", tz = "GMT")),
      max = as.POSIXct(paste(input$selected_date, "23:59:59", tz = "GMT")),
      value = c(as.POSIXct(paste(input$selected_date, "08:00:00", tz = "GMT")), 
                as.POSIXct(paste(input$selected_date, "17:00:00", tz = "GMT")))
    )
  })
  ###
  
  ### update select date input to earliest date in the recording data
  # todo: complete this to automatically extract the date. Not high priority
  observeEvent(input$file2, {
    updateDateInput(
      session,
      "selected_date",
      value = as.POSIXct("2023-01-01", tz = "GMT")
      # value = as.POSIXct(format(recording_df$X.measured_call_datetime[1], "%Y-%m-%d"), tz <- "GMT")
    )
  })
  
  # add mic markers when mic_df is uploaded
  observeEvent(input$file1, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = mic_df(),
        lat = ~X.lat.,
        lng = ~X.lon.,
        radius = 6,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5
      ) %>%
      arrowMarkers(
        arrows = list(
          list(
            lng1 = -71.057, lat1 = 42.36,
            lng2 = -71.056, lat2 = 42.361,
            weight = 2, color = "red", size = 10
          )
        )
      ) %>%
      fitBounds(lng1 = min(mic_df()$X.lon.), lat1 = min(mic_df()$X.lat.),
                lng2 = max(mic_df()$X.lon.), lat2 = max(mic_df()$X.lat.))
  })
  
  output$recording_table <- DT::renderDataTable({
    # not the most ideal solution, but it'll do for now. todo: fix timezone issue
    selected_time_min <- input$selected_time_range[1] + hours(13)
    selected_time_max <- input$selected_time_range[2] + hours(13)
    # print(selected_time_min)
    # print(selected_time_max)
    
    recording_df = recording_df() %>%
      filter(
        X.measured_call_datetime. >= as.POSIXct(input$selected_time_range[1], format = "%Y-%m-%d %H:%M:%S"),
        X.measured_call_datetime. <= as.POSIXct(input$selected_time_range[2], format = "%Y-%m-%d %H:%M:%S")
      )
      
    datatable(recording_df, editable = list(target = 'row', disable = list(columns = c(0, 2, 3, 4, 5, 6, 7))), rownames = FALSE)
  })
  
  ### selected mic from datatable
  # todo: need to select mic ID from filtered datatable
  observeEvent(input$recording_table_rows_selected, {
    selected_rows = input$recording_table_rows_selected
    selected_mics = recording_df()$X.mic_ID.[selected_rows]
    print(selected_mics)
    
    selected_bearings = recording_df()$X.measured_bearing.[selected_rows]
    print(selected_bearings)
    
    
  })
  
  # # test #
  # # create a reactiveValues to store the edited data
  # # edited_data <- reactiveValues(df = NULL)
  # 
  # # hi <- reactive({
  # #   input$recording_table_cell_edit
  # # })
  # 
  # # observer to update edited_data when the table is edited
  # observeEvent(input$recording_table_cell_edit, {
  #   # info <- input$recording_table_cell_edit
  #   # str(info)
  #   
  #   print("hi")
  #   
  #   # # Check if the data table is not empty and if a cell was edited
  #   # if (!is.null(info) && !is.null(info$value)) {
  #   #   edited_data$df <- editData(edited_data$df, info)
  #   # }
  # })
  # ###
  
  ### file uploads
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
    
    # print(recording_df$X.measured_call_datetime[1])
    
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
  ###
  
  ### map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)
      )
      # todo: fit to page
  })
  ###
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)