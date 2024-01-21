library(shiny)

library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

library(dplyr)
library(lubridate)

library(DT)

ui <- fluidPage(
  titlePanel("Survey of Orangutan Noise Data"),
  ### map
  leafletOutput("map"),
  ###
  sidebarLayout(
    sidebarPanel(
      ### file uploads
      fileInput("fileMic", "Upload Mic Coordinate Data in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      fileInput("fileRecordings", "Upload Recording Data in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      fileInput("fileGibbons", "Upload Gibbon Group Data (Ground Truth) in CSV Format",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      ###
      ### filters
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
    
    ### Main panel for displaying outputs
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
  observeEvent(input$fileRecordings, {
    updateDateInput(
      session,
      "selected_date",
      value = as.POSIXct("2023-01-01", tz = "GMT")
      # value = as.POSIXct(format(recording_df$X.measured_call_datetime[1], "%Y-%m-%d"), tz <- "GMT")
    )
  })
  
  ### add mic markers when mic_df is uploaded
  observeEvent(input$fileMic, {
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
      addArrowhead(data = matrix(c(-62.1, 13.2, -62.8, 13.6), ncol = 2)) %>%
      fitBounds(lng1 = min(mic_df()$X.lon.), lat1 = min(mic_df()$X.lat.),
                lng2 = max(mic_df()$X.lon.), lat2 = max(mic_df()$X.lat.))
  })
  ###
  
  ### update
  # recording_df <- reactiveVal(data.frame())
  # observeEvent(input$fileRecordings, {
  #   recording_df(recording_df())
  # })
  
  ### main recordings datatable
  output$recording_table <- DT::renderDataTable({
    recording_df_filtered = recording_df() %>%
      filter(
        X.measured_call_datetime. >= as.POSIXct(input$selected_time_range[1], format = "%Y-%m-%d %H:%M:%S"),
        X.measured_call_datetime. <= as.POSIXct(input$selected_time_range[2], format = "%Y-%m-%d %H:%M:%S")
      )
    
    # # Apply the edits to the filtered data
    # if (!is.null(edited_data$df)) {
    #   print("hello1")
    #   recording_df_filtered <- merge(
    #     recording_df_filtered,
    #     edited_data$df,
    #     by = "recording_table_row_id", # Add a unique identifier for each row
    #     all.x = TRUE
    #   )
    #   print("hello2")
    # }
      
    # datatable(recording_df, editable = list(target = 'row', disable = list(columns = c(0, 2, 3, 4, 5, 6, 7))), rownames = FALSE)
    datatable(recording_df_filtered, editable = list(target = 'cell', disable = list(columns = c(0, 2, 3, 4, 5, 6, 7))), rownames = FALSE)
  })
  ###
  
  ### selected mic from datatable
  # todo: need to select mic ID from filtered datatable
  
  edited_data <- reactiveValues(df = NULL)
  
  proxy_recording_table = dataTableProxy('recording_table')
  
  observeEvent(input$recording_table_cell_edit, {
    print("hi")
    print(input$recording_table_cell_edit)
    # browser()
    # edited_data$df <- editData(edited_data$df, input$recording_table_cell_edit, 'recording_table')
    # info = input$recording_table_cell_edit
    # str(info)  # check what info looks like (a data frame of 3 columns)
    # recording_df <<- editData(recording_df, info)
    # replaceData(proxy_recording_table, recording_df, resetPaging = FALSE)  # important
    # # the above steps can be merged into a single editData() call; see examples below
  })
  
  
  observeEvent(input$recording_table_rows_selected, {
    selected_rows = input$recording_table_rows_selected
    selected_mics = recording_df()$X.mic_ID.[selected_rows]
    print(selected_mics)
    selected_mic_lat = mic_df()$X.lat.[selected_mics]
    selected_mic_lon = mic_df()$X.lon.[selected_mics]

    selected_bearings = recording_df()$X.measured_bearing.[selected_rows]
  })
  
  # test #
  # create a reactiveValues to store the edited data
  # edited_data <- reactiveValues(df = NULL)

  # hi <- reactive({
  #   input$recording_table_cell_edit
  # })

  # observer to update edited_data when the table is edited
  observeEvent(input$recording_table_cell_edit, {
    # info <- input$recording_table_cell_edit
    # str(info)

    print("hi")

    # # Check if the data table is not empty and if a cell was edited
    # if (!is.null(info) && !is.null(info$value)) {
    #   edited_data$df <- editData(edited_data$df, info)
    # }
  })
  ###

  ### file uploads
  mic_df <- reactive({
    req(input$fileMic)
    mic_df <- read.csv(input$fileMic$datapath,
                       header = TRUE,
                       sep = ",",
                       quote = "")
    return(mic_df)
  })
  
  recording_df <- reactive({
    req(input$fileRecordings)
    recording_df <- read.csv(input$fileRecordings$datapath,
                             header = TRUE,
                             sep = ",",
                             quote = "")
    
    # reorder by measured datetime of gibbon call
    recording_df <- recording_df[order(recording_df$X.measured_call_datetime), ]
    
    return(recording_df)
  })
  
  gibbon_df <- reactive({
    req(input$fileGibbons)
    gibbon_df <- read.csv(input$fileGibbons$datapath,
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