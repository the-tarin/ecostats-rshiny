library(shiny)

# maps
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(sp)

# dates and data manipulation
library(dplyr)
library(lubridate)

# datatables
library(DT)

# calculate coordinates from an arc
calculate_arrow_head_coordinates <- function(lat, lng, bearing, radius) {
  # Radius of the Earth in meters
  earth_radius <- 6371000  # approximate value for Earth
  
  # Convert bearing from radians to degrees
  bearing_deg <- bearing * 180 / pi
  
  # Calculate new latitude and longitude
  lat_rad <- lat * pi / 180
  lng_rad <- lng * pi / 180
  
  new_lat_rad <- asin(sin(lat_rad) * cos(radius / earth_radius) +
                        cos(lat_rad) * sin(radius / earth_radius) * cos(bearing))
  
  new_lng_rad <- lng_rad + atan2(sin(bearing) * sin(radius / earth_radius) * cos(lat_rad),
                                 cos(radius / earth_radius) - sin(lat_rad) * sin(new_lat_rad))
  
  # Convert new latitude and longitude to degrees
  new_lat <- new_lat_rad * 180 / pi
  new_lng <- new_lng_rad * 180 / pi
  
  return(c(new_lng, new_lat))
}

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
    ),
    
    ### Main panel for displaying outputs
    mainPanel(
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
    arrow1 <- matrix(c(106.6407, 106.6480, 14.25521, 14.26078), 2)

    coordinates_matrix <- matrix(c(
      -78.5, 17.6,
      -78.8, 17.6,
      -79.0, 17.5,
      -79.2, 17.5,
      -79.4, 17.5,
      -79.6, 17.4,
      -79.6, 16.9,
      -79.7, 16.3,
      -79.8, 16.0,
      -79.9, 15.8,
      -79.9, 15.7,
      -80.3, 16.2,
      -81.1, 16.6,
      -81.8, 16.6,
      -82.2, 17.0,
      -82.8, 17.3,
      -83.4, 17.4,
      -84.0, 17.9,
      -84.7, 18.1,
      -85.2, 18.3,
      -85.5, 18.6,
      -85.8, 19.1,
      -86.1, 19.5,
      -86.4, 20.1,
      -86.7, 20.3,
      -86.8, 20.6,
      -86.8, 20.6,
      -86.9, 20.8,
      -87.0, 20.8,
      -87.1, 21.0,
      -87.1, 21.3,
      -87.0, 21.6,
      -86.8, 21.8,
      -86.1, 22.4,
      -85.4, 23.1,
      -84.3, 24.0,
      -83.1, 25.0,
      -81.7, 25.9,
      -81.0, 26.2,
      -78.8, 28.0,
      -76.0, 30.1,
      -72.0, 33.3,
      -67.9, 36.8,
      -63.5, 40.5,
      -60.0, 42.5,
      -57.5, 44.0,
      -55.0, 45.0,
      -52.0, 45.5
    ), ncol = 2, byrow = TRUE)
    spatial_lines <- SpatialLines(list(Lines(list(Line(coords = coordinates_matrix)), ID = "1")))
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      removeArrowhead(layerId = NULL) %>%
      addCircleMarkers(
        data = mic_df(),
        lat = ~X.lat.,
        lng = ~X.lng.,
        radius = 6,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5
      ) %>%
      addArrowhead(data = spatial_lines, color = "red") %>%
      fitBounds(lng1 = min(mic_df()$X.lng.), lat1 = min(mic_df()$X.lat.),
                lng2 = max(mic_df()$X.lng.), lat2 = max(mic_df()$X.lat.))
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
  
  arrows <- reactiveValues(coordinates = array())
  
  observeEvent(input$recording_table_rows_selected, {
    selected_rows = input$recording_table_rows_selected
    selected_mics = recording_df()$X.mic_ID.[selected_rows]
    print(selected_mics)
    
    radius = 1000 # meters. todo: calculate the optimum radius given mic coordinates

    # arrow_coordinates_cum <- matrix(nrow = 0, ncol = 2)
    arrow_coordinates_total <- array(NA, dim = c(2, 2, length(selected_mics)))
    
    for (i in 1:length(selected_mics)) {
      selected_mic_lat <- mic_df()$X.lat.[selected_mics[i]]
      selected_mic_lng <- mic_df()$X.lng.[selected_mics[i]]
      
      selected_mic_coordinates <- c(selected_mic_lng, selected_mic_lat)
      
      selected_bearings <- recording_df()$X.measured_bearing.[selected_rows[i]]
      
      # calculate new arrowhead coordinates from mic coordinates and measured detection bearing
      arrow_head_coordinates <- calculate_arrow_head_coordinates(selected_mic_lat, selected_mic_lng, selected_bearings, radius)
      arrow_coordinates <- rbind(selected_mic_coordinates, arrow_head_coordinates)
      rownames(arrow_coordinates) = NULL
      colnames(arrow_coordinates) = c("lng", "lat")
      
      # arrow_coordinates_cum <- rbind(arrow_coordinates_cum, arrow_coordinates)
      arrow_coordinates_total[, , i] <- arrow_coordinates
    }
    arrows$coordinates = arrow_coordinates_total
  })
  
  # update map with bearing directions for selected recordings
  observeEvent(arrows$coordinates, {
    print("Updated coordinates...")
    print(arrows$coordinates)
    
    # prevents plotting arrows on declaration
    if (!any(is.na(arrows$coordinates))) {
      leafletProxy("map") %>%
        addArrowhead(data = arrows$coordinates, color = "red")
    }
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