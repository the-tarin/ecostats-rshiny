library(shiny)
library(shinyTime)

# maps
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

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
  titlePanel("Survey of Acoustic Spatial-Recapture Data"),
  ### map
  leafletOutput("map", width = "100%", height = 800),
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
      # dateInput(inputId = "selected_date", label = "Select Date"),
      
      sliderInput(
        "selected_time_range",
        "Time Range from Data",
        min = as.POSIXct("2023-01-01 00:00:00"),
        max = as.POSIXct("2023-01-01 23:59:59"),
        value = c(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 17:00:00")),
        step = 60
      ),
      
      actionButton("set_scope_time_range", "Set Scope Range"),
      
      sliderInput(
        "selected_scope_time_range",
        "Scope Time Range",
        min = as.POSIXct("2023-01-01 00:00:00"),
        max = as.POSIXct("2023-01-01 23:59:59"),
        value = c(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 17:00:00")),
        step = 60
      ),
      
      # timeInput("selected_start_time", label = "Select Start Time:"),
      # timeInput("selected_end_time", label = "Select End Time:"),
      
      # sliderInput(
      #   "selected_time_range_minute",
      #   "Select Time Range",
      #   min = as.POSIXct("1970-01-01 00:00:00"),
      #   max = as.POSIXct("1970-01-01 23:59:59"),
      #   value = c(as.POSIXct("1970-01-01 08:00:00"), as.POSIXct("1970-01-01 17:00:00")),
      #   step = 60,
      #   timeFormat = "%H:%M"
      # ),
    ),
    
    ### Main panel for displaying outputs
    mainPanel(
      DT::dataTableOutput("recording_table"),
      actionButton("test", "Test"),
      actionButton("set_new_animal_ID", "Set New Animal ID"),
      downloadButton("download_button", "Download Recordings"),
    )
    ###
  )
)

server <- function(input, output, session) {
  ### change silderInput date based on select date output
  # observeEvent(input$selected_date, {
  #   updateSliderInput(
  #     session,
  #     "selected_time_range",
  #     min = as.POSIXct(paste(input$selected_date, "00:00:00", tz = "GMT")),
  #     max = as.POSIXct(paste(input$selected_date, "23:59:59", tz = "GMT")),
  #     value = c(as.POSIXct(paste(input$selected_date, "03:00:00", tz = "GMT")),
  #               as.POSIXct(paste(input$selected_date, "17:00:00", tz = "GMT")))
  #   )
  # })
  
  observeEvent(recording_data$recording_first_call_datetime, {
    date_start <- substr(recording_data$recording_first_call_datetime, start = 0, stop = 10)
    time_start <- substr(recording_data$recording_first_call_datetime, start = 12, stop = 20)
    date_end <- substr(recording_data$recording_last_call_datetime, start = 0, stop = 10)
    time_end <- substr(recording_data$recording_last_call_datetime, start = 12, stop = 20)
    # todo: add date_start/end without any issues. Currently doesn't work.
    updateSliderInput(
      session,
      "selected_time_range",
      min = as.POSIXct(paste("2023-01-01", time_start, tz = "GMT")),
      max = as.POSIXct(paste("2023-01-01", time_end, tz = "GMT")),
      value = c(as.POSIXct(paste("2023-01-01", time_start, tz = "GMT")),
                as.POSIXct(paste("2023-01-01", time_end, tz = "GMT")))
    )
  })
  ###
  
  ### update select date input to earliest date in the recording data
  # todo: complete this to automatically extract the date. Not high priority
  # observeEvent(input$fileRecordings, {
  #   updateDateInput(
  #     session,
  #     "selected_date",
  #     value = as.POSIXct("2023-01-01", tz = "GMT")
  #     # value = as.POSIXct(format(recording_df$X.measured_call_datetime[1], "%Y-%m-%d"), tz <- "GMT")
  #   )
  # })
  
  ### add mic markers when mic_df is uploaded
  observeEvent(input$fileMic, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = mic_df(),
        lat = ~X.lat.,
        lng = ~X.lng.,
        radius = 6,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5
      ) %>%
      fitBounds(lng1 = min(mic_df()$X.lng.), lat1 = min(mic_df()$X.lat.),
                lng2 = max(mic_df()$X.lng.), lat2 = max(mic_df()$X.lat.))
  })
  ###
  
  ### main recordings datatable
  proxy <- DT::dataTableProxy('recording_table')
  output$recording_table <- DT::renderDataTable({
    # todo: create proxy to stop having to reinitialise datatable 
    req(input$fileRecordings)
    recording_df_filtered = recording_data$recording_master_df %>%
      filter(
        X.measured_call_datetime. >= as.POSIXct(input$selected_time_range[1], format = "%Y-%m-%d %H:%M:%S"),
        X.measured_call_datetime. <= as.POSIXct(input$selected_time_range[2], format = "%Y-%m-%d %H:%M:%S")
      )
    recording_data$recording_temp_df = recording_df_filtered
    
    datatable(recording_df_filtered, editable = list(target = 'cell', disable = list(columns = c(1, 2, 3, 4, 5, 6, 7, 8))), rownames = FALSE,  extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'))) %>%
      formatRound(columns=c('X.ground_truth_bearing.', 'X.measured_bearing.'), digits=3)
  })
  ###
  
  ### save changes from datatable edits
  observeEvent(input$recording_table_cell_edit, {
    edit = input$recording_table_cell_edit
    # find the recording ID which has been edited from datatable (temp dataframe) and save changes to master dataframe
    edited_recording_ID <- recording_data$recording_temp_df[edit$row, edit$col+2]
    recording_data$recording_master_df[recording_data$recording_master_df[,2] == edited_recording_ID, 1] <- edit$value
  })
  
  observeEvent(input$set_new_animal_ID, {
    selected_rows = input$recording_table_rows_selected
    selected_recording_ID <- recording_data$recording_temp_df[selected_rows, 2]
    selected_recording_ID <- as.integer(selected_recording_ID)
    
    print(selected_recording_ID)
    
    # todo: issues with not being able to set animal ID. Probably due to some timing issue.
    # generate and set new animal ID
    max_animal_ID <- max(recording_data$recording_master_df[,1])
    print(recording_data$recording_master_df[, 2])
    selected_rows <- which(recording_data$recording_master_df[, 2] == selected_recording_ID)
    print(selected_rows)
    recording_data$recording_master_df[selected_rows, 1] <- max_animal_ID + 1
  })
  
  ### testing purposes
  observeEvent(input$test, {
    print(input$recording_table_rows_selected)
  })
  ###
  
  ### download recordings
  # todo: ...
  ###
  
  ### adding arrows to map
  arrows <- reactiveValues(coordinates = array())
  
  observeEvent(input$recording_table_rows_selected, ignoreNULL = FALSE, {
    selected_rows = input$recording_table_rows_selected
    if (is.null(selected_rows)) {
      arrows$coordinates = array()
      return()
    }
    selected_mics = recording_data$recording_master_df$X.mic_ID.[selected_rows]
    
    radius = 1000 # meters. todo: calculate the optimum radius given mic coordinates

    arrow_coordinates_total <- array(NA, dim = c(2, 2, length(selected_mics)))
    # arrow_coordinates_total <- list()
    
    for (i in 1:length(selected_mics)) {
      selected_mic_lat <- mic_df()$X.lat.[selected_mics[i]]
      selected_mic_lng <- mic_df()$X.lng.[selected_mics[i]]
      
      selected_mic_coordinates <- c(selected_mic_lng, selected_mic_lat)
      
      selected_bearings <- recording_data$recording_master_df$X.measured_bearing.[selected_rows[i]]
      
      # calculate new arrowhead coordinates from mic coordinates and measured detection bearing
      arrow_head_coordinates <- calculate_arrow_head_coordinates(selected_mic_lat, selected_mic_lng, selected_bearings, radius)
      arrow_coordinates <- rbind(selected_mic_coordinates, arrow_head_coordinates)
      rownames(arrow_coordinates) = NULL
      colnames(arrow_coordinates) = c("lng", "lat")
      
      arrow_coordinates_total[, , i] <- arrow_coordinates
      # arrow_coordinates_total[[i]] <- Line(arrow_coordinates)
    }
    # arrow_coordinates_total_lines <- SpatialLines(arrow_coordinates_total)
    arrows$coordinates = arrow_coordinates_total
  })
  
  # update map with bearing directions for selected recordings
  # implementation with array of matrices. todo: try using sp package objects
  observeEvent(arrows$coordinates, {
    leafletProxy("map") %>% clearGroup("all")
    # prevents plotting arrows on declaration
    if (!any(is.na(arrows$coordinates))) {
      for (i in 1:dim(arrows$coordinates)[3]) {
        leafletProxy("map") %>% addArrowhead(data = arrows$coordinates[,,i], group = "all", layerId = paste0("arrow_", i), color = "red", opacity = 50, 
                                             options = arrowheadOptions(yawn = 40, fill = FALSE))
      }
    }
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
  
  recording_data <- reactiveValues(
    recording_temp_df = NULL,
    recording_master_df = NULL,
    recording_first_call_datetime = NULL,
    recording_last_call_datetime = NULL
  )
  
  observeEvent(input$fileRecordings, {
    req(input$fileRecordings)
    
    recording_master_df <- read.csv(input$fileRecordings$datapath,
                             header = TRUE,
                             sep = ",",
                             quote = "")
    
    # reorder by measured datetime of gibbon call
    recording_master_df <- recording_master_df[order(recording_master_df$X.measured_call_datetime), ]
    
    recording_data$recording_first_call_datetime <- recording_master_df$X.measured_call_datetime[1]
    recording_data$recording_last_call_datetime <- recording_master_df$X.measured_call_datetime[length(recording_master_df$X.measured_call_datetime)]
    
    print(recording_data$recording_first_call_datetime)
    print(recording_data$recording_last_call_datetime)
    
    animal_ID <- rep(0, nrow(recording_master_df))
    recording_master_df <- cbind(animal_ID, recording_master_df)
    
    recording_master_df$X.recording_ID <- as.integer(recording_master_df$X.recording_ID)
    
    recording_data$recording_temp_df <- recording_master_df
    recording_data$recording_master_df <- recording_master_df
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
      ) %>%
      setView(lng = 0, lat = 0, zoom = 3)
  })
  ###
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)