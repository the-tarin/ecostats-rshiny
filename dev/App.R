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
      DT::dataTableOutput("recording_table"),
      actionButton("test", " Test")
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
        lng = ~X.lng.,
        radius = 6,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5
      ) %>%
      fitBounds(lng1 = min(mic_df()$X.lng.), lat1 = min(mic_df()$X.lat.),
                lng2 = max(mic_df()$X.lng.), lat2 = max(mic_df()$X.lat.))
  })
  ###
  
  ### update
  # recording_df <- reactiveVal(data.frame())
  # observeEvent(input$fileRecordings, {
  #   recording_df(input$fileRecordings)
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
    datatable(recording_df_filtered, editable = list(target = 'cell', disable = list(columns = c(1, 2, 3, 4, 5, 6, 7, 8))), rownames = FALSE)
  })
  ###
  
  proxy_recording_table = dataTableProxy('recording_table')
  
  observeEvent(input$recording_table_cell_edit, {
    browser()
    info = input$recording_table_cell_edit
    str(info)
    recording_df <<- editData(recording_df, info)
    # replaceData(proxy_recording_table, recording_df, resetPaging = FALSE)  # important
    # # the above steps can be merged into a single editData() call; see examples below
  })
  
  ### testing purposes
  observeEvent(input$test, {
    print(input$recording_table_rows_selected)
  })
  ###
  
  ### adding arrows to map
  arrows <- reactiveValues(coordinates = array())
  
  observeEvent(input$recording_table_rows_selected, ignoreNULL = FALSE, {
    selected_rows = input$recording_table_rows_selected
    if (is.null(selected_rows)) {
      arrows$coordinates = array()
      return()
    }
    selected_mics = recording_df()$X.mic_ID.[selected_rows]
    
    radius = 1000 # meters. todo: calculate the optimum radius given mic coordinates

    arrow_coordinates_total <- array(NA, dim = c(2, 2, length(selected_mics)))
    # arrow_coordinates_total <- list()
    
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
        leafletProxy("map") %>% addArrowhead(data = arrows$coordinates[,,i], group = "all", layerId = paste0("arrow_", i), color = "red")
      }
    }
  })
  ###

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
    # if (!is.null(info) && !is.null(info$value)) {cd ..
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
    
    animal_ID <- rep(-1, nrow(recording_df))
    print(animal_ID)
    recording_df <- cbind(animal_ID, recording_df)
    
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