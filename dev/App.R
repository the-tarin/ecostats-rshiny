library(shiny)

# time input
library(shinyTime)

# maps
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

# other
library(geosphere)
library(lubridate)
library(DT)

ui <- fluidPage(
  titlePanel("Survey of Acoustic Spatial-Recapture Data"),
  # Split the app into two halves vertically
  fluidRow(
    # Left half
    column(width = 6,
           # Add UI elements for the left half
           ### map
           leaflet::leafletOutput("map", width = "100%", height = 800),
           actionButton("home_map_view", "Home Map View")
    ),
    
    # Right half
    column(width = 6,
           # Add UI elements for the right half
           tabsetPanel(id = "tabs",
                       tabPanel("Mics",
                                DT::dataTableOutput("mic_table"),
                       ),
                       tabPanel("Recordings",
                                DT::dataTableOutput("recording_table"),
                                actionButton("set_new_call_ID", "Set New Call ID")
                       ),
                       tabPanel("Calls",
                                DT::dataTableOutput("call_table"),
                                actionButton("remove_call", "Remove Call"),
                                actionButton("set_new_animal_ID", "Group Call to Animal")
                       ),
                       tabPanel("Animals",
                                DT::dataTableOutput("animal_table")
                       ),
           ),
           downloadButton("download_button", "Download Recordings"),
    )
  ),
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
      
      sliderInput(
        "selected_time_range",
        "Time Range of Data",
        min = as.POSIXct("2023-01-01 00:00:00"),
        max = as.POSIXct("2023-01-01 23:59:59"),
        value = c(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 17:00:00")),
        step = 1
      ),
      
      sliderInput(
        "selected_scope_time_range",
        "Scope Time Range",
        min = as.POSIXct("2023-01-01 00:00:00"),
        max = as.POSIXct("2023-01-01 23:59:59"),
        value = c(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 17:00:00")),
        step = 1
      )
    ),
    
    ### Main panel for displaying outputs
    mainPanel(
    )
    ###
  )
)

server <- function(input, output, session) {
  ### sliders for filtering recordings by datetime
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
    req(input$fileRecordings)
    updateSliderInput(
      session,
      "selected_time_range",
      min = as.POSIXct(recording_data$recording_first_call_datetime, format = "%Y-%m-%d %H:%M:%S"),
      max = as.POSIXct(recording_data$recording_last_call_datetime, format = "%Y-%m-%d %H:%M:%S"),
      value = c(as.POSIXct(recording_data$recording_first_call_datetime, format = "%Y-%m-%d %H:%M:%S"),
                as.POSIXct(recording_data$recording_last_call_datetime, format = "%Y-%m-%d %H:%M:%S"))
    )
    # updateTimeInput(
    #   session,
    #   "selected_time_start",
    #   value = as.POSIXct(recording_data$recording_first_call_datetime, format = "%Y-%m-%d %H:%M:%S")
    # )
    # updateTimeInput(
    #   session,
    #   "selected_time_end",
    #   value = as.POSIXct(recording_data$recording_last_call_datetime, format = "%Y-%m-%d %H:%M:%S")
    # )
  })
  
  # observeEvent(input$selected_time_start, {
  #   updateSliderInput(
  #     session,
  #     "selected_time_range",
  #     min = as.POSIXct(input$selected_time_start, format = "%Y-%m-%d %H:%M:%S")
  #   )
  # })
  # 
  # observeEvent(input$selected_time_end, {
  #   updateSliderInput(
  #     session,
  #     "selected_time_range",
  #     max = as.POSIXct(input$selected_time_end, format = "%Y-%m-%d %H:%M:%S")
  #   )
  # })
  
  observeEvent(input$selected_time_range, {
    updateSliderInput(
      session,
      "selected_scope_time_range",
      min = input$selected_time_range[1],
      max = input$selected_time_range[2],
      value = c(input$selected_time_range[1], input$selected_time_range[2])
    )
  })
  ###
  
  ### add mic markers when mic_df is uploaded
  observeEvent(mic_data$mic_df, {
    req(input$fileMic)
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        lat = mic_data$mic_df$X.lat.,
        lng = mic_data$mic_df$X.lng.,
        radius = 6,
        label = paste0("Mic ID: ", mic_data$mic_df$X.mic_id.),
        color = "red",
        stroke = FALSE, fillOpacity = 0.5
      ) %>%
      fitBounds(lng1 = min(mic_data$mic_df$X.lng.), lat1 = min(mic_data$mic_df$X.lat.),
                lng2 = max(mic_data$mic_df$X.lng.), lat2 = max(mic_data$mic_df$X.lat.))
  })
  ###
  
  ### recordings datatable
  recording_table_proxy <- DT::dataTableProxy('recording_table') # currently unused, todo: figure out how to use table proxy
  output$recording_table <- DT::renderDataTable({
    # todo: create proxy logic to stop having to reinitialise datatable 
    req(input$fileRecordings)
    print("update recordings table")

    recording_df_filtered <- recording_data$recording_master_df[
      recording_data$recording_master_df$X.measured_call_datetime. >= input$selected_scope_time_range[1] &
        recording_data$recording_master_df$X.measured_call_datetime. <= input$selected_scope_time_range[2],
    ]
    
    recording_data$recording_temp_df = recording_df_filtered
    
    datatable(recording_df_filtered, editable = list(target = 'cell', disable = list(columns = c(2, 3, 4, 5, 6, 7, 8))), rownames = FALSE,  extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))
  })
  ###
  
  ### mic datatable
  output$mic_table <- DT::renderDataTable({
    req(mic_data$mic_df)
    datatable(mic_data$mic_df, rownames = FALSE)
  })
  ###
  
  ### calls logic
  call_data <- reactiveValues(
    call_temp_df = data.frame(),
    call_master_df = data.frame(),
  )
  
  # remove calls
  observeEvent(input$remove_call, {
    selected_rows <- input$call_table_rows_selected
    call_data$call_master_df <- call_data$call_master_df[-(selected_rows),]
    ### todo: reset calls_ID value set in the recordings dataframe
    
  })
  #

  # calls datatable
  call_table_proxy <- DT::dataTableProxy('call_table')
  output$call_table <- DT::renderDataTable({
    # todo: create proxy logic to stop having to reinitialise datatable 
    req(input$fileRecordings)
    print("update calls table")
    
    call_data$call_temp_df = call_data$call_master_df
    
    datatable(call_data$call_temp_df, editable = list(target = 'cell', disable = list(columns = c(1, 2))), rownames = FALSE,  extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))
  })
  #
  
  # save changes from datatable edits
  observeEvent(input$recording_table_cell_edit, {
    edit = input$recording_table_cell_edit
    # find the recording ID which has been edited from datatable (temp dataframe) and save changes to master dataframe
    edited_recording_ID <- recording_data$recording_temp_df[edit$row, edit$col+3]
    recording_data$recording_master_df[recording_data$recording_master_df[,2] == edited_recording_ID, 2] <- edit$value
    
    ### todo: logic to export data to calls datatable
  })
  #
  
  # set new call ID
  observeEvent(input$set_new_call_ID, {
    selected_rows = input$recording_table_rows_selected
    selected_recording_ID <- recording_data$recording_temp_df[selected_rows, 3]
    selected_recording_ID <- as.integer(selected_recording_ID)
    
    call_ID <- max(recording_data$recording_master_df[,2]) + 1

    recording_data$recording_master_df[selected_rows, 2] <- new_call_ID
    
    ### todo: calculate mean datetime
    animal_ID = as.integer(0)
    recording_ID <- paste(selected_recording_ID, collapse = ", ")
    new_call <- cbind(animal_ID, call_ID, recording_ID)
    call_data$call_master_df <- rbind(call_data$call_master_df, new_call)
  })
  
  # edit call ID in 
  
  ###
  
  ### animals logic
  animal_data <- reactiveValues(
    animal_temp_df = data.frame(),
    animal_master_df = data.frame(),
  )
  
  # animals datatable
  animal_table_proxy <- DT::dataTableProxy('animal_table')
  output$animal_table <- DT::renderDataTable({
    # todo: create proxy logic to stop having to reinitialise datatable 
    req(input$fileRecordings)
    print("update animal table")
    
    animal_data$animal_temp_df = animal_data$animal_master_df
    
    datatable(animal_data$animal_temp_df, editable = list(target = 'cell', disable = list(columns = c(0, 1, 2))), rownames = FALSE,  extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))
  })
  #
  
  # set new animal ID
  observeEvent(input$set_new_animal_ID, {
    selected_rows = input$call_table_rows_selected
    selected_call_ID <- call_data$call_master_df[selected_rows, 2]
    selected_call_ID <- as.integer(selected_call_ID)
    
    print(selected_call_ID)
    
    # need to convert these two cols to int. Not sure why. Perhaps due to the cbind when making new calls observations
    call_data$call_master_df[,1] <- as.integer(call_data$call_master_df[,1])
    call_data$call_master_df[,2] <- as.integer(call_data$call_master_df[,2])
    
    new_animal_ID <- max(call_data$call_master_df[,1]) + 1
    
    selected_recording_ID <- array()
    
    # set new animal ID to all selected rows of calls
    for (i in 1:length(selected_call_ID)) {
      selected_row_call <- which(call_data$call_master_df[, 2] == selected_call_ID[i])
      selected_row_recording <- which(recording_data$recording_master_df[, 2] == selected_call_ID[i])
      
      recording_data$recording_master_df[selected_row_recording, 1] <- new_animal_ID
      call_data$call_master_df[selected_row_call, 1] <- new_animal_ID
      
      selected_recording_ID <- recording_data$recording_master_df[selected_row_recording, 1]
      selected_recording_ID <- cbind(selected_recording_ID, )
    }
    
    selected_call_ID <- paste(selected_call_ID, collapse = ", ")
    selected_recording_ID <- paste(selected_recording_ID, collapse = ", ")
    new_animal <- cbind(new_animal_ID, selected_call_ID, selected_recording_ID)

    animal_data$animal_master_df <- rbind(animal_data$animal_master_df, new_animal)
  })
  ###
  
  ### download recordings
  output$download_button <- downloadHandler(
    filename = function() {
      paste("data-", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep="")
    },
    content = function(file) {
      str(recording_data$recording_master_df)
      write.csv(recording_data$recording_master_df, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  ###
  
  ### adding arrows and dots to map
  arrows <- reactiveValues(
    coordinates = array(), 
    recording_ID = array(),
    call_ID = array(),
    gender_colour = array()
  )
  
  dots <- reactiveValues(
    coordinates = array(),
    recording_ID = array(),
    call_ID = array(),
    gender_colour = array()
  )
  
  # select rows in recordings datatable
  observeEvent(input$recording_table_rows_selected, ignoreNULL = FALSE, {
    selected_rows <- input$recording_table_rows_selected
    if (is.null(selected_rows)) {
      arrows$coordinates <- array()
      dots$coordinates <- array()
      return()
    }
    radius <- 1000 # meters. todo: calculate the optimum radius given mic coordinates
    
    # separate arrow and dots row index based on whether bearing is NA or not
    selected_rows_with_bearing <- c()
    selected_rows_without_bearing <- c()
    for (i in selected_rows) {
      if (is.na(recording_data$recording_master_df$X.measured_bearing[i])) {
        selected_rows_without_bearing <- c(selected_rows_without_bearing, i)
      } else {
        selected_rows_with_bearing <- c(selected_rows_with_bearing, i)
      }
    }
    
    arrows_coordinates_total <- array(NA, dim = c(2, 2, length(selected_rows_with_bearing)))
    dots_coordinates_total <- array(NA, dim = c(1, 2, length(selected_rows_without_bearing)))
    
    selected_mic_IDs_without_bearing <- recording_data$recording_master_df$X.mic_ID[selected_rows_without_bearing]
    selected_mic_IDs_with_bearing <- recording_data$recording_master_df$X.mic_ID[selected_rows_with_bearing]

    if (length(selected_rows_with_bearing) > 0) {
      for (i in 1:length(selected_rows_with_bearing)) {
        mic_row_index <- which(mic_data$mic_df$X.mic_id. == selected_mic_IDs_with_bearing[i])
        selected_mic_lat <- mic_data$mic_df$X.lat.[mic_row_index]
        selected_mic_lng <- mic_data$mic_df$X.lng.[mic_row_index]
        
        selected_mic_coordinates <- c(selected_mic_lng, selected_mic_lat)
        
        selected_bearing <- recording_data$recording_master_df$X.measured_bearing[selected_rows_with_bearing[i]]
        # calculate new arrowhead coordinates from mic coordinates and measured detection bearing
        arrow_head_coordinates <- geosphere::destPoint(p = c(selected_mic_lng, selected_mic_lat), 
                                                         b = selected_bearing * (180/pi), 
                                                         d = radius)
        arrow_coordinates <- rbind(selected_mic_coordinates, arrow_head_coordinates)
        rownames(arrow_coordinates) <- NULL
        colnames(arrow_coordinates) <- c("lng", "lat")
        arrows_coordinates_total[, , i] <- arrow_coordinates
      }
    }

    if (length(selected_rows_without_bearing) > 0 ) {
      for (i in 1:length(selected_rows_without_bearing)) {
        mic_row_index <- which(mic_data$mic_df$X.mic_id == selected_mic_IDs_without_bearing[i])
        selected_mic_lat <- mic_data$mic_df$X.lat[mic_row_index]
        selected_mic_lng <- mic_data$mic_df$X.lng[mic_row_index]
        
        selected_mic_coordinates <- c(selected_mic_lng, selected_mic_lat)
        # rownames(selected_mic_coordinates) <- NULL
        # colnames(selected_mic_coordinates) <- c("lng", "lat")
        dots_coordinates_total[, , i] <- selected_mic_coordinates
      }
    }
    
    if (length(selected_rows_with_bearing) > 0) {
      arrows$coordinates <- arrows_coordinates_total
      arrows$recording_ID <- recording_data$recording_master_df$X.recording_ID[selected_rows_with_bearing]
      arrows$call_ID <- recording_data$recording_master_df$call_ID[selected_rows_with_bearing]
      arrows$gender_colour <- recording_data$recording_master_df$gender_colour[selected_rows_with_bearing]
    } else {
      arrows$coordinates <- array()
    }

    if (length(selected_rows_without_bearing) > 0) {
      dots$coordinates <- dots_coordinates_total
      dots$recording_ID <- recording_data$recording_master_df$X.recording_ID[selected_rows_without_bearing]
      dots$call_ID <- recording_data$recording_master_df$call_ID[selected_rows_without_bearing]
      dots$gender_colour <- recording_data$recording_master_df$gender_colour[selected_rows_without_bearing]
    } else {
      dots$coordinates <- array()
    }
  })
  
  
  # update map with bearing directions for selected recordings which do have a bearing
  observeEvent(arrows$coordinates, {
    leafletProxy("map") %>% clearGroup("arrows")
    # prevents plotting arrows on declaration
    if (!any(is.na(arrows$coordinates))) {
      for (i in 1:dim(arrows$coordinates)[3]) {
        leafletProxy("map") %>% addArrowhead(data = arrows$coordinates[,,i], group = "arrows", layerId = paste0("arrow_", i), label = paste0("Recording ID: ", arrows$recording_ID[i], ", Call ID: ", arrows$call_ID[i]), color = arrows$gender_colour[i], opacity = 50, 
                                             options = arrowheadOptions(yawn = 40, fill = FALSE))
      }
    }
  })
  
  # update map with dots for selected recordings which don't have a bearing
  observeEvent(dots$coordinates, {
    leafletProxy("map") %>% clearGroup("dots")
    # prevents plotting arrows on declaration
    if (!any(is.na(dots$coordinates))) {
      for (i in 1:dim(dots$coordinates)[3]) {
        leafletProxy("map") %>% addCircleMarkers(lat = dots$coordinates[,,i][2], lng = dots$coordinates[,,i][1], radius = 8, group = "dots", layerId = paste0("dot_", i), label = paste0("Recording ID: ", dots$recording_ID[i], ", Call ID: ", dots$call_ID[i]), color = dots$gender_colour[i], stroke = FALSE, fillOpacity = 50)
      }
    }
  })
  ###
  
  ### what section is this under???
  # select rows in calls datatable
  observeEvent(input$call_table_rows_selected, ignoreNULL = FALSE, {
    selected_rows <- input$call_table_rows_selected
    if (is.null(selected_rows)) {
      arrows$coordinates = array()
      return()
    }
    
    selected_recordings <- call_data$call_master_df$selected_recording_ID[selected_rows]
    selected_recordings <- as.integer(unlist(strsplit(selected_recordings, split = ",\\s*")))
    
    radius = 1000 # meters. todo: calculate the optimum radius given mic coordinates
    
    arrow_coordinates_total <- array(NA, dim = c(2, 2, length(selected_recordings)))
    
    for (i in 1:length(selected_recordings)) {
      selected_row <- which(recording_data$recording_master_df$X.recording_ID. == selected_recordings[i])
      selected_mic <- recording_data$recording_temp_df$X.mic_ID.[selected_row]
      
      selected_mic_lat <- mic_data$mic_df$X.lat.[selected_mic]
      selected_mic_lng <- mic_data$mic_df$X.lng.[selected_mic]
      
      selected_mic_coordinates <- c(selected_mic_lng, selected_mic_lat)
      selected_bearing <- recording_data$recording_master_df$X.measured_bearing.[selected_row]
      
      # calculate new arrowhead coordinates from mic coordinates and measured detection bearing
      arrow_head_coordinates <- geosphere::destPoint(p = c(selected_mic_lng, selected_mic_lat), b = selected_bearing * (180/pi), d = radius)
      arrow_coordinates <- rbind(selected_mic_coordinates, arrow_head_coordinates)
      rownames(arrow_coordinates) = NULL
      colnames(arrow_coordinates) = c("lng", "lat")
      
      arrow_coordinates_total[, , i] <- arrow_coordinates
    }
    # arrow_coordinates_total_lines <- SpatialLines(arrow_coordinates_total)
    arrows$coordinates <- arrow_coordinates_total
    arrows$recording_ID <- recording_data$recording_master_df$X.recording_ID[selected_rows]
    arrows$call_ID <- recording_data$recording_master_df$call_ID[selected_rows]
  })
  ###

  ### file uploads
  mic_data <- reactiveValues(
    mic_df = data.frame()
  )
  
  observeEvent(input$fileMic, {
    req(input$fileMic)
    mic_data$mic_df <- read.csv(input$fileMic$datapath,
                       header = TRUE,
                       sep = ",",
                       quote = "")
    ### todo: convert UTM to lat / lon
    ### todo: check if all mic IDs are different
  })

  recording_data <- reactiveValues(
    recording_temp_df = data.frame(),
    recording_master_df = data.frame(),
    recording_first_call_datetime = as.POSIXct("2023-01-01 00:00:00"),
    recording_last_call_datetime = as.POSIXct("2023-01-01 00:00:00")
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
    
    call_ID <- rep(0, nrow(recording_master_df))
    animal_ID <- rep(0, nrow(recording_master_df))
    
    # assign gender colour for arrows
    recording_master_df$X.measured_gender <- gsub('"', '', as.character(recording_master_df$X.measured_gender))
    gender <- recording_master_df$X.measured_gender
    gender_colour <- ifelse(gender == "M", "blue",
                            ifelse(gender == "F", "pink", "grey"))
    recording_master_df$gender_colour <- gender_colour
    
    recording_master_df <- cbind(animal_ID, call_ID, recording_master_df)
    
    recording_master_df$X.recording_ID <- as.integer(recording_master_df$X.recording_ID)
    
    recording_data$recording_temp_df <- recording_master_df
    recording_data$recording_master_df <- recording_master_df
  })
  
  #gibbon_df <- reactive({
  #  req(input$fileGibbons)
  #  gibbon_df <- read.csv(input$fileGibbons$datapath,
  #                        header = TRUE,
  #                        sep = ",",
  #                        quote = "")
  #  return(gibbon_df)
  #})
  ###
  
  ### map
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = 0, lat = 0, zoom = 3)
  })
  
  observeEvent(input$home_map_view, {
    req(input$fileMic)
    leafletProxy("map") %>%
      fitBounds(lng1 = min(mic_data$mic_df$X.lng.), lat1 = min(mic_data$mic_df$X.lat.),
                lng2 = max(mic_data$mic_df$X.lng.), lat2 = max(mic_data$mic_df$X.lat.))
  })
  ###
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)