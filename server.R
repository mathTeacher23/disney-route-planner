server <- function(input, output, session) {
  
  route_bounds <- reactiveVal(NULL)
  initial_bounds <- reactiveVal(NULL)
  route_text <- reactiveVal("")
  
  route_plan <- eventReactive(input$plan_route, {
    req(input$rp_location_start, input$rp_location_end)
    
    start <- filter(location_lookup, Location == input$rp_location_start)
    end   <- filter(location_lookup, Location == input$rp_location_end)
    
    if (nrow(start) == 0 || nrow(end) == 0) {
      return(list(msg = "❌ Invalid start or end location selected.", route = NULL))
    }
    
    # Convert to sf points
    start_sf <- sf::st_as_sf(start, coords = c("Longitude", "Latitude"), crs = 4326)
    end_sf   <- sf::st_as_sf(end,   coords = c("Longitude", "Latitude"), crs = 4326)
    
    # Route using public OSRM API
    route <- tryCatch({
      osrm::osrmRoute(src = start_sf, dst = end_sf, overview = "full", returnclass = "sf")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(route)) {
      return(list(msg = "❌ Routing failed. OSRM may be unreachable.", route = NULL))
    }
    
    msg <- paste0("✅ Route from ", start$Location, " to ", end$Location, 
                  "\nDistance: ", round(route$distance, 2), " km",
                  "\nDuration: ", round(route$duration, 1), " mins")
    
    list(msg = msg, route = route, start = start, end = end)
  })
  
  output$route_message <- renderText({
    rp <- route_plan()
    rp$msg
  })
  
  observe({
    toggleState("plan_route", !is.null(input$rp_location_start) && input$rp_location_start != "" &&
                  !is.null(input$rp_location_end) && input$rp_location_end != "")
  })
  
  # When a marker is clicked: zoom in and center
  observeEvent(input$route_map_marker_click, {
    click <- input$route_map_marker_click
    
    leafletProxy("route_map") %>%
      setView(
        lng = as.numeric(click$lng),
        lat = as.numeric(click$lat) + 0.03,
        zoom = 13
      )
  })
  
  observeEvent(input$map_clicked_blank, {
    bounds <- route_bounds()
    if (is.null(bounds)) {
      # No route displayed, reset to initial map bounds
      init_bounds <- initial_bounds()
      if (!is.null(init_bounds)) {
        leafletProxy("route_map") %>%
          fitBounds(
            lng1 = init_bounds$lng1,
            lat1 = init_bounds$lat1,
            lng2 = init_bounds$lng2,
            lat2 = init_bounds$lat2
          )
      }
    } else {
      # Route exists, fit to route bounds
      leafletProxy("route_map") %>%
        fitBounds(
          lng1 = bounds$lng1,
          lat1 = bounds$lat1,
          lng2 = bounds$lng2,
          lat2 = bounds$lat2
        )
    }
  })
  
  observeEvent(input$popup_closed, {
    bounds <- route_bounds()
    if (is.null(bounds)) {
      # No route displayed, reset to initial map bounds
      init_bounds <- initial_bounds()
      if (!is.null(init_bounds)) {
        leafletProxy("route_map") %>%
          fitBounds(
            lng1 = init_bounds$lng1,
            lat1 = init_bounds$lat1,
            lng2 = init_bounds$lng2,
            lat2 = init_bounds$lat2
          )
      }
    } else {
      # Route exists, fit to route bounds
      leafletProxy("route_map") %>%
        fitBounds(
          lng1 = bounds$lng1,
          lat1 = bounds$lat1,
          lng2 = bounds$lng2,
          lat2 = bounds$lat2
        )
    }
  })
  
  observeEvent(input$plan_route, {
    rp <- route_plan()
    if (is.null(rp$route)) return()
    
    lngs <- c(rp$start$Longitude, rp$end$Longitude)
    lats <- c(rp$start$Latitude, rp$end$Latitude)
    
    # Save the route bounds (for zooming on route)
    route_bounds(list(
      lng1 = min(lngs), lat1 = min(lats),
      lng2 = max(lngs), lat2 = max(lats)
    ))
    
    leafletProxy("route_map") %>%
      clearGroup("route_line") %>%
      addPolylines(data = rp$route, color = "blue", weight = 7, group = "route_line") %>%
      
      # Add labels for start and end locations
      addLabelOnlyMarkers(
        lng = rp$start$Longitude,
        lat = rp$start$Latitude,
        label = rp$start$Location,
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textsize = "13px"),
        group = "route_line"
      ) %>%
      addLabelOnlyMarkers(
        lng = rp$end$Longitude,
        lat = rp$end$Latitude,
        label = rp$end$Location,
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textsize = "13px"),
        group = "route_line"
      ) %>%
      
      fitBounds(min(lngs), min(lats), max(lngs), max(lats))
    
  })
  
  observeEvent(input$clear_route, {
    leafletProxy("route_map") %>%
      clearGroup("route_line")
    
    route_bounds(NULL)  # Clear route bounds since no route displayed
    
    init_bounds <- initial_bounds()
    if (!is.null(init_bounds)) {
      leafletProxy("route_map") %>%
        fitBounds(
          lng1 = init_bounds$lng1,
          lat1 = init_bounds$lat1,
          lng2 = init_bounds$lng2,
          lat2 = init_bounds$lat2
        )
    }
    
    # NEW: Clear selected inputs
    updateSelectInput(session, "rp_location_start", selected = "")
    updateSelectInput(session, "rp_location_end", selected = "")
    
    # Clear route text
    route_text("")
  })
  
  
  
  get_leaflet_data <- function(){
    leaflet_data <- disney_df
    
    #Merge Class & Id to single column
    leaflet_data$Destination <- paste0(leaflet_data$Class, ": ", leaflet_data$Id, ", (", leaflet_data$Price, ")")
    
    #Group by Proximity and concat to single string all Ids
    leaflet_data %>%
      filter(Class %in% c("Resort", "Park", "Hotel", "Shopping Plaza", "Dining")) %>%
      arrange(Location, Class, Id) %>%
      group_by(Location) %>%
      mutate(Close_By = paste0(Destination, collapse = '<br>')) -> nearby_df
    
    #Join the HTML description and photos from blurb df to the leaflet dataframe
    nearby_df <- nearby_df %>% left_join(blurb_df, by = 'Location')
    nearby_df <- nearby_df[!(nearby_df$Class %in% c('Dining', 'Animal Highlights', 'Attraction', 'Guest Services', 'Shopping')), ]
    return(nearby_df)
  }
  
  output$route_map <- renderLeaflet({
    # Load base data
    nearby_df <- get_leaflet_data()
    pal <- colorFactor(palette = 'Set1', domain = nearby_df$Park_Proximity)
    
    # Calculate and save initial bounds when the map loads for the first time
    lngs <- nearby_df$Longitude
    lats <- nearby_df$Latitude
    initial_bounds(list(
      lng1 = min(lngs), lat1 = min(lats),
      lng2 = max(lngs), lat2 = max(lats)
    ))
    
    leaflet(data = nearby_df) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
      addProviderTiles("CartoDB.Positron", group = "Carto Light") %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~Radius,
        opacity = 1,
        fillOpacity = 1,
        color = "black",
        fillColor = ~pal(Park_Proximity),
        label = ~Location,  
        popup = ~paste(Description, "<br><br><b>Transportation Access:</b> ", Transportation_Access)
      ) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>%
      addLayersControl(baseGroups = c("Carto Light", "ESRI", "OSM")) %>%
      addLegend(
        pal = pal,
        position = "bottomright",
        values = ~Park_Proximity,
        title = "Nearby Disney Parks & Shopping",
        opacity = 1
      ) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('click', function(e) {
          Shiny.setInputValue('map_clicked_blank', Date.now());
        });
        map.on('popupclose', function(e) {
          Shiny.setInputValue('popup_closed', Date.now());
        });
      }
    ")
  })
  
  
  get_route_message <- function(start_loc, end_loc, lookup) {
    split_modes <- function(x) {
      if (is.na(x) || x == "") return(character(0))
      trimws(unlist(strsplit(x, ",")))
    }
    
    # Safety check for identical locations
    if (start_loc == end_loc) {
      return(paste("You're already at", start_loc, "— no travel needed!"))
    }
    
    # Get start/end rows
    start_row <- lookup %>% filter(Location == start_loc)
    end_row <- lookup %>% filter(Location == end_loc)
    
    # Safety check
    if (nrow(start_row) == 0 || nrow(end_row) == 0) return("Invalid location selection.")
    
    # --- NEW WALK CHECK ---
    walkable_str <- start_row$Walkable_To
    walkable_vec <- split_modes(walkable_str)
    if (end_loc %in% walkable_vec) {
      return(paste("You can walk directly from", start_loc, "to", end_loc, "."))
    }
    # ----------------------
    
    start_class <- start_row$Class
    end_class <- end_row$Class
    start_prox <- start_row$Park_Proximity
    end_prox <- end_row$Park_Proximity
    
    start_modes <- split_modes(start_row$Transportation_Access)
    end_modes <- split_modes(end_row$Transportation_Access)
    
    # Helper: intersect and remove "Boat" if park proximity mismatches
    clean_intersect <- function(a, b, prox1, prox2) {
      modes <- intersect(a, b)
      if ("Boat" %in% modes && prox1 != prox2) {
        modes <- setdiff(modes, "Boat")
      }
      modes
    }
    
    # CASE 1: Not both Resorts — direct route
    if (!(start_class == "Resort" && end_class == "Resort")) {
      modes <- clean_intersect(start_modes, end_modes, start_prox, end_prox)
      if (length(modes) == 0) {
        return("No direct transportation options available between these locations.")
      }
      return(paste("Travel by:", paste(modes, collapse = ", ")))
    }
    
    # CASE 2: Both are Resorts — must route through Park or Disney Springs
    # Skyliner special case
    if ("Skyliner" %in% start_modes && "Skyliner" %in% end_modes &&
        start_prox == end_prox && start_prox %in% c("Epcot", "Hollywood Studios")) {
      return("Both resorts are on the Skyliner route. You may take the Skyliner directly between them.")
    }
    
    # Find hubs for start and end proximities
    hubs <- lookup %>%
      filter(Class %in% c("Park", "Shopping Plaza") & (Location == start_prox | Location == end_prox))
    
    if (nrow(hubs) == 0) {
      return("Could not find a nearby park hub for routing.")
    }
    
    # Get hubs individually for start and end proximity
    start_hub <- hubs %>% filter(Location == start_prox)
    end_hub <- hubs %>% filter(Location == end_prox)
    
    # Ensure fallback if either hub missing
    if (nrow(start_hub) == 0) start_hub <- hubs[1, ]
    if (nrow(end_hub) == 0) end_hub <- hubs[1, ]
    
    # Helper to get modes for leg
    get_leg_modes <- function(from_modes, to_hub) {
      clean_intersect(from_modes, split_modes(to_hub$Transportation_Access), 
                      from_modes = from_modes, prox1 = NA, prox2 = NA)
    }
    
    # Option 1: start → start_hub → end
    leg1_modes_1 <- clean_intersect(start_modes, split_modes(start_hub$Transportation_Access), start_prox, start_hub$Park_Proximity)
    leg2_modes_1 <- clean_intersect(end_modes, split_modes(start_hub$Transportation_Access), end_prox, start_hub$Park_Proximity)
    
    # Option 2: start → end_hub → end
    leg1_modes_2 <- clean_intersect(start_modes, split_modes(end_hub$Transportation_Access), start_prox, end_hub$Park_Proximity)
    leg2_modes_2 <- clean_intersect(end_modes, split_modes(end_hub$Transportation_Access), end_prox, end_hub$Park_Proximity)
    
    # Build messages for both options if valid
    messages <- c()
    
    if (length(leg1_modes_1) > 0 && length(leg2_modes_1) > 0) {
      messages <- c(messages,
                    paste0(
                      "Option 1: Start at ", start_loc, " and take the ", paste(leg1_modes_1, collapse = ", "),
                      " to ", start_hub$Location, ". Then take the ", paste(leg2_modes_1, collapse = ", "),
                      " to ", end_loc, "."
                    )
      )
    }
    
    if (length(leg1_modes_2) > 0 && length(leg2_modes_2) > 0) {
      messages <- c(messages,
                    paste0(
                      "Option 2: Start at ", start_loc, " and take the ", paste(leg1_modes_2, collapse = ", "),
                      " to ", end_hub$Location, ". Then take the ", paste(leg2_modes_2, collapse = ", "),
                      " to ", end_loc, "."
                    )
      )
    }
    
    if (length(messages) == 0) {
      return("No valid transportation path found between these resorts via Disney transportation.")
    }
    
    # Return combined options separated by newlines
    paste(messages, collapse = "\n\n")
  }
  
  
  observeEvent(input$plan_route, {
    msg <- get_route_message(input$rp_location_start, input$rp_location_end, location_lookup)
    route_text(msg)
  })
  
  
  
  output$route_message <- renderUI({
    # route_msg() returns text with \n line breaks
    HTML(gsub("\n", "<br>", route_text()))
  })
  
  
  
  
} # end of server
