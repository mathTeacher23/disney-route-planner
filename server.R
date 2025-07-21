server <- function(input, output, session) {
  
  route_bounds <- reactiveVal(NULL)
  
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
    if (!is.null(bounds)) {
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
    if (!is.null(bounds)) {
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
    
    # Save bounds
    route_bounds(list(
      lng1 = min(lngs), lat1 = min(lats),
      lng2 = max(lngs), lat2 = max(lats)
    ))
    
    leafletProxy("route_map") %>%
      fitBounds(min(lngs), min(lats), max(lngs), max(lats))
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
    rp <- route_plan()
    if (is.null(rp$route)) return(NULL)
    
    nearby_df <- get_leaflet_data()
    pal <- colorFactor(palette = 'Set1', domain = nearby_df$Park_Proximity)
    
    start_name <- rp$start$Location
    end_name <- rp$end$Location
    
    nearby_df <- nearby_df %>%
      mutate(
        is_start = Location == start_name,
        is_end = Location == end_name,
        label_text = case_when(
          is_start ~ paste0("Start: ", Location),
          is_end ~ paste0("End: ", Location),
          TRUE ~ NA_character_
        )
      )
    
    map <- leaflet(data = nearby_df) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~Radius,
        opacity = 1,
        fillOpacity = 1,
        color = "black",
        fillColor = ~pal(Park_Proximity),
        label = ~label_text,
        labelOptions = labelOptions(noHide = FALSE, direction = "auto"),
        popup = ~paste(Description, "<br><br><b>Transportation Access:</b> ", Transportation_Access)
      ) %>%
      addPolylines(data = rp$route, color = "blue", weight = 4) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI") %>%
      addProviderTiles("CartoDB.Positron", group = "Carto Light") %>%
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
      )
    
    # Attach the onRender JS to detect blank map clicks and popup close
    htmlwidgets::onRender(map, "
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
    # If both have Skyliner and share proximity, allow that as a special case
    if ("Skyliner" %in% start_modes && "Skyliner" %in% end_modes &&
        start_prox == end_prox && start_prox %in% c("Epcot", "Hollywood Studios")) {
      return("Both resorts are on the Skyliner route. You may take the Skyliner directly between them.")
    }
    
    # Get the park to route through — based on start's Park_Proximity
    hub <- lookup %>%
      filter(Class %in% c("Park", "Shopping Plaza"), Location == start_prox | Location == end_prox)
    
    if (nrow(hub) == 0) {
      return("Could not find a nearby park hub for routing.")
    }
    
    hub_row <- hub[1, ]  # Use first match
    
    hub_modes <- split_modes(hub_row$Transportation_Access)
    
    # Leg 1: start → hub
    leg1_modes <- clean_intersect(start_modes, hub_modes, start_prox, hub_row$Park_Proximity)
    # Leg 2: hub → end
    leg2_modes <- clean_intersect(end_modes, hub_modes, end_prox, hub_row$Park_Proximity)
    
    if (length(leg1_modes) == 0 || length(leg2_modes) == 0) {
      return("No valid transportation path found between these resorts via Disney transportation.")
    }
    
    msg <- paste0(
      "Start at ", start_loc, " and take the ", paste(leg1_modes, collapse = ", "),
      " to ", hub_row$Location, ". Then take the ", paste(leg2_modes, collapse = ", "),
      " to ", end_loc, "."
    )
    
    return(msg)
  }
  
  route_msg <- eventReactive(input$plan_route, {
    get_route_message(input$rp_location_start, input$rp_location_end, location_lookup)
  })
  
  output$route_message <- renderText({
    route_msg()
  })
  
  
  
} # end of server
