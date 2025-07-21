server <- function(input, output, session) {
  
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
  
  output$route_map <- renderLeaflet({
    rp <- route_plan()
    if (is.null(rp$route)) return(NULL)
    
    leaflet() %>%
      addProviderTiles(input$tile_style) %>%
      addPolylines(data = rp$route, color = "blue", weight = 4) %>%
      
      # Start marker with label and popup
      addMarkers(
        lng = rp$start$Longitude,
        lat = rp$start$Latitude,
        label = paste0("Start: ", input$rp_location_start),
        popup = paste0("Start: ", input$rp_location_start),
        labelOptions = labelOptions(noHide = TRUE, direction = "auto")
      ) %>%
      
      # End marker with label and popup
      addMarkers(
        lng = rp$end$Longitude,
        lat = rp$end$Latitude,
        label = paste0("End: ", input$rp_location_end),
        popup = paste0("End: ", input$rp_location_end),
        labelOptions = labelOptions(noHide = TRUE, direction = "auto")
      )
  })
  
  
  get_route_message <- function(start_loc, end_loc, lookup) {
    split_modes <- function(x) {
      if (is.na(x) || x == "") return(character(0))
      trimws(unlist(strsplit(x, ",")))
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
    hub <- lookup %>% filter(Location == start_prox, Class %in% c("Park", "Shopping Plaza"))
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
