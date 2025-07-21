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
      addMarkers(lng = rp$start$Longitude, lat = rp$start$Latitude,
                 popup = rp$start$Location, label = "Start") %>%
      addMarkers(lng = rp$end$Longitude, lat = rp$end$Latitude,
                 popup = rp$end$Location, label = "End")
  })
  
} # end of server
