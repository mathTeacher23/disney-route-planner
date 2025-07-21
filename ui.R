ui <- fluidPage(
  titlePanel("Disney Transportation Route Planner"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("rp_location_start", "From:", choices = location_lookup$Location),
      selectInput("rp_location_end", "To:", choices = location_lookup$Location),
      
      selectInput(
        "tile_style", "Map Style:",
        choices = c(
          "OpenStreetMap" = "OpenStreetMap",
          "Carto (Light)" = "CartoDB.Positron",
          "Carto (Dark)" = "CartoDB.DarkMatter",
          "Satellite" = "Esri.WorldImagery"
        ),
        selected = "CartoDB.Positron"
      ),
      
      actionButton("plan_route", "Plan Route")
    ),
    
    mainPanel(
      h4("Route Message"),
      verbatimTextOutput("route_message"),
      br(),
      leafletOutput("route_map", height = 600)
    )
  )
)
