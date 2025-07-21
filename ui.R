ui <- page_sidebar(
  title = "🏰 Disney Transportation Route Planner",
  theme = bs_theme(bootswatch = "sandstone"),
  useShinyjs(),
  
  sidebar = sidebar(
    position = "left",
    width = 400,
    open = TRUE,
    tags$style(HTML("
  .highlight-card {
    background-color: #f5f5f5 !important;
    border: none !important;
    border-radius: 8px !important;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2) !important;
    overflow: visible !important;          /* allow dropdowns to overflow */
    position: relative !important;         /* needed for proper positioning */
    z-index: 1;
    margin-bottom: 20px;
  }

  /* Make sure the dropdown appears above everything */
  .selectize-dropdown {
    z-index: 9999 !important;
  }

  /* Optional: taller scrollable dropdown content */
  .selectize-dropdown-content {
    max-height: 300px !important;
    overflow-y: auto !important;
  }

  .highlight-card .card-header {
    background-color: #f5f5f5 !important;
    border-bottom: none !important;
    font-weight: bold;
    font-size: 1.1rem;
    padding: 15px 20px;
    color: #333;
  }

  .highlight-card .card-body {
    background-color: #f5f5f5 !important;
    border: none !important;
    padding: 20px !important;
    margin: 0 !important;
    box-shadow: none !important;
  }

  .highlight-card pre {
    white-space: pre-wrap !important;   /* wrap long lines */
    word-break: break-word !important;  /* break long words */
    overflow-x: hidden !important;      /* remove horizontal scrollbar */
  }

  .highlight-card:hover {
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.3) !important;
  }
")),
  card(
    class = "highlight-card",
    style = "height: 610px;",
    card_header("Route Planner"),
    card_body(
      selectInput("rp_location_start", "Start Location", choices = c("", unique(location_lookup$Location)), selected = ""),
      selectInput("rp_location_end", "End Location", choices = c("", unique(location_lookup$Location)), selected = ""),
      
      actionButton("plan_route", "Plan Route", class = "btn-primary"),
      HTML("<p> <b> Suggested Route </b></p>"),
      #textOutput("route_message", container = tags$p),
      uiOutput("route_message"),
      actionButton("clear_route", "Clear Route")
      
    )
  )
    ),
    
    mainPanel(
      width = 800,
      leafletOutput("route_map", height = 600) 
    )
)