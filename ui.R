ui <- page_sidebar(
  title = "🏰 Disney World Transportation Route Planner",
  theme = bs_theme(bootswatch = "sandstone"),
  useShinyjs(),
  
  sidebar = sidebar(
    position = "left",
    width = 400,
    open = TRUE,
    
    # Global custom styles
    tags$style(HTML("
      /* Card styling */
      .highlight-card {
        background-color: #f5f5f5 !important;
        border: none !important;
        border-radius: 8px !important;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2) !important;
        overflow: visible !important;
        position: relative !important;
        z-index: 1;
        margin-bottom: 20px;
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
        white-space: pre-wrap !important;
        word-break: break-word !important;
        overflow-x: hidden !important;
      }

      .highlight-card:hover {
        box-shadow: 0 8px 20px rgba(0, 0, 0, 0.3) !important;
      }

      /* Selectize dropdown fix */
      .selectize-dropdown-content {
        max-height: 300px !important;
        overflow-y: auto !important;
      }

      .selectize-dropdown {
        z-index: 9999 !important;
      }
    ")),
    
    wellPanel(
      selectInput(
        inputId = "rp_location_start",
        label = "Start Location",
        choices = c("", unique(location_lookup$Location)),
        selected = "",
        selectize = TRUE
      ),
      selectInput(
        inputId = "rp_location_end",
        label = "End Location",
        choices = c("", unique(location_lookup$Location)),
        selected = ""
      ),
      actionButton("plan_route", "Plan Route", class = "btn-primary"),
      actionButton("clear_route", "Clear Route"),
      hr(),
      actionButton("swap_locations", "🔁 Swap", class = "btn-secondary")
    ),
    
    wellPanel(
      HTML("<p><b>How do you get to your destination?</b></p>"),
      uiOutput("route_message"),
      hr(),
      HTML("<p><b>What's near your destination?</b></p>"),
      uiOutput("nearby_places")
    )
  ),
  
  mainPanel(
    width = 800,
    leafletOutput("route_map", height = 750)
  )
)
