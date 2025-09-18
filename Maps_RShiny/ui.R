# ui.r

fluidPage(
  theme = shinytheme("flatly"),
  tags$style(HTML("
    .shiny-input-container { margin-bottom: 1rem; }
    .sidebar .well { background: #f8f9fa; border: none; }
    #habitat_map { border: 2px solid #e2e3e5; border-radius: 5px; }
    .introjs-tooltip { font-size: 1.2rem; }
  ")),
  introjsUI(),
  
  titlePanel("Gulf Council EFH 5-year Review Habitat Maps"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Project Overview"),
      tags$p("This app allows users to explore EFH polygons for managed species by life stage."),
      
      actionButton("start_tour", "Take Tour", class = "btn-primary"),
      actionButton("efh_descriptions", "EFH Descriptions", class = "btn-info"),
      actionButton("artificial_reef", "Artificial Reef", class = "btn-warning"),
      br(), br(),
      
      # Habitat Type Dropdown
      div(id = "habitat_box",
          selectInput(
            "selected_habitat",
            label = "Select Habitat Type:",
            choices = habitat_choices,  # 
            selected = NULL,            # nothing selected by default
            multiple = TRUE
          )
      ),
      
      # Habitat Zone Dropdown 
      div(id = "zone_box",
          selectInput(
            "selected_zone",
            label = "Select Habitat Zone:",
            choices = names(zone_choices),  # only real zones
            selected = NULL,                # nothing selected by default
            multiple = TRUE                 # allow multiple selections
          )
      ),
      
      #Species dropdown remains below habitat
      div(id = "species_box",
          selectInput("selected_species", "Select Species:", choices = species_lookup, selected=NULL)
      ),
      checkboxGroupInput("selected_stages", "Select Life Stages to Display:",
                         choices = lifestage_labels,
                         selected = "Adult"),
     
      tags$div("Data updated: July 2025", style = "font-size:80%; color:grey;")
    ),
    mainPanel(
      leafletOutput("habitat_map", height = "90vh") %>% withSpinner(color = "#2C3E50")
    )
  )
)



      
   