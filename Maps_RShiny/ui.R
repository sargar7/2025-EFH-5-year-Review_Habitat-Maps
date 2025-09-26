# ui.r
fluidPage(
  theme = shinytheme("flatly"),
  ## make dropdown boxes white 
  tags$style(HTML("
    .shiny-input-container { margin-bottom: 1rem; }
    .sidebar .well { background: #f8f9fa; border: none; }
    #habitat_map { border: 2px solid #e2e3e5; border-radius: 5px; }
    .introjs-tooltip { font-size: 1.2rem; }

    /* PickerInput white background */
    .bootstrap-select .dropdown-toggle {
      background-color: white !important;
      color: black !important;
    }
    .bootstrap-select .dropdown-menu {
      background-color: white !important;
    }
    .bootstrap-select .dropdown-menu li a {
      color: black !important;
    }
    /* Optional: hover effect in dropdown */
    .bootstrap-select .dropdown-menu li a:hover {
      background-color: #f0f0f0 !important;
      color: black !important;
    }                              
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
      
      # Habitat Type Dropdown with X remove
      div(id = "habitat_box",
          pickerInput(
            inputId = "selected_habitat",
            label = "Select Habitat Type:",
            choices = habitat_choices,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `none-selected-text` = "Select habitat type"
            )
          )
      ),
      
      # Habitat Zone Dropdown with X remove
      div(id = "zone_box",
          pickerInput(
            inputId = "selected_zone",
            label = "Select Habitat Zone:",
            choices = names(zone_choices),
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `none-selected-text` = "Select habitat zone"
            )
          )
      ),
      
      # Species Dropdown with clear X button
      div(id = "species_box",
          pickerInput(
            inputId = "selected_species",
            label = "Select Species:",
            choices = c("None" = "", species_lookup),
            selected = "",
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              `none-selected-text` = "Select species",
              `allow-clear` = TRUE
            )
          )
      ),
      
      # Life stages checkboxes remain unchanged
      checkboxGroupInput("selected_stages", "Select Life Stages to Display:",
                         choices = lifestage_labels,
                         selected = "Adult"),
      
      tags$div("Data updated: 2024", style = "font-size:80%; color:darkgrey;"),
      tags$div("Contact: sarah.gardiner@gulfcouncil.org for more information", style="font-size:80%; color:grey;")
    ),
    
    mainPanel(
      leafletOutput("habitat_map", height = "90vh") %>% withSpinner(color = "#2C3E50")
    )
  )
)

