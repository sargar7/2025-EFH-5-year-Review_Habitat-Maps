#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#getwd() ### double check correct wd

#### EFH 5 year review App

library(shiny)
source("global.r")

## APP using RDS files on the server
#Gulf of Mexico - Documents\EFH\EFH Generic Amendment 5\SHP_species_maps\RDS_simplified

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$style(HTML("
    .shiny-input-container { margin-bottom: 1rem; }
    .sidebar .well { background: #f8f9fa; border: none; }
    #habitat_map { border: 2px solid #e2e3e5; border-radius: 5px; }
    .introjs-tooltip { font-size: 1.2rem; }
  ")),
  introjsUI(),
  
  titlePanel("Gulf of Mexico - EFH Habitat Maps"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Project Overview"),
      tags$p("This app allows users to explore EFH polygons for managed species by life stage."),
      
      actionButton("start_tour", "Take Tour", class = "btn-primary"),
      actionButton("layer_info", "Layer Info", class = "btn-info"),
      br(), br(),
      
      div(id = "species_box",
          selectInput("selected_species", "Select Species:", choices = species_labels)
      ),
      checkboxGroupInput("selected_stages", "Select Life Stages to Display:",
                         choices = lifestage_labels,
                         selected = lifestage_labels),
      
      tags$div("Data updated: July 2025", style = "font-size:80%; color:grey;")
    ),
    mainPanel(
      leafletOutput("habitat_map", height = "90vh") %>% withSpinner(color = "#2C3E50")
    )
  )
)

server <- function(input, output, session) {
  # Start Tour
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = data.frame(
      element = c("#species_box", "#habitat_map", "#layer_info"),
      intro = c(
        "Use the dropdown to explore EFH layers for each species.",
        "The map displays EFH polygons by selected life stages.",
        "Click 'Layer Info' to view descriptions of each map layer."
      ),
      position = c("bottom", "top", "top")
    )))
  })
  
  # Render the map with Esri basemap
  output$habitat_map <- renderLeaflet({
    leaflet() %>%
      leaflet.esri::addEsriBasemapLayer("Imagery") %>%
      fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2)
  })
  
  # Add EFH polygons and legend
  
  observe({
    req(input$selected_species)
    
    leafletProxy("habitat_map") %>%
      clearShapes() %>%
      clearControls()
    
    species_code <- names(species_labels)[species_labels == input$selected_species]
    selected_stages <- input$selected_stages
    
    for (stage in lifestages) {
      label <- lifestage_labels[[stage]]
      stage_data <- rds_files[[species_code]][[stage]]
      
      if (label %in% selected_stages && !is.null(stage_data)) {
        leafletProxy("habitat_map") %>%
          addPolygons(
            data = stage_data,
            color = stage_colors[[stage]],
            weight = 2,
            opacity = 1,
            fillOpacity = 0.8,
            group = label,
            label = label
          )
      }
    }
    
    # Add legend
    visible_stages <- lifestages[
      sapply(rds_files[[species_code]], Negate(is.null)) & 
        lifestage_labels %in% selected_stages
    ]
    
    if (length(visible_stages) > 0) {
      leafletProxy("habitat_map") %>%
        addLegend(
          position = "topright",
          colors = stage_colors[visible_stages],
          labels = lifestage_labels[visible_stages],
          title = "Life Stage"
        )
    }
  })
  
  # Show Layer Info Modal ##comment out to see if this helps run the app better
 observeEvent(input$layer_info, {
    req(input$selected_species)
    
    species_code <- names(species_labels)[species_labels == input$selected_species]
    
    info_filtered <- polygon_layer_data %>%
      filter(species == species_code, lifestage %in% lifestages) %>%
      select(lifestage, shapefile_name) %>%
      distinct()
    
    info_html <- paste(
      sapply(names(lifestage_labels), function(ls) {
        shp_names <- info_filtered$shapefile_name[info_filtered$lifestage == ls]
        if (length(shp_names) > 0) {
          paste0("<b>", lifestage_labels[[ls]], ":</b> ", paste(shp_names, collapse = ", "))
        } else {
          NULL
        }
      }),
      collapse = "<br><br>"
    )
    
    showModal(modalDialog(
      title = paste("Layer Descriptions for", input$selected_species),
      tagList(
        tags$p("EFH polygons are visualized by species and life stage."),
        tags$p("Below are the habitat attributes associated with each life stage broken out by eco-region and coastal zone:"),
        HTML(info_html)      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)

## sometimes glitch when loading the map 
