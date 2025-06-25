#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#### EFH 5 year review App
# Packages 

library(sf)
library(tidyr)
library(dplyr)
library(stringr)
library(ggspatial)
library(leaflet)
library(htmlwidgets) #save html package 
library(webshot2)
library(rmapshaper) # simplify shapefiles
library(viridisLite)
library(RColorBrewer) #lifestage color convention
library(shiny)
library(leaflet.esri)
library(shinydashboard)
library(jsonlite) # try to process tile URLs
library(leaflet.extras)
library(tmap)


# Load EFH URL data
URL_dir <- read.csv("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/species_habitatmap_url.csv", stringsAsFactors = FALSE)
View(URL_dir)

# Life stage codes used in data
life_stages <- unique(URL_dir$lifestage)

# Nicely formatted labels for legend display
lifestage_labels <- c(
  egg = "Egg",
  larvae = "Larvae",
  postlarvae = "Post Larvae",
  earlyjuvenile = "Early Juvenile",
  latejuvenile = "Late Juvenile",
  adult = "Adult",
  spawningadult = "Spawning Adult"
)
lifestages <- names(lifestage_labels)
stage_colors <- setNames(brewer.pal(length(lifestages), "Set2"), lifestages)

# Gulf bounds
gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)

# Define UI for application that displays species-lifestage maps for Gulf Species
# --- UI ---
ui <- fluidPage(
  titlePanel("2025 EFH 5-Year Review - Species Habitat Maps"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species", choices = unique(URL_dir$species)),
      checkboxGroupInput("lifestages", "Select Life Stages",
                         choices = setNames(lifestages, lifestage_labels),
                         selected = lifestages)
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2)
  })
  
  # When species changes, preload all 7 lifestage layers (hidden initially)
  observeEvent(input$species, {
    proxy <- leafletProxy("map")
    
    # Clear old layers and legend
    proxy %>% clearGroup(lifestages) %>% removeControl("legend")
    
    species_layers <- URL_dir %>% filter(species == input$species)
    
    # Add all lifestage layers (hide initially)
    for (i in seq_len(nrow(species_layers))) {
      stage <- species_layers$lifestage[i]
      color <- stage_colors[[stage]]
      
      proxy <- proxy %>%
        addEsriFeatureLayer(
          url = species_layers$url[i],
          group = stage,
          options = featureLayerOptions(
            style = list(
              color = color,
              fillColor = color,
              weight = 1,
              opacity = 1,
              fillOpacity = 0.75
            )
          )
        ) %>%
        hideGroup(stage)
    }
  })
  
  # Toggle visibility based on checkbox selection
  observe({
    req(input$lifestages)
    
    proxy <- leafletProxy("map")
    
    # Hide all groups
    for (stage in lifestages) {
      proxy <- proxy %>% hideGroup(stage)
    }
    
    # Show selected groups
    for (stage in input$lifestages) {
      proxy <- proxy %>% showGroup(stage)
    }
    
    # Update legend
    proxy %>% removeControl("legend")
    
    if (length(input$lifestages) > 0) {
      legend_html <- paste0(
        "<div style='background:white; padding:10px; border-radius:5px; box-shadow:2px 2px 6px rgba(0,0,0,0.3);'><strong>Life Stages</strong><br>",
        paste0(
          "<div style='margin-bottom:4px;'><span style='display:inline-block; width:12px; height:12px; background:",
          stage_colors[input$lifestages],
          "; margin-right:6px;'></span>",
          lifestage_labels[input$lifestages],
          "</div>", collapse = ""
        ),
        "</div>"
      )
      proxy %>% addControl(html = legend_html, position = "topright", layerId = "legend")
    }
  })
}

# --- Run App ---
shinyApp(ui, server)