#server.r

function(input, output, session) {
  # Start Tour
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = data.frame(
      element = c("#habitat_box","#zone_box", "#species_box", "#habitat_map", "#layer_info"),  
      intro = c(
        "Use the dropdown to explore habitat type polygons on the map.", 
        "Use the dropdown to explore habitat zone polygons on the map.",
        "Use the dropdown to explore EFH layers for each species.",
        "The map displays EFH polygons by selected life stages or habitat type.", 
        "Click 'Layer Info' to view descriptions of each map layer."
      ),
      position = c("bottom","bottom", "bottom", "top", "top")  # positions for tooltips
    )))
  })
  
  # Render the map with Esri basemap
  output$habitat_map <- renderLeaflet({
    leaflet() %>%
      
      #basemap
      leaflet.esri::addEsriBasemapLayer("Imagery") %>%
      fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2)
})
  # Habitat Type Info Modal
  observeEvent(input$habitat_info, {
    req(input$selected_habitat)
    
    showModal(modalDialog(
      title = "Habitat Type Info",
      tagList(
        tags$p("Select a habitat type to view the associated polygons on the map."),
        tags$p("These habitat type maps were used to create species-specific EFH maps."),
        tags$p(paste("Currently selected habitat type:", input$selected_habitat))
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Render selected habitat type polygons
  observe({
    selected_code <- input$selected_habitat
    
    # Clear previous Habitat Type layer
    leafletProxy("habitat_map") %>% clearGroup("Habitat Type") %>% removeControl("habitat_legend")
    
    # Only add polygons if a real habitat type is selected
    if (!is.null(selected_code) && selected_code != "None" && selected_code %in% names(habitat_sf)) {
      data <- habitat_sf[[selected_code]]
      
      # Lookup pretty name from your habitat_choices vector
      legend_label <- names(habitat_choices)[habitat_choices == selected_code]  
      
      leafletProxy("habitat_map") %>%
        addPolygons(
          data = data,
          color = "blue",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.5,
          group = "Habitat Type",
          label = legend_label
        ) %>%
        addLegend(
          position = "topright",
          colors = "blue",
          labels = legend_label,
          title = "Habitat Type",
          layerId = "habitat_legend"   #  independent legend for habitat
        )
    }
  })
  
  # Add legend (species/lifestage)
  observe({
    req(input$selected_species)
    
    species_code <- input$selected_species
    selected_stages <- input$selected_stages
    
    # Clear only species/lifestage polygons and legend, leave Habitat Type intact
    leafletProxy("habitat_map") %>%
      clearGroup("Life Stage") %>%
      removeControl(layerId = "lifestage_legend")
    
    # Add species/lifestage polygons
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
            group = "Life Stage",      # group separate from Habitat Type
            label = label
          )
      }
    }
    
    # Add species/lifestage legend
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
          title = "Life Stage",
          layerId = "lifestage_legend"   # *** ensures legend can be cleared independently
        )
    }
  })
  
  ###  Render selected habitat zones
  ###  Render selected habitat zone (dropdown)
  observe({
    selected_zone <- input$selected_zone  # single value
    
    # Clear previous Habitat Zone layer
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Zone") %>%
      removeControl(layerId = "zone_legend")
    
    # Only add layer if a real zone is selected
    if (!is.null(selected_zone) && selected_zone != "None") {
      zone_key <- tolower(selected_zone)
      zone_data <- zone_sf[[zone_key]]
      
      if (!is.null(zone_data)) {
        leafletProxy("habitat_map") %>%
          addPolylines(                  # draw as line
            data = st_boundary(zone_data),
            color = zone_colors[[zone_key]],
            weight = 2,
            opacity = 1,
            group = "Habitat Zone",
            label = selected_zone
          ) %>%
          addLegend(
            position = "topright",
            colors = zone_colors[[zone_key]],
            labels = selected_zone,
            title = "Habitat Zone",
            layerId = "zone_legend"
          )
      }
    }
  })
  
  
  # Show Layer Info Modal with simplified ER ranges
  observeEvent(input$layer_info, {
    req(input$selected_species)
    
    species_code <- input$selected_species
    
    # Filter to selected species and lifestages
    info_filtered <- polygon_layer_data %>%
      filter(species == species_code, lifestage %in% lifestages) %>%
      select(lifestage, habitatzone, habitattype, ecoregion) %>%
      distinct()
    
    # Define ordering for zones
    zone_order <- c("est" = 1, "near" = 2, "off" = 3)
    
    # For each lifestage, generate simplified pretty_names
    info_html <- paste(
      sapply(lifestages, function(ls) {
        df_stage <- info_filtered %>% filter(lifestage == ls)
        
        if(nrow(df_stage) == 0) {
          return(paste0("<b>", lifestage_labels[[ls]], ":</b> No information is available"))
        }
        
        # Map to pretty names and replace NA with message
        df_stage <- df_stage %>%
          mutate(
            habitat_pretty = ifelse(is.na(habitat_map[habitattype]), "No information is available", habitat_map[habitattype]),
            zone_pretty    = ifelse(is.na(zone_map[habitatzone]), "No information is available", zone_map[habitatzone]),
            er_pretty      = ifelse(is.na(er_map[ecoregion]), "No information is available", er_map[ecoregion])
          )
        
        # Group by zone + habitat type and summarize ER ranges
        simplified <- df_stage %>%
          group_by(zone_pretty, habitat_pretty, habitatzone) %>%
          summarise(
            er_range = if (n() > 1) paste0(min(er_pretty), "-", max(er_pretty)) else unique(er_pretty),
            .groups = "drop"
          ) %>%
          arrange(factor(habitatzone, levels = names(zone_order)), habitat_pretty)
        
        # If all entries are "No information is available", show single message
        if(all(simplified$zone_pretty == "No information is available")) {
          return(paste0("<b>", lifestage_labels[[ls]], ":</b> No information is available"))
        }
        
        # Combine into single string per lifestage
        paste0("<b>", lifestage_labels[[ls]], ":</b> ", paste(simplified$zone_pretty, simplified$habitat_pretty, simplified$er_range, collapse = ", "))
      }),
      collapse = "<br><br>"
    )
    
    # Show modal
    showModal(modalDialog(
      title = paste("Layer Descriptions for", input$selected_species),
      tagList(
        tags$p("EFH polygons are visualized by species and life stage."),
        tags$p("Below are the habitat attributes associated with each life stage broken out by eco-region and coastal zone:"),
        HTML(info_html)
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}
  