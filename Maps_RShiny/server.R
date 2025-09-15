#server.r

function(input, output, session) {
  # Start Tour
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = data.frame(
      element = c("#habitat_box","#zone_box", "#species_box", "#habitat_map", "#layer_info"),  
      intro = c(
        "Use the dropdown to select habitat type(s) polygons on the map.", 
        "Use the dropdown to select habitat zone(s) polygons on the map.",
        "Use the dropdown to explore EFH layers for each species.",
        "The map displays EFH polygons by selected life stages or habitat type.", 
        "Click 'Layer Info' to view descriptions of each map layer."
      ),
      position = c("bottom","bottom", "bottom", "top", "top")  # positions for tooltips
    )))
  })
  
  # Render the map with Esri basemap
  output$habitat_map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%  # disable attribution bar
      leaflet.esri::addEsriBasemapLayer("Imagery") %>%
      fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2) %>%
      addControl(
        html = "<img src='Logo_color.jpg' style='width:120px; opacity:0.8;'>",
        position = "bottomleft"
      )
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
  
  ### Render selected habitat types (dropdown)
  observe({
    selected_habitats <- input$selected_habitat  # may be multiple
    
    # Clear previous Habitat Type layer and legend
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Type") %>%
      removeControl(layerId = "habitat_legend")
    
    if (length(selected_habitats) > 0) {
      # Loop through selected habitats and add polygons
      for (habitat_code in selected_habitats) {
        
        habitat_data <- habitat_sf[[habitat_code]]
        
        # Map code to pretty name
        habitat_name <- names(habitat_choices)[habitat_choices == habitat_code][1]
        
        # Look up color as a plain string, not a named vector
        habitat_color <- as.character(habitat_palette[habitat_name])
        
        if (!is.null(habitat_data)) {
          leafletProxy("habitat_map") %>%
            addPolygons(
              data = habitat_data,
              color = habitat_color,      # stroke color
              weight = 2,
              opacity = 1,
              fillOpacity = 0.5,
              fillColor = habitat_color,  # fill color must match stroke
              group = "Habitat Type",
              label = habitat_name
            )
        }
      }
      
      # Add legend in the same order as selected
      legend_labels <- sapply(selected_habitats, function(code) names(habitat_choices)[habitat_choices == code][1])
      legend_colors <- as.character(habitat_palette[legend_labels])  # ensure plain vector
      
      leafletProxy("habitat_map") %>%
        addLegend(
          position = "topright",
          colors = legend_colors,
          labels = legend_labels,
          title = "Habitat Type",
          layerId = "habitat_legend"
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
    
    species_name <- names(species_lookup)[species_lookup == species_code]
    
    if (length(visible_stages) > 0) {
      leafletProxy("habitat_map") %>%
        addLegend(
          position = "topright",
          colors = stage_colors[visible_stages],
          labels = lifestage_labels[visible_stages],
          title = species_name,
          layerId = "lifestage_legend"   # *** ensures legend can be cleared independently
        )
    }
  })
  

  ### Render selected habitat zone (dropdown or checkboxes)
  observe({
    selected_zones <- input$selected_zone  # may be multiple
    
    #define fixed order for legend
    zone_order <- c("Estuarine", "Nearshore", "Offshore")
    
    #reorder selected zones according to fixed order
    ordered_zones <- zone_order[zone_order %in% selected_zones]
    
    # Clear previous Habitat Zone layer
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Zone") %>%
      removeControl(layerId = "zone_legend")
    
    if (length(ordered_zones) > 0) {
      # NEW: loop through zones instead of assuming a single one
      for (zone in ordered_zones) {
        zone_key <- tolower(zone)
        zone_data <- zone_sf[[zone_key]]
        
        if (!is.null(zone_data)) {
          leafletProxy("habitat_map") %>%
            addPolylines(
              data = st_boundary(zone_data),
              color = zone_colors[[zone_key]],
              weight = 2,
              opacity = 1,
              group = "Habitat Zone",
              label = zone
            )
        }
      }
      
      leafletProxy("habitat_map") %>%
        addLegend(
          position = "topright",
          colors = unname(zone_colors[tolower(ordered_zones)]),
          labels = ordered_zones,
          title = "Habitat Zone",
          layerId = "zone_legend"
        )
    }
  })
 
  # Show Layer Info Modal with simplified ER ranges
  observeEvent(input$layer_info, {
    req(input$selected_species)
    
    species_code <- input$selected_species
    
    info_filtered <- polygon_layer_data %>%
      filter(species == species_code, lifestage %in% lifestages) %>%
      select(lifestage, habitatzone, habitattype, ecoregion) %>%
      distinct()
    
    info_html <- paste(
      sapply(lifestages, function(ls) {
        df_stage <- info_filtered %>% filter(lifestage == ls)
        
        if (nrow(df_stage) == 0) {
          return(paste0("<b>", lifestage_labels[[ls]], ":</b> No information is available"))
        }
        
        df_stage <- df_stage %>%
          mutate(
            habitat_pretty = recode(tolower(habitattype), !!!habitat_map, .default = "No information is available"),
            zone_pretty    = recode(tolower(habitatzone), !!!zone_map, .default = "No information is available"),
            er_pretty      = recode(tolower(ecoregion), !!!er_map, .default = "No information is available")
          )
        
        # If everything came back as "No information..."
        if (all(df_stage$habitat_pretty == "No information is available" &
                df_stage$zone_pretty == "No information is available" &
                df_stage$er_pretty == "No information is available")) {
          return(paste0("<b>", lifestage_labels[[ls]], ":</b> No information is available"))
        }
        
        simplified <- df_stage %>%
          group_by(zone_pretty, habitat_pretty) %>%
          summarise(
            er_range = if (n_distinct(er_pretty) > 1) {
              paste0(min(er_pretty), "–", max(er_pretty))
            } else {
              unique(er_pretty)
            },
            .groups = "drop"
          )
        
        paste0(
          "<b>", lifestage_labels[[ls]], ":</b> ",
          paste(simplified$zone_pretty, simplified$habitat_pretty, simplified$er_range, collapse = ", ")
        )
      }),
      collapse = "<br><br>"
    )
    
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
  