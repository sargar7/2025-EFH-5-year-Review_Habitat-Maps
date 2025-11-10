#server.r

function(input, output, session) {
  
  # Start Tour
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = data.frame(
      element = c("#habitat_box","#zone_box", "#species_box", "#habitat_map", "#efh_descriptions","#artificial_reef"),  
      intro = c(
        "Use the dropdown to select habitat type(s) polygons on the map.", 
        "Use the dropdown to select habitat zone(s) polygons on the map.",
        "Use the dropdown to explore EFH layers for each species.",
        "The map displays EFH polygons by selected life stages or habitat type.", 
        "Click to view habitat type descriptions for each species lifestage",
        "Click to learn more about Artifical Reef Habitat."
      ),
      position = c("bottom","bottom", "bottom", "top", "top", "bottom")
    )))
  })
  
  # Render base map
  output$habitat_map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      leaflet.esri::addEsriBasemapLayer("Imagery") %>%
      fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2) %>%
      addControl(html = "<img src='Logo_color.jpg' style='width:120px; opacity:0.8;'>", position = "bottomleft") %>%
      addLabelOnlyMarkers(
        lng = -90, lat = 25,
        label = "Gulf of America",
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                    style = list("color" = "white", "font-size" = "12px", "font-style" = "italic"))
      ) %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = TRUE, metric = FALSE, updateWhenIdle = TRUE))
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
      easyClose = TRUE, footer = NULL
    ))
  })
  
  ### Render selected habitat types
  observe({
    selected_habitats <- input$selected_habitat  ### CHANGED: pickerInput returns NULL if nothing selected
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Type") %>%
      removeControl(layerId = "habitat_legend")
    
    if (!is.null(selected_habitats) && length(selected_habitats) > 0) {  ### CHANGED
      for (habitat_code in selected_habitats) {
        habitat_data <- habitat_sf[[habitat_code]]
        habitat_name <- names(habitat_choices)[habitat_choices == habitat_code][1]
        habitat_color <- as.character(habitat_palette[habitat_name])
        if (!is.null(habitat_data)) {
          leafletProxy("habitat_map") %>%
            addPolygons(
              data = habitat_data,
              color = habitat_color,
              weight = 1,
              opacity = 1,
              fillOpacity = 1,
              fillColor = habitat_color,
              group = "Habitat Type",
              label = habitat_name
            )
        }
      }
      
      legend_labels <- sapply(selected_habitats, function(code) names(habitat_choices)[habitat_choices == code][1])
      legend_colors <- as.character(habitat_palette[legend_labels])
      
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
  
  ### Render habitat zones
  observe({
    selected_zones <- input$selected_zone  ### CHANGED: pickerInput returns NULL if nothing selected
    zone_order <- c("Estuarine", "Nearshore", "Offshore")
    ordered_zones <- zone_order[zone_order %in% selected_zones]
    
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Zone") %>%
      removeControl(layerId = "zone_legend")
    
    if (!is.null(selected_zones) && length(ordered_zones) > 0) {  ### CHANGED
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
  
  ### Render species/lifestage polygons
  observe({
    species_code <- input$selected_species  ### CHANGED: pickerInput returns "" if cleared
    selected_stages <- input$selected_stages
    
    leafletProxy("habitat_map") %>%
      clearGroup("Life Stage") %>%
      removeControl(layerId = "lifestage_legend")
    
    if (is.null(species_code) || species_code == "") {  ### CHANGED: handle cleared selection
      updateCheckboxGroupInput(session, "selected_stages", selected = character(0))
      return()
    }
    
    if (!species_code %in% names(rds_files)) return()
    
    for (stage in lifestages) {
      label <- lifestage_labels[[stage]]
      stage_data <- rds_files[[species_code]][[stage]]
      
      if (label %in% selected_stages && !is.null(stage_data)) {
        leafletProxy("habitat_map") %>%
          addPolygons(
            data = stage_data,
            color = stage_colors[[stage]],
            weight = 0,
            opacity = 1,
            fillOpacity = 0.8,
            group = "Life Stage",
            label = label
          )
      }
    }
    
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
          layerId = "lifestage_legend"
        )
    }
  })
  
  # EFH Descriptions Modal (with italics from no_spatial column)
  observeEvent(input$efh_descriptions, {
    species_code <- input$selected_species
    
    info_html <- if (species_code != "") {
      
      # Normalize species for matching
      selected_species <- tolower(species_code)
      
      # Filter descriptors for this species
      species_desc <- efh_descriptors %>%
        filter(species == selected_species)
      
      # Build block per life stage
      blocks <- lapply(lifestages, function(ls) {
        desc_row <- species_desc %>% filter(lifestage == tolower(ls))
        
        # Main EFH text
        main_text <- if (nrow(desc_row) > 0 && !is.na(desc_row$efh) && trimws(desc_row$efh) != "") {
          desc_row$efh
        } else {
          "No descriptor provided."
        }
        
        # No spatial text (italicized)
        italic_text <- if (nrow(desc_row) > 0 && !is.na(desc_row$no_spatial) && trimws(desc_row$no_spatial) != "") {
          paste0(" <i>", desc_row$no_spatial, "</i>")
        } else {
          ""
        }
        
        # Combine main and italic text
        paste0("<b>", lifestage_labels[[ls]], ":</b> ", main_text, italic_text)
      })
      
      # Combine all life stages with spacing
      paste(blocks, collapse = "<br><br>")
      
    } else {
      "<b>No species selected.</b>"
    }
    
    # Show modal
    showModal(modalDialog(
      title = paste("Habitat Layer Descriptions for", ifelse(species_code == "", "None", species_code)),
      tagList(
        tags$p("Below are the habitat types associated with each life stage:"),
        HTML(info_html),  # preserves HTML/italics
        tags$hr(),
        tags$p(
          "Updated Metadata are provided through Council contracted work completed in 2023/2024, available ",
          tags$a(
            href = "https://drive.google.com/drive/folders/1qx9lop8Wgq2YAcrRIYJ-kR-YH9KWSdtF?usp=sharing", 
            target = "_blank", "here"
          ),
          "."
        )
      ),
      easyClose = TRUE, footer = NULL
    ))
  })
  
 
  # Artificial Reef Modal
  observeEvent(input$artificial_reef, {
    showModal(modalDialog(
      title = "Artificial Reefs",
      tagList(
        tags$p(
          "Artificial reefs are human-made structures placed on the seafloor to mimic natural reef habitats. 
In the Gulf, two types of artificial reefs are recognized: 
1) structures intentionally placed as artificial reefs, and 
2) structures such as oil and gas platforms that are intended for other purposes but do provide fish habitat. 

Petroleum platforms have been in place since the 1940s. A variety of other structures in the Gulf also serve as artificial reefs, 
including pipelines and sunken vessels. They can enhance local fisheries by aggregating fish and supporting biodiversity 
in areas where natural reefs are sparse."
        ),
        tags$p(
          "Gulf Council and EFH Considerations: The Gulf Council currently does not manage artificial reefs as Essential Fish Habitat (EFH). 
Fixed petroleum platforms and other artificial reef substrates were considered for inclusion as EFH in 2013. 
At that time, the Council chose not to modify current essential fish habitat type designations."
        ),
        tags$p(
          "2013 Council Evaluation of Artificial Reef Draft Options Document: ",
          tags$a(
            href = "https://gulf-council-media.s3.amazonaws.com/uploads/2025/03/K-5-Artificial-Reefs-as-EFH-Amendment-6-3-2013.pdf",
            target = "_blank",
            "View Document"
          ),
          "."
        ),
        tags$p(
          "Currently, individual states manage their own artificial reef databases, which can be accessed through their websites."
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

  