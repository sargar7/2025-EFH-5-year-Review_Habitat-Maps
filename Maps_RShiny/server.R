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
  
  # Render selected habitat types
  observe({
    selected_habitats <- input$selected_habitat
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Type") %>%
      removeControl(layerId = "habitat_legend")
    
    if (length(selected_habitats) > 0) {
      for (habitat_code in selected_habitats) {
        habitat_data <- habitat_sf[[habitat_code]]
        habitat_name <- names(habitat_choices)[habitat_choices == habitat_code][1]
        habitat_color <- as.character(habitat_palette[habitat_name])
        if (!is.null(habitat_data)) {
          leafletProxy("habitat_map") %>%
            addPolygons(
              data = habitat_data,
              color = habitat_color,
              weight = 0,
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
  
  ### Render species/lifestage polygons
  
  ### # server.r - Species/lifestage polygons with "None" option
  observe({
    species_code <- input$selected_species
    selected_stages <- input$selected_stages
    
    # Clear only species/lifestage polygons and legend
    leafletProxy("habitat_map") %>%
      clearGroup("Life Stage") %>%
      removeControl(layerId = "lifestage_legend")
    
    if (is.null(species_code) || species_code == "") {  ### # CHANGED: None selected
      # If "None" is selected, reset life stages selection to empty
      updateCheckboxGroupInput(session, "selected_stages", selected = character(0))  ### # CHANGED
      return()  # exit early, no polygons rendered
    }
    
    if (!species_code %in% names(rds_files)) return()
    
    # If a real species is selected, render polygons
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
    # If "None" is selected, nothing is drawn (polygons cleared above)
  })
  
  # Render habitat zones
  observe({
    selected_zones <- input$selected_zone
    zone_order <- c("Estuarine", "Nearshore", "Offshore")
    ordered_zones <- zone_order[zone_order %in% selected_zones]
    
    leafletProxy("habitat_map") %>%
      clearGroup("Habitat Zone") %>%
      removeControl(layerId = "zone_legend")
    
    if (length(ordered_zones) > 0) {
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
  
  # EFH Descriptions Modal
  observeEvent(input$efh_descriptions, {
    species_code <- input$selected_species
    
    info_html <- if (species_code != "") {
      info_filtered <- polygon_layer_data %>%
        filter(species == species_code, lifestage %in% lifestages) %>%
        select(lifestage, habitatzone, habitattype, ecoregion) %>%
        distinct()
      
      paste(
        sapply(lifestages, function(ls) {
          df_stage <- info_filtered %>% filter(lifestage == ls)
          if (nrow(df_stage) == 0) return(paste0("<b>", lifestage_labels[[ls]], ":</b> No information is available"))
          df_stage <- df_stage %>%
            mutate(
              habitat_pretty = recode(tolower(habitattype), !!!habitat_map, .default = "No information is available"),
              zone_pretty    = recode(tolower(habitatzone), !!!zone_map, .default = "No information is available"),
              er_pretty      = recode(tolower(ecoregion), !!!er_map, .default = "No information is available")
            )
          simplified <- df_stage %>%
            group_by(zone_pretty, habitat_pretty) %>%
            summarise(
              er_range = if (n_distinct(er_pretty) > 1) paste0(min(er_pretty), "–", max(er_pretty)) else unique(er_pretty),
              .groups = "drop"
            )
          paste0("<b>", lifestage_labels[[ls]], ":</b> ", paste(simplified$zone_pretty, simplified$habitat_pretty, simplified$er_range, collapse = ", "))
        }),
        collapse = "<br><br>"
      )
    } else {
      "<b>No species selected.</b>"
    }
    
    showModal(modalDialog(
      title = paste("Habitat Layer Descriptions for", ifelse(species_code == "", "None", species_code)),
      tagList(
        tags$p("EFH polygons are visualized by species and life stage."),
        tags$p("Below are the habitat attributes associated with each life stage broken out by eco-region and coastal zone:"),
        HTML(info_html),
        tags$hr(),
        tags$p(
          "Updated Metadata are provided through Council contracted work completed in 2023/2024, available ",
          tags$a(href = "https://drive.google.com/drive/folders/1qx9lop8Wgq2YAcrRIYJ-kR-YH9KWSdtF?usp=sharing", target = "_blank", "here"),
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

  