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
  
  # EFH Descriptions Modal
  observeEvent(input$efh_descriptions, {
    species_code <- input$selected_species
    
    info_html <- if (species_code != "") {
      # Filter polygon_layer_data for selected species and all life stages ### CHANGED
      info_filtered <- polygon_layer_data %>%
        filter(species == species_code, lifestage %in% lifestages) %>%
        select(lifestage, habitatzone, habitattype, ecoregion, shapefile_name) %>%  ### CHANGED: include shapefile_name if you want to display
        distinct()
      
      # Helper function to format attributes cleanly
      format_attributes <- function(habitat_type, habitat_zone, ecoregion) {
        attrs <- c(
          ifelse(is.na(habitat_type) | habitat_type == "", NA, habitat_type),
          ifelse(is.na(habitat_zone)  | habitat_zone == "", NA, habitat_zone),
          ifelse(is.na(ecoregion)     | ecoregion == "", NA, ecoregion)
        )
        if (all(is.na(attrs))) return("No spatial data available")
        paste(na.omit(attrs), collapse = " ")  # space-separated for clean display ### CHANGED
      }
      
      # Build EFH description per life stage
      if (nrow(info_filtered) == 0) {
        "<b>No spatial data available.</b>"  ### # CHANGED: if no data at all
      } else {
        paste(
          sapply(lifestages, function(ls) {
            df_stage <- info_filtered %>% filter(lifestage == ls)
            
            if (nrow(df_stage) == 0) {
              return(paste0("<b>", lifestage_labels[[ls]], ":</b> No spatial data available"))
            }
            
            # Recode attributes to pretty names ### CHANGED
            df_stage <- df_stage %>%
              mutate(
                habitat_pretty = recode(tolower(habitattype), !!!habitat_map, .default = ""),
                zone_pretty    = recode(tolower(habitatzone), !!!zone_map, .default = ""),
                er_pretty      = recode(tolower(ecoregion), !!!er_map, .default = "")
              )
            
            simplified <- df_stage %>%
              group_by(zone_pretty, habitat_pretty) %>%
              summarise(
                er_range = if (n_distinct(er_pretty) > 1) paste0(min(er_pretty), "–", max(er_pretty)) else unique(er_pretty),
                .groups = "drop"
              )
            
            # Format each row with helper function to avoid repeated "No information" messages
            stage_text <- sapply(seq_len(nrow(simplified)), function(i) {
              format_attributes(
                simplified$habitat_pretty[i],
                simplified$zone_pretty[i],
                simplified$er_range[i]
              )
            })
            
            paste0("<b>", lifestage_labels[[ls]], ":</b> ", paste(stage_text, collapse = "; "))
          }),
          collapse = "<br><br>"
        )
      }
      
    } else {
      "<b>No species selected.</b>"
    }
    
    ### Show modal with cleaned EFH descriptions
    showModal(modalDialog(
      title = paste("Habitat Layer Descriptions for", ifelse(species_code == "", "None", species_code)),
      tagList(
        tags$p("EFH polygons are visualized by species and life stage."),
        tags$p("Below are the habitat attributes associated with each life stage broken out by eco-region and coastal zone:"),
        HTML(info_html),
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

  