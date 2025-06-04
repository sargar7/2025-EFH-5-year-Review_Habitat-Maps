###EFH 5 year review 
##Species habitat maps 
##Adding habitat type shapefiles to read by R 
##Creating habitat map for each species by lifestage


install.packages("sf")
install.packages("raster")
install.packages("ggmap")
install.packages("stringr")
install.packages("tmap")
install.packages("ggspatial")
install.packages("leaflet")
install.packages("webshot2") #saving .png image of maps 
install.packages("htmlwidgets")
install.packages ("rmapshaper") ##simplify shapfiles but maintains spatial data 
instlall.packages ("RColorBrewer") 
install.packages ("shinyjs")
install.packages("leaflet.esr")

 
 ##############################################################################
 ################# EFH 5 YEAR REVIEW SPECIES HABITAT MAPS 2025 ###############
 
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
 library(RColorBrewer)
 library(shiny)
library(leaflet.esri)

 
 ###### DIRECTORIES ######
 
 getwd()
 
 gpkg_dir <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/2025 GIS Clipped Habitat/Converted_GPKG"
 dir.create(gpkg_dir, showWarnings = FALSE)
 
 # List all .gpkg files in the directory
 gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = TRUE)
 View(gpkg_files)
 
 
 
 # Define base output directory for saving all files
 # Local and do not get pushed to Github
 output_dir <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/Maps_Output"
 
 # Set output dir for .gpkg
 gpkg_output_dir <- file.path(output_dir, "gpkg_output")
 dir.create(gpkg_output_dir, showWarnings = FALSE)
 
 # Output base directory for species-specific shapefiles
 shp_output_base <- "C:/Users/Sarah/OneDrive - GOM/Desktop/Generic AM 5 GIS files/Maps_Output/SHP_species_maps"
 
 # Create base output dir if it doesn't exist
 dir.create(shp_output_base, recursive = TRUE, showWarnings = FALSE)
 
 
 ######################## LOAD .GPKG HABITAT FILES ##############################
 
 # Initialize an empty list to store the shapefiles from the .gpkg files
 shapefile_list <- list()
 
 ##### Loop through the .gpkg files and read the layers #####
 
 for (file in gpkg_files) {
   # Get the available layers in the .gpkg file
   layers <- st_layers(file)$name
   
   # Loop through each layer and read it into the shapefile list
   for (layer in layers) {
     # Correct the naming convention to match "habitattype_habitatzone_ecoregion"
     # Extract the correct file name (i.e., the first part of the layer name)
     # assuming the layer name is not in the format you expect.
     # For example, layer_name could be "wca_off_er1" or something else.
     
     # Use only the part of the layer name that corresponds to your naming convention
     layer_name_parts <- strsplit(layer, "_")[[1]]
     correct_name <- paste(tolower(layer_name_parts[1]), 
                           tolower(layer_name_parts[2]), 
                           tolower(layer_name_parts[3]), 
                           sep = "_")
     
     # Store the shapefile in the list with the corrected name
     shapefile_list[[correct_name]] <- st_read(file, layer = layer, quiet = TRUE)
   }
 }
 
 names(shapefile_list) <- tolower(names(shapefile_list)) #double check all are lower
 
 
 ######################### .gpkg summary ####################################
 
 # Create a summary table of key info
 shapefile_summary <- lapply(names(shapefile_list), function(name) {
   shp <- shapefile_list[[name]]
   gpkg_path <- file.path(gpkg_dir, paste0(name, ".gpkg"))
   
   list(
     name = name,
     crs = st_crs(shp)$epsg,
     features = nrow(shp),
     file_size_mb = if (file.exists(gpkg_path)) file.info(gpkg_path)$size / 1024^2 else NA,
     geometry_type = unique(st_geometry_type(shp))
   )
 }) %>% bind_rows()
 
 # View in Viewer pane
 View(shapefile_summary)
 
 
 ##### test plot with .gpkg files #####
 
 # Extract the layers
 wca_layer <- shapefile_list[["wca_off_er1"]]
 hb_layer <- shapefile_list[["hb_est_er2"]]
 em_est_er1 <-shapefile_list[["em_est_er1"]]
 
 str(wca_layer)
 str(hb_layer)
 str(em_est_er1)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data = wca_layer, 
               fillColor = "red", 
               fillOpacity = 0.5, 
               color = "red", 
               weight = 1,
               group = "WCA OFF ER1",
               label = ~paste("WCA")) %>%
   addPolygons(data = hb_layer, 
               fillColor = "green", 
               fillOpacity = 0.5, 
               color = "green", 
               weight = 1,
               group = "HB EST ER2",
               label = ~paste("HB")) %>%
   addPolygons(data = em_est_er1, 
               fillColor = "purple", 
               fillOpacity = 0.5, 
               color = "purple", 
               weight = 1,
               group = "EM EST ER1",
               label = ~paste("EM EST ER1")) %>%
   addLayersControl(
     overlayGroups = c("WCA OFF ER1", "HB EST ER2", "EM EST ER1"),
     options = layersControlOptions(collapsed = FALSE)
   )
 
 ############### LOAD SPECIES_HABITAT CSV FILE AND FORMAT ####################
 
 ################ Species Habitat Tables- Full dataset ###################
 
 species_habitat<- read.csv("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/species_habitatattributes.csv")
 View(species_habitat)
 
 ##separate values that are followed by commas into separate row
 species_habitat_long <-species_habitat %>%
   separate_rows(habitatzone, sep = ",\\s*") %>%
   separate_rows(habitattype, sep = ",\\s*") %>%
   separate_rows(ecoregion, sep = ",\\s*")
 
 
 ##clean up, make all lowercase and remove any additional spaces
 species_habitat_clean <- species_habitat_long %>%
   mutate(
     habitattype = str_trim(str_to_lower(habitattype)), #EM, HB,mangrove, oyster, reef, sand, SAV, shelf, SB, WCA
     habitatzone = str_trim(str_to_lower(habitatzone)), #est, near, off
     species = str_trim(str_to_lower(species)),
     lifestage = str_trim(str_to_lower(lifestage)),
     ecoregion = ifelse(ecoregion != "" & !is.na(ecoregion), paste0("er", ecoregion), "")
   ) %>%
   mutate(
     shapefile_name = apply(select(., habitattype, habitatzone, ecoregion), 1, function(row) {
       paste(row[row != "" & !is.na(row)], collapse = "_")
     })
   ) ##no shapefile_name for those species lifestage that do not have data to inform
 View(species_habitat_clean)
 
 #################### QA/QC: Identify missing shapefiles ######################
 
 available_shapes <- names(shapefile_list)
 needed_shapes <- unique(species_habitat_clean$shapefile_name)
 missing_shapes <- setdiff(needed_shapes, available_shapes)
 
 # Create a table of records that rely on missing shapefiles
 missing_records <- species_habitat_clean %>%
   filter(shapefile_name %in% missing_shapes)
 
 View(missing_records)
 
 # Save for review
 write.csv(missing_records, file.path(output_dir, "missing_shapefiles_QAQC.csv"), row.names = FALSE)
 
 # Message for visibility
 message(paste(length(missing_shapes), "missing shapefiles detected. Details saved to 'missing_shapefiles_QAQC.csv'."))
 
 ## 46 missing shapefiles 
 ## not an 'error' but no data exists to identify those habitat spatial files 
 ## ex: sand_off, reef_off, etc. 
 
 ################## MAP CREATION WITH LOOP ##########################
 
 ########## skip missing shapefiles #################
 # Filter out records with missing shapefiles so they aren't used in the map loop
 species_habitat_clean <- species_habitat_clean %>%
   filter(!shapefile_name %in% missing_shapes)
 
 
 #create empty log for mapping success/failure 
 log_results <- data.frame(
   species = character(),
   lifestage = character(),
   status = character(),
   missing_files = character(),
   used_files = character(),
   stringsAsFactors = FALSE
 ) 
 
 # Get all species
 species_list <- unique(species_habitat_clean$species)
 View(species_list)
 
 # Optional: Chunk size for partial runs
 chunk_size <- 5  # Set to desired number of species per chunk, or NULL to process all at once
 chunk_indices <- split(species_list, ceiling(seq_along(species_list) / chunk_size))
 
 # Assign colorblind-safe colors for lifestages
 lifestages <- unique(species_habitat_clean$lifestage)
 stage_colors <- setNames(brewer.pal(length(lifestages), "Set2"), lifestages)
 
 #Lifestage naming 
 lifestage_labels <- c(
   egg = "Egg",
   larvae = "Larvae",
   postlarvae = "Post Larvae",
   earlyjuvenile = "Early Juvenile",
   latejuvenile = "Late Juvenile",
   adult = "Adult",
   spawningadult = "Spawning Adult"
 )
 
 View(species_habitat_clean)
 print(unique(species_habitat_clean$species))
 print(species_list)
 print(chunk_indices)
 
 ########################### HTML MAP PRODUCTION ###############################
 # Loop by species
 for (chunk in chunk_indices) {
   tryCatch({
     for (sp in chunk) {
       
       # Filter data for species
       species_group <- species_habitat_clean %>% filter(species == sp)
       
       # Create folder for species inside Maps_output
       species_safe <- gsub("[^[:alnum:]_]", "_", sp)
       species_dir <- file.path(output_dir, species_safe)
       dir.create(species_dir, showWarnings = FALSE, recursive = TRUE)
       
       for (stage in lifestages) {
         stage_group <- species_group %>%
           filter(lifestage == stage)
         
         
         shapes <- unique(stage_group$shapefile_name)
         stage_label <- lifestage_labels[[stage]]
         if (is.null(stage_label)) stage_label <- str_to_title(stage)
         color <- stage_colors[[stage]]
         title_text <- paste(str_to_title(sp), "-", str_to_title(stage), "EFH")
         safe_id <- gsub("[^[:alnum:]_]", "_", paste(sp, stage, sep = "_"))
         html_file <- file.path(species_dir, paste0("map_", safe_id, ".html"))
         
         # If no data, generate blank map and log
         if (nrow(stage_group) == 0) {
           blank_map <- leaflet() %>%
             addProviderTiles("Esri.WorldImagery") %>%
             setView(lng = -89, lat = 25, zoom = 5) %>%
             addLabelOnlyMarkers(
               lng = -98, lat = 31,
               label = title_text,
               labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                           textsize = "20px", fontWeight = "bold", opacity = 1)
             )
           
           saveWidget(blank_map, file = html_file, selfcontained = FALSE)
           message(paste("Blank map saved:", html_file))
           
           log_results <- rbind(log_results, data.frame(
             species = sp,
             lifestage = stage,
             status = "No data (blank map)",
             missing_files = NA,
             used_files = NA
           ))
           next
         }
         
         # Map generation
         efh_map <- leaflet() %>%
           addProviderTiles("Esri.WorldImagery") %>%
           setView(lng = -89, lat = 25, zoom = 5) %>%
           addLabelOnlyMarkers(
             lng = -98, lat = 31,
             label = paste(str_to_title(sp), "-", stage_label, "EFH"),
             labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                         textsize = "20px", fontWeight = "bold", opacity = 1)
           )
         
         shape_failed <- FALSE
         missing_shape_files <- character()
         used_shape_files <- character()
         
         for (shape_name in shapes) {
           gpkg_file <- file.path(gpkg_dir, paste0(shape_name, ".gpkg"))
           used_shape_files <- c(used_shape_files, basename(gpkg_file))
           
           if (file.exists(gpkg_file)) {
             try({
               shp <- st_read(gpkg_file, quiet = TRUE)
               efh_map <- efh_map %>%
                 addPolygons(data = shp,
                             fillColor = color,
                             fillOpacity = 1,
                             color = color,
                             weight = 1,
                             group = shape_name,
                             label = shape_name)
               rm(shp); gc()
             }, silent = TRUE)
           } else {
             warning(paste("Missing GPKG:", gpkg_file))
             shape_failed <- TRUE
             missing_shape_files <- c(missing_shape_files, basename(gpkg_file))
           }
         }
         
         # Save map & log
         tryCatch({
           saveWidget(efh_map, file = html_file, selfcontained = TRUE)
           message(paste("Saved:", html_file))
           status <- if (shape_failed) "Saved with missing files" else "Success"
           log_results <- rbind(log_results, data.frame(
             species = sp,
             lifestage = stage,
             status = status,
             missing_files = if (length(missing_shape_files) > 0) paste(missing_shape_files, collapse = "; ") else NA,
             used_files = if (length(used_shape_files) > 0) paste(used_shape_files, collapse = "; ") else NA
           ))
         }, error = function(e) {
           log_results <- rbind(log_results, data.frame(
             species = sp,
             lifestage = stage,
             status = paste("Save error:", conditionMessage(e)),
             missing_files = if (length(missing_shape_files) > 0) paste(missing_shape_files, collapse = "; ") else NA,
             used_files = if (length(used_shape_files) > 0) paste(used_shape_files, collapse = "; ") else NA
           ))
         })
         
         # Clean memory per stage
         rm(efh_map, stage_group, shapes, html_file, color, safe_id, title_text,
            shape_failed, missing_shape_files, used_shape_files); gc()
       }
       
       # Clean memory per species
       rm(species_group, species_safe, species_dir); gc()
     }
   }, error = function(e) {
     message("Chunk failed:", conditionMessage(e))
   })
 }
 
 # Save the log
 write.csv(log_results, file = file.path(output_dir, "map_generation_log.csv"), row.names = FALSE)
  
 
 ### 050825 notes ####
## 46 'missing' shapefiles- no data to inform these layers 
## not an error per se, but area for developed research- highlight in research priorities 
 

 
 ####################################### SHP MAPS #####################################
 #####052925
 ##### create shapefiles per lifestage to give to basher to tile #######
 ##### tileing creates a url for each species lifestage map and can put that into an r shiny app
 
 
 ######################### ALL SPECIES SHP MAPS ############################

 # Get all unique species and lifestage combinations from your full dataset
 species_lifestage_combos <- species_habitat_clean %>%
   distinct(species, lifestage)
 
 # Function to process and export each lifestage shapefile per species
 export_lifestage_shp <- function(species_name, stage) {
   shape_names <- species_habitat_clean %>%
     filter(species == species_name, lifestage == stage) %>%
     pull(shapefile_name) %>%
     unique()
   
   combined_list <- list()
   
   for (name in shape_names) {
     gpkg_path <- file.path(gpkg_dir, paste0(name, ".gpkg"))
     if (file.exists(gpkg_path)) {
       shp <- tryCatch(st_read(gpkg_path, quiet = TRUE), error = function(e) NULL)
       if (!is.null(shp) && nrow(shp) > 0) {
         shp$source_file <- name
         combined_list[[length(combined_list) + 1]] <- shp
       } else {
         message("Skipped empty or unreadable file: ", name)
       }
     }
   }
   
   # Create species-specific output folder
   species_dir <- file.path(shp_output_base, species_name)
   dir.create(species_dir, recursive = TRUE, showWarnings = FALSE)
   
   # Define shapefile path
   shp_path <- file.path(species_dir, paste0(species_name, "_", stage, ".shp"))
   
   if (length(combined_list) > 0) {
     # Harmonize columns before combining
     common_cols <- Reduce(intersect, lapply(combined_list, names))
     combined_clean <- lapply(combined_list, function(x) x[, common_cols])
     combined_sf <- do.call(rbind, combined_clean)
   } else {
     # Create an empty sf object with valid geometry column and CRS
     combined_sf <- st_sf(
       data.frame(source_file = character()),
       geometry = st_sfc(crs = 4326)
     )
     message("No data found. Creating empty shapefile for ", species_name, " - ", stage)
   }
   
   # Write the shapefile (empty or not)
   st_write(combined_sf, shp_path, delete_layer = TRUE)
   message("Saved shapefile: ", shp_path)
 }
 
 # Loop through all species-lifestage combinations and export
 apply(species_lifestage_combos, 1, function(row) {
   export_lifestage_shp(species_name = row["species"], stage = row["lifestage"])
 })
 
 
######################### 2025 EFH 5 year review RShiny App ####################
 
 
 ##Rshiny using feature layers - URL derived from ArcGIS

 # Load EFH URL data
 URL_dir <- read.csv("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/species_habitatmap_url.csv", stringsAsFactors = FALSE)
 View(URL_dir)
 
 # Life stage codes used in data
 life_stages <- unique(URL_dir$lifestage)
 
 # Nicely formatted labels for legend display
 nice_names <- c(
   egg = "Egg",
   larvae = "Larvae",
   postlarvae = "Post Larvae",
   earlyjuvenile = "Early Juvenile",
   latejuvenile = "Late Juvenile",
   adult = "Adult",
   spawningadult = "Spawning Adult"
 )
 
 # Color palette for each life stage
 palette_colors <- brewer.pal(n = max(3, length(life_stages)), name = "Set1")
 lifestage_colors <- setNames(palette_colors[1:length(life_stages)], life_stages)
 
 # Gulf bounds
 gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)
 
 # UI
 ui <- fluidPage(
   titlePanel("2025 EFH 5 Year Review - Species Habitat Maps"),
   sidebarLayout(
     sidebarPanel(
       selectInput("species", "Select Species", choices = unique(URL_dir$species)),
       checkboxGroupInput("lifestages", "Select Life Stages", choices = life_stages, selected = life_stages)
     ),
     mainPanel(
       leafletOutput("map", height = "600px")
     )
   )
 )
 
 # Server
 server <- function(input, output, session) {
   output$map <- renderLeaflet({
     leaflet() %>%
       addProviderTiles("Esri.WorldImagery") %>%
       fitBounds(gulf_bounds$lng1, gulf_bounds$lat1, gulf_bounds$lng2, gulf_bounds$lat2)
   })
   
   observe({
     req(input$species, input$lifestages)
     
     selected_layers <- URL_dir %>%
       filter(species == input$species, lifestage %in% input$lifestages)
     
     proxy <- leafletProxy("map") %>%
       clearControls()
     
     # Clear only those groups that were previously selected
     for (grp in life_stages) {
       proxy <- proxy %>% clearGroup(grp)
     }
     
     # Add EFH layers for each selected life stage
     for (i in seq_len(nrow(selected_layers))) {
       stage <- selected_layers$lifestage[i]
       color <- unname(lifestage_colors[stage])
       
       proxy %>%
         addEsriFeatureLayer(
           url = selected_layers$url[i],
           group = stage,
           options = featureLayerOptions(
             style = list(
               color = color,
               fillColor = color,
               weight = 1,
               opacity = 1,
               fillOpacity = 0.5
             )
           )
         )
     }
     
     # Custom legend showing only currently selected layers
     legend_html <- paste0(
       "<div style='background:white; padding:10px; border-radius:5px; box-shadow: 2px 2px 6px rgba(0,0,0,0.3);'><strong>Life Stages</strong><br>",
       paste0(
         "<div style='margin-bottom:4px;'><span style='display:inline-block; width:12px; height:12px; background:",
         lifestage_colors[input$lifestages],
         "; margin-right:5px;'></span>",
         nice_names[input$lifestages],  # Use nice labels here only
         "</div>", collapse = ""
       ),
       "</div>"
     )
     
     proxy %>%
       addControl(html = legend_html, position = "topright")
   })
 }
 
 shinyApp(ui, server)
 
 ########## notes 060425 ###########
 # ex .png file and start importing to EFH document for Sept SSC review 
 #currently using feature layers which allow for more dynamic customization AFTER URLs produced 
 # better for on the go edits as Carrie/John view the maps rather than having to recreate in ArcPro before tiling 
 
 ## tiling has better memory useage, but the feature layer apppears to be working - but slower rendering occasionally
 #CHATGPT pros and cons 
 #Capability	Feature Layers (addEsriFeatureLayer)	Tile/Map Layers (addEsriDynamicMapLayer)
 ##                         Feature Layer                                 Tile Layer 
# Toggle individual layers	✅ Yes — each layer can be toggled by group	✅ Yes — but grouped tiles must be published separately
# Layer interactivity	      ✅ Yes — support popups, styling, c          ❌ No — static raster image (no geometry info)
# Custom styling (in R)   	✅ Yes — fill color, border, etc.	          ❌ No — styling must be done before publishing
# Speed/performance       	❌ Slower (downloads full features)	        ✅ Much faster (just image tiles)

 