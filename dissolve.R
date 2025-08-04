
# Load required libraries
library(sf)
library(leaflet)
library(dplyr)
library(tools)


# Read adult Almacojack EFH shapefile
#almaco_adult <- st_read(
  "C:/Users/johnf.GMFMC/Gulf of Mexico Fishery Mgmt Council/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps/almacojack/almacojack_adult.shp"
#)

# Set working directory
base_dir <- "C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps"
input_dir <- file.path(base_dir, "almacojack")
output_dir <- file.path(base_dir, "RDS_species_maps_dis")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ---- Read Shapefile ----
almaco_adult <- st_read(
"C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps/almacojack/almacojack_adult.shp"
)

# ---- Leaflet Map: Original Polygons ----
p1 <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(
    data = almaco_adult,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.3,
    group = "Original Polygons"
  )

# ---- Dissolve Polygons ----
# 1. Validate geometries to avoid issues during union
# 2. Union all features into one or fewer shapes
# 3. Convert result back to 'sf' object
dissolved <- almaco_adult %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf()

# 4. Keep only valid polygon types (exclude LINESTRINGs, if present)
almaco_adult_dissolve <- dissolved[
  sf::st_geometry_type(dissolved) %in% c("POLYGON", "MULTIPOLYGON"),
]

# ---- Leaflet Map: Dissolved Polygons ----
p2 <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(
    data = almaco_adult_dissolve,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.3,
    group = "Dissolved Polygons"
  )

# ---- Export dissolved shapefile ----
# Save the cleaned and dissolved polygons to a new shapefile
output_file <- file.path(output_dir, "almaco_adult_dissolve.shp")
st_write(almaco_adult_dissolve, output_file)


################## loop through all almacojack shp files to dissolve ################

# List all .shp files in the almacojack folder
shp_files <- list.files(input_dir, pattern = "\\.shp$", full.names = TRUE)
print(shp_files)

# Loop through each shapefile
for (shp_path in shp_files) {
  
  # Extract file name (without extension)
  shp_name <- tools::file_path_sans_ext(basename(shp_path))
  
  # Extract species name from folder (e.g., almacojack from .../almacojack/filename.shp)
  species_name <- basename(dirname(shp_path))
  
  # Read shapefile
  message("Reading: ", shp_name)
  shp_data <- st_read(shp_path, quiet = TRUE)
  
  sf_use_s2(FALSE) ##try to fix s2 error
  
  # Validate geometries and dissolve all polygons
  dissolved <- shp_data %>%
    st_make_valid() %>%
    st_union() %>%
    st_sf()
  
  # Keep only POLYGON and MULTIPOLYGON
  dissolved_clean <- dissolved[
    st_geometry_type(dissolved) %in% c("POLYGON", "MULTIPOLYGON"),
  ]
  
  # Create species-specific output folder
  species_output_dir <- file.path(output_dir, species_name)
  if (!dir.exists(species_output_dir)) {
    dir.create(species_output_dir, recursive = TRUE)
  }
  
  # Create output file path
  # Define .rds output path (same folder, with .rds extension)
  output_rds_file <- file.path(species_output_dir, paste0(shp_name, "_dissolve.rds"))
  
  # Save sf object as .rds
  saveRDS(dissolved_clean, file = output_rds_file)
  
  message("Saved RDS: ", output_rds_file)
}
  
### use readRDS to read back later ###


################# loop through all spp. lifestages to dissolve #################

# Set input and output directories
input_dir <- "C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps"
output_dir <- "C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps/RDS_species_maps_dis"

# List all .shp files recursively
shp_files <- list.files(input_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
print(shp_files)

# Loop through each shapefile
for (shp_path in shp_files) {
  
  # Extract shapefile name without extension
  shp_name <- file_path_sans_ext(basename(shp_path))
  
  # Extract species name from the subfolder (e.g., almacojack)
  species_name <- basename(dirname(shp_path))
  
  # Read shapefile as sf
  message("Reading: ", shp_name)
  shp_data <- st_read(shp_path, quiet = TRUE)
  
  # Disable s2 to avoid geometry errors (optional depending on CRS and geometry)
  sf_use_s2(FALSE)
  
  # Clean geometry and dissolve
  dissolved <- shp_data %>%
    st_make_valid() %>%
    st_union() %>%
    st_sf()
  
  # Keep only polygon or multipolygon
  dissolved_clean <- dissolved[st_geometry_type(dissolved) %in% c("POLYGON", "MULTIPOLYGON"), ]
  
  # Create species output folder if not exists
  species_output_dir <- file.path(output_dir, species_name)
  if (!dir.exists(species_output_dir)) {
    dir.create(species_output_dir, recursive = TRUE)
  }
  
  # Define output .rds file path
  output_rds_file <- file.path(species_output_dir, paste0(shp_name, "_dissolve.rds"))
  
  # Save sf object as .rds
  saveRDS(dissolved_clean, file = output_rds_file)
  
  message("Saved RDS: ", output_rds_file)
}

#### simplify gagearlyjuvenile rds files 

library(sf)
library(rmapshaper)

library(sf)
library(rmapshaper)
library(tools)

# Directories
input_base <- "C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps/RDS_species_maps_dis"
output_base <- file.path(input_base, "RDS_simplified")

# Set size threshold (10,000 KB = 10 * 1024^2 bytes)
size_threshold <- 10 * 1024^2  # 10 MB

# List all .rds files recursively
rds_files <- list.files(input_base, pattern = "_dissolve\\.rds$", recursive = TRUE, full.names = TRUE)

# Loop through each file
for (rds_path in rds_files) {
  # Skip already-simplified files
  if (grepl("RDS_simplified", rds_path)) next
  
  file_size <- file.info(rds_path)$size
  relative_path <- gsub(input_base, "", rds_path)
  output_path <- file.path(output_base, relative_path)
  
  # Ensure output directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  if (file_size > size_threshold) {
    message("🔄 Simplifying: ", basename(rds_path), " [", round(file_size / 1024^2, 1), " MB]")
    shp <- readRDS(rds_path)
    
    simplified <- tryCatch({
      ms_simplify(shp, keep = 0.05, keep_shapes = TRUE)
    }, error = function(e) {
      message("❌ Simplification failed: ", e$message)
      return(NULL)
    })
    
    if (!is.null(simplified)) {
      saveRDS(simplified, output_path)
      message("✅ Saved simplified: ", output_path)
    } else {
      message("⚠️ Skipped: Could not simplify ", basename(rds_path))
    }
    
  } else {
    file.copy(rds_path, output_path, overwrite = TRUE)
    message("📦 Copied (small): ", basename(rds_path), " [", round(file_size / 1024^2, 1), " MB]")
  }
}

