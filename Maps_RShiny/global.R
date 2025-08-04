# Packages 

library(sf)
library(tidyr)
library(dplyr)
library(leaflet)
library(viridisLite)
library(RColorBrewer) #lifestage color convention
library(shiny)
library(leaflet.esri)
library(shinydashboard)
library(leaflet.extras)
library(shinythemes)
library(terra)
library(shinycssloaders)
library(leafgl)
library(rintrojs)
library(shiny)

## modify directory, but rds files can be found in GOM onedrive
## All gulf Council staff should be able to access and run the app
## need to move polygon layer data csv file 

#Load polygon layer data csv file 
polygon_layer_data <-read.csv("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/species_habitat_clean.csv", stringsAsFactors = FALSE)

#Load EFH RDS data 
rds_base_dir <- "C:/Users/Sarah/GOM/Gulf of Mexico - Documents/EFH/EFH Generic Amendment 5/SHP_species_maps/RDS_simplified"

# Life stages and labels
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

# Get list of species directories
species_dirs <- list.dirs(rds_base_dir, recursive = FALSE, full.names = FALSE)

# Load all RDS files into a nested list: rds_files[[species]][[lifestage]]
rds_files <- setNames(lapply(species_dirs, function(species) {
  setNames(lapply(lifestages, function(stage) {
    file_path <- file.path(rds_base_dir, species, paste0(species, "_", stage, "_dissolve.rds"))
    if (file.exists(file_path)) {
      readRDS(file_path)
    } else {
      NULL  # skip missing files
    }
  }), lifestages)
}), species_dirs)

stage_colors <- setNames(brewer.pal(length(lifestages), "Dark2"), lifestages)

# Gulf bounds
gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)

print("✅ global.R loaded successfully") 
