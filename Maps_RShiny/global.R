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

setwd("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/Maps_RShiny")
getwd()  # confirm current working directory


#Load polygon layer data csv file 
polygon_layer_data <-read.csv("species_habitat_clean_pretty.csv", stringsAsFactors = FALSE)

#Load EFH RDS data 
rds_base_dir <- "RDS_simplified"

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
#################### habitat Type ########################
# Folder with combined habitat RDS
habitat_combined_dir <- "Habitat_Type_RDS_simplified/Habitat_Type_Combined"

# Auto-detect all combined habitat RDS files
habitat_files <- list.files(habitat_combined_dir, pattern = "_combined\\.rds$", full.names = TRUE)

# Extract short habitat codes from filenames (e.g., em, hb)
habitat_codes <- sub("_combined\\.rds$", "", basename(habitat_files))

# Create named list for dropdown choices (pretty name = code)
habitat_choices <- setNames(habitat_codes, c(
  "Emergent Marsh","Hard Bottom","Mangrove","Oyster Reef","Reef",
  "Sand","Submerged Aquatic Vegetation","Soft Bottom","Shelf/Slope Edge","Water Column Associated"
))

# Preload all habitat sf objects into a list
habitat_sf <- setNames(lapply(habitat_files, readRDS), habitat_codes)

########################## Habitat Zone #################################
# Load Habitat Zone RDS
habitat_zone_dir <- "RDS_Habitat_Zone"

# Auto-detect all habitat zone RDS files
zone_files <- list.files(habitat_zone_dir, pattern = "\\.rds$", full.names = TRUE)

# Expected short codes (estuarine, nearshore, offshore)
zone_choices <- c("Estuarine" = "estuarine",
                  "Nearshore" = "nearshore",
                  "Offshore" = "offshore")

# Preload all sf objects
zone_sf <- setNames(lapply(zone_choices, function(z) {
  file_path <- file.path(habitat_zone_dir, paste0(z, ".rds"))
  if (file.exists(file_path)) readRDS(file_path) else NULL
}), zone_choices)

zone_colors <- setNames(c("lightblue", "yellow", "violet"), zone_choices)

# species_pretty = display name, species_code = folder name / rds prefix
species_lookup <- c(
  "Almaco Jack" = "almacojack",
  "Banded Rudderfish" = "bandedrudderfish",
  "Blackfin Snapper" = "blackfinsnapper",
  "Black Grouper" = "blackgrouper",
  "Brown Shrimp" = "brownshrimp",
  "Cobia" = "cobia",
  "Cubera Snapper" = "cuberasnapper",
  "Gag Grouper" = "gaggrouper",
  "Goldface Tilefish" = "goldfacetilefish",
  "Goliath Grouper" = "goliathgrouper",
  "Gray Snapper" = "graysnapper",
  "Gray Triggerfish" = "graytriggerfish",
  "Greater Amberjack" = "greateramberjack",
  "Hogfish" = "hogfish",
  "King Mackerel" = "kingmackerel",
  "Lane Snapper" = "lanesnapper",
  "Lesser Amberjack" = "lesseramberjack",
  "Pink Shrimp" = "pinkshrimp",
  "Red Drum" = "reddrum",
  "Red Grouper" = "redgrouper",
  "Red Snapper" = "redsnapper",
  "Royal Red Shrimp" = "royalredshrimp",
  "Scamp" = "scamp",
  "Silk Snapper" = "silksnapper",
  "Snowy Grouper" = "snowygrouper",
  "Spanish Mackerel" = "spanishmackerel",
  "Speckled Hind" = "speckledhind",
  "Spiny Lobster" = "spinylobster",
  "Tilefish" = "tilefish",
  "Vermillion Snapper" = "vermillionsnapper",
  "Warsaw Grouper" = "warsawgrouper",
  "Wenchman" = "wenchman",
  "White Shrimp" = "whiteshrimp",
  "Yellowedge Grouper" = "yellowedgegrouper",
  "Yellowfin Grouper" = "yellowfingrouper",
  "Yellowmouth Grouper" = "yellowmouthgrouper",
  "Yellowtail Snapper" = "yellowtailsnapper"
)


# Gulf bounds
gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)

print("✅ global.R loaded successfully") 
