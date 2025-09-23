# Packages 

library(sf)
library(tidyr)
library(dplyr)
library(leaflet)
library(viridisLite)
library(RColorBrewer) #lifestage, habitattype color convention
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
library(mapview)
library(webshot2)
library(Polychrome)


## All gulf Council staff should be able to access and run the app
## R code is : Gulf of Mexico - Documents\EFH\EFH Generic Amendment 5\000_RShiny App Code\Maps_RShiny (2).zip\Maps_RShiny

#Load polygon layer data csv file 
polygon_layer_data <-read.csv("species_habitat_clean_pretty.csv", stringsAsFactors = FALSE)

# Remove unwanted combinations: Offshore + Mangrove or Emergent Marsh
polygon_layer_data <- polygon_layer_data %>%
  filter(!(habitatzone == "off" & habitattype %in% c("mangrove", "em", "sav")))

#Load EFH RDS data 
rds_base_dir <- "RDS_Species_Habitat_V2"


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
    file_path <- file.path(rds_base_dir, species, paste0(species, "_", stage, ".rds"))
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


# Define fixed color palette for all habitat types
# Fixed EFH Habitat Type palette (high contrast, no blues)
# Fixed EFH Habitat Type palette (pastel + high contrast, no blues)
habitat_palette <- c(
  "Emergent Marsh"               = "#FBB4AE", # pastel red/pink
  "Hard Bottom"                  = "#FDB462", # pastel orange
  "Mangrove"                     = "#FFFFB3", # pastel yellow
  "Oyster Reef"                  = "#B3DE69", # pastel green
  "Reef"                         = "#CAB2D6", # pastel purple
  "Sand"                         = "#FCCDE5", # pastel pink
  "Submerged Aquatic Vegetation" = "#CCEBC5", # mint green
  "Soft Bottom"                  = "#E5C494", # tan/beige
  "Shelf/Slope Edge"             = "#FFED6F", # bright pastel yellow
  "Water Column Associated"      = "#BC80BD"  # pastel magenta/purple
)



# -------------------------------------------------------------------
# Lookup maps for pretty names
# -------------------------------------------------------------------

##species
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

# Habitat type pretty names
habitat_map <- c(
  "em" = "Emergent Marsh",
  "hb" = "Hard Bottom",
  "mangrove" = "Mangrove",
  "oyster" = "Oyster Reef",
  "reef" = "Reef",
  "sand" = "Sand",
  "sav" = "SAV",
  "sb" = "Soft Bottom",
  "shelf" = "Shelf/Slope Edge",
  "wca" = "WCA"
)

# Zone pretty names
zone_map <- c(
  "est" = "Estuarine",
  "near" = "Nearshore",
  "off"  = "Offshore"
)

# Eco-region pretty names
er_map <- c(
  "er1"= "ER1",
  "er2"= "ER2",
  "er3"= "ER3",
  "er4"= "ER4",
  "er5"= "ER5"
)

# Gulf bounds
gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)

print("✅ global.R loaded successfully") 
