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

# Load EFH URL data
URL_dir <- read.csv("C:/Users/Sarah/Documents/GitHub/2025-EFH-5-year-Review_Habitat-Maps/species_habitatmap_url.csv", stringsAsFactors = FALSE)

# Life stage codes used in data
life_stages <- unique(URL_dir$lifestage)

# Nicely formatted labels for legend display
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
stage_colors <- setNames(brewer.pal(length(lifestages), "Set2"), lifestages)

# Gulf bounds
gulf_bounds <- list(lng1 = -98, lat1 = 23, lng2 = -81, lat2 = 34)

print("✅ global.R loaded successfully")