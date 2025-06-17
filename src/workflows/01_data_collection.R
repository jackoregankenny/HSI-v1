# Data Collection Workflow
# Author: Auto-generated scaffold
# Purpose: Orchestrate hyperspectral data collection from multiple sources

# Load required libraries and functions
library(here)
library(glue)
library(sf)
library(yaml)

# Source utility functions
source(here("src", "data_collection", "api_client.R"))

# Load configuration
config <- yaml::read_yaml(here("config", "data_sources.yml"))

# Define study area (example - replace with your field boundaries)
study_area <- data.frame(
  lon = c(-72.5, -72.4, -72.4, -72.5, -72.5),
  lat = c(42.5, 42.5, 42.6, 42.6, 42.5)
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_bbox()

# Define date range for data collection
date_range <- c("2023-01-01", "2023-12-31")

# Main data collection workflow
main_data_collection <- function() {
  
  message("=== Starting Hyperspectral Data Collection Workflow ===")
  message(glue("Study area: {paste(study_area, collapse=', ')}"))
  message(glue("Date range: {date_range[1]} to {date_range[2]}"))
  
  # Create output directory
  output_dir <- here("data", "raw")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Collect data from all available sources
  collected_data <- collect_hyperspectral_data(
    field_boundaries = study_area,
    date_range = date_range,
    output_dir = output_dir,
    sources = c("neon", "usgs_library")  # Add "sentinel2" when credentials available
  )
  
  message("=== Data Collection Workflow Complete ===")
  
  return(collected_data)
}

# Run the workflow
if (interactive()) {
  results <- main_data_collection()
  message("Data collection completed. Check the 'data/raw' directory for results.")
} 