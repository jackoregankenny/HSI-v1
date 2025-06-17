# Install Required Packages for Hyperspectral Soil Analysis
# Author: Auto-generated scaffold
# Purpose: Install and load all necessary R packages

# Check if renv is installed, if not install it
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv for the project
renv::restore()

# Core packages for hyperspectral analysis
required_packages <- c(
  # Hyperspectral data analysis
  "hsdar",
  "hyperSpec",
  "prospectr",
  
  # Spatial data handling
  "terra",
  "sf",
  "stars",
  "mapview",
  
  # Data manipulation and visualization
  "tidyverse",
  "dplyr",
  "ggplot2",
  "plotly",
  "viridis",
  
  # Web data access
  "httr",
  "rvest",
  "jsonlite",
  "curl",
  
  # Machine learning and statistics
  "caret",
  "randomForest",
  "e1071",
  "pls",
  
  # Signal processing
  "signal",
  "pracma",
  
  # Configuration and utilities
  "yaml",
  "here",
  "glue",
  "progress",
  
  # Parallel processing
  "doParallel",
  "foreach",
  
  # Report generation
  "rmarkdown",
  "knitr",
  "DT"
)

# Function to install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg, dependencies = TRUE)
  } else {
    message(paste(pkg, "already installed"))
  }
}

# Install all required packages
message("Installing required packages for hyperspectral soil analysis...")
sapply(required_packages, install_if_missing)

# Load core packages
library(dplyr)
library(ggplot2)
library(here)

message("Package installation complete!")
message("Use renv::snapshot() to save the current package versions to renv.lock") 