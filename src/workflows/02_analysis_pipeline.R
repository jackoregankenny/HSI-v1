# Analysis Pipeline Workflow
# Author: Auto-generated scaffold  
# Purpose: Complete hyperspectral data analysis for soil quality assessment

library(here)
library(glue)
library(dplyr)

# Source analysis functions
source(here("src", "preprocessing", "spectral_preprocessing.R"))
source(here("src", "analysis", "soil_quality_modeling.R"))

# Main analysis pipeline
main_analysis_pipeline <- function(data_dir = "data/raw", 
                                  output_dir = "output") {
  
  message("=== Starting Hyperspectral Analysis Pipeline ===")
  
  # Check for collected data
  metadata_file <- file.path(data_dir, "collection_metadata.rds")
  if (!file.exists(metadata_file)) {
    stop("No collection metadata found. Please run data collection first.")
  }
  
  metadata <- readRDS(metadata_file)
  message(glue("Found data collected on: {metadata$collection_date}"))
  
  # Create output directories
  dir.create(file.path(output_dir, "processed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "reports"), recursive = TRUE, showWarnings = FALSE)
  
  # For demonstration, create sample data
  # In practice, you would load your actual hyperspectral data here
  sample_data <- create_sample_hyperspectral_data()
  
  # Preprocessing
  message("\n--- Preprocessing Phase ---")
  preprocessed_result <- preprocess_hyperspectral_data(
    input_data = sample_data$spectra,
    wavelengths = sample_data$wavelengths,
    output_path = file.path(output_dir, "processed", "preprocessed_data.rds")
  )
  
  # Feature extraction
  message("\n--- Feature Extraction Phase ---")
  features <- extract_soil_features(preprocessed_result$preprocessed_data)
  
  # Model training (using sample soil data)
  message("\n--- Model Training Phase ---")
  soil_properties <- sample_data$soil_properties
  
  models <- train_soil_models(features, soil_properties)
  saveRDS(models, file.path(output_dir, "models", "soil_models.rds"))
  
  # Model evaluation
  message("\n--- Model Evaluation Phase ---")
  evaluation_results <- evaluate_models(models, features, soil_properties)
  
  # Generate predictions
  message("\n--- Prediction Phase ---")
  predictions <- predict_soil_properties(models, features)
  
  # Generate final report
  message("\n--- Report Generation Phase ---")
  report <- generate_soil_report(
    models = models,
    evaluation_results = evaluation_results,
    predictions = predictions,
    output_dir = file.path(output_dir, "reports")
  )
  
  message("=== Analysis Pipeline Complete ===")
  message(glue("Results saved to: {output_dir}"))
  
  return(list(
    preprocessed_data = preprocessed_result,
    models = models,
    evaluation = evaluation_results,
    predictions = predictions,
    report = report
  ))
}

# Helper function to create sample data for demonstration
create_sample_hyperspectral_data <- function() {
  
  # Generate sample spectral data
  n_samples <- 100
  wavelengths <- seq(400, 2500, by = 5)
  n_bands <- length(wavelengths)
  
  # Create realistic-looking soil spectra
  spectra <- t(replicate(n_samples, {
    # Base soil spectrum
    base_spectrum <- 0.1 + 0.3 * exp(-((wavelengths - 800) / 300)^2) +
                   0.2 * exp(-((wavelengths - 1400) / 200)^2) +
                   0.1 * exp(-((wavelengths - 2200) / 150)^2)
    
    # Add noise and variability
    base_spectrum + rnorm(n_bands, 0, 0.02) + runif(1, -0.05, 0.05)
  }))
  
  # Generate sample soil properties
  soil_properties <- data.frame(
    organic_matter = rnorm(n_samples, 3.5, 1.2),
    ph = rnorm(n_samples, 6.5, 0.8),
    clay_content = rnorm(n_samples, 25, 8),
    nitrogen = rnorm(n_samples, 1200, 300),
    phosphorus = rnorm(n_samples, 45, 15)
  )
  
  # Ensure realistic ranges
  soil_properties$organic_matter <- pmax(0.5, pmin(soil_properties$organic_matter, 8))
  soil_properties$ph <- pmax(4.5, pmin(soil_properties$ph, 9))
  soil_properties$clay_content <- pmax(5, pmin(soil_properties$clay_content, 60))
  soil_properties$nitrogen <- pmax(200, pmin(soil_properties$nitrogen, 3000))
  soil_properties$phosphorus <- pmax(5, pmin(soil_properties$phosphorus, 150))
  
  return(list(
    spectra = spectra,
    wavelengths = wavelengths,
    soil_properties = soil_properties
  ))
}

# Run pipeline if called directly
if (interactive()) {
  results <- main_analysis_pipeline()
  message("Analysis pipeline completed successfully!")
} 