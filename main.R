# Main Entry Point for Hyperspectral Soil Analysis Project
# Author: Auto-generated scaffold
# Purpose: Provides a simple interface to run the complete analysis workflow

# Load required packages
library(here)

# Main function to run complete workflow
main <- function(run_data_collection = TRUE, run_analysis = TRUE, 
                create_visualizations = TRUE) {
  
  cat("==========================================\n")
  cat("Hyperspectral Soil Quality Analysis System\n")
  cat("==========================================\n\n")
  
  # Step 1: Install packages if needed
  cat("Checking and installing required packages...\n")
  if (file.exists(here("src", "setup", "install_packages.R"))) {
    source(here("src", "setup", "install_packages.R"))
  }
  
  # Step 2: Data Collection
  if (run_data_collection) {
    cat("\n--- STEP 1: DATA COLLECTION ---\n")
    tryCatch({
      source(here("src", "workflows", "01_data_collection.R"))
      cat("✓ Data collection completed successfully\n")
    }, error = function(e) {
      cat("✗ Data collection failed:", e$message, "\n")
      cat("Note: This may be due to missing API credentials or network issues\n")
    })
  }
  
  # Step 3: Analysis Pipeline
  if (run_analysis) {
    cat("\n--- STEP 2: ANALYSIS PIPELINE ---\n")
    tryCatch({
      source(here("src", "workflows", "02_analysis_pipeline.R"))
      cat("✓ Analysis pipeline completed successfully\n")
    }, error = function(e) {
      cat("✗ Analysis pipeline failed:", e$message, "\n")
    })
  }
  
  # Step 4: Create Visualizations
  if (create_visualizations) {
    cat("\n--- STEP 3: VISUALIZATION ---\n")
    tryCatch({
      source(here("src", "visualization", "plot_functions.R"))
      cat("✓ Visualization functions loaded successfully\n")
    }, error = function(e) {
      cat("✗ Visualization setup failed:", e$message, "\n")
    })
  }
  
  cat("\n==========================================\n")
  cat("Workflow completed!\n")
  cat("Check the 'output' directory for results.\n")
  cat("==========================================\n")
}

# Quick start function for demo
demo <- function() {
  cat("Running demonstration with sample data...\n")
  
  # Create necessary directories
  dir.create("logs", showWarnings = FALSE)
  
  # Run analysis pipeline only (skip data collection for demo)
  main(run_data_collection = FALSE, run_analysis = TRUE, create_visualizations = TRUE)
  
  cat("\nDemo completed! Check 'output' directory for results.\n")
}

# Helper function to display project structure
show_project_structure <- function() {
  cat("Project Structure:\n")
  cat("├── README.md                    # Project documentation\n")
  cat("├── HyperspectralSoilAnalysis.Rproj  # R project file\n")
  cat("├── renv.lock                    # Package dependencies\n")
  cat("├── main.R                       # Main entry point (this file)\n")
  cat("├── config/                      # Configuration files\n")
  cat("│   ├── data_sources.yml         # Data source definitions\n")
  cat("│   └── analysis_params.yml      # Analysis parameters\n")
  cat("├── src/                         # Source code\n")
  cat("│   ├── setup/                   # Setup scripts\n")
  cat("│   ├── data_collection/         # Data collection functions\n")
  cat("│   ├── preprocessing/           # Data preprocessing\n")
  cat("│   ├── analysis/               # Analysis functions\n")
  cat("│   ├── visualization/          # Plotting functions\n")
  cat("│   └── workflows/              # Main workflows\n")
  cat("├── data/                       # Data directories\n")
  cat("│   ├── raw/                    # Raw data\n")
  cat("│   ├── processed/              # Processed data\n")
  cat("│   └── external/               # External reference data\n")
  cat("├── output/                     # Output directories\n")
  cat("│   ├── figures/                # Generated plots\n")
  cat("│   ├── reports/                # Analysis reports\n")
  cat("│   └── models/                 # Saved models\n")
  cat("├── docs/                       # Documentation\n")
  cat("└── tests/                      # Unit tests\n")
}

# Display usage information
usage <- function() {
  cat("Hyperspectral Soil Analysis Project\n")
  cat("===================================\n\n")
  cat("Usage:\n")
  cat("  main()                         # Run complete workflow\n")
  cat("  demo()                         # Run demonstration with sample data\n")
  cat("  show_project_structure()       # Display project structure\n")
  cat("  usage()                        # Show this help message\n\n")
  cat("Workflow Components:\n")
  cat("  1. Data Collection: Fetch hyperspectral data from internet sources\n")
  cat("  2. Preprocessing: Clean and prepare spectral data\n")
  cat("  3. Analysis: Train models and predict soil properties\n")
  cat("  4. Visualization: Generate plots and reports\n\n")
  cat("Configuration:\n")
  cat("  - Edit 'config/data_sources.yml' to configure data sources\n")
  cat("  - Edit 'config/analysis_params.yml' to adjust analysis parameters\n\n")
  cat("For detailed information, see README.md\n")
}

# Run usage information when script is loaded
if (interactive()) {
  usage()
} 