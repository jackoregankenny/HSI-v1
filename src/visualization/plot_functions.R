# Visualization Functions for Hyperspectral Soil Analysis
# Author: Auto-generated scaffold
# Purpose: Create plots and visualizations for spectral data and results

library(ggplot2)
library(plotly)
library(viridis)
library(dplyr)
library(gridExtra)

# Plot spectral profiles
plot_spectral_profiles <- function(hyperspec_obj, n_samples = 10, interactive = FALSE) {
  
  spectra <- hyperspec_obj@data$spc
  wavelengths <- hyperspec_obj@wavelength
  
  # Select random samples if too many
  if (nrow(spectra) > n_samples) {
    selected_indices <- sample(nrow(spectra), n_samples)
    spectra <- spectra[selected_indices, ]
  }
  
  # Convert to long format for plotting
  plot_data <- expand.grid(
    wavelength = wavelengths,
    sample = 1:nrow(spectra)
  )
  plot_data$reflectance <- as.vector(t(spectra))
  
  p <- ggplot(plot_data, aes(x = wavelength, y = reflectance, color = factor(sample))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Hyperspectral Profiles",
      x = "Wavelength (nm)",
      y = "Reflectance",
      color = "Sample"
    ) +
    theme_minimal() +
    scale_color_viridis_d() +
    guides(color = "none")
  
  if (interactive) {
    return(ggplotly(p))
  } else {
    return(p)
  }
}

# Plot soil property predictions
plot_soil_predictions <- function(predictions, actual_values = NULL, property = "organic_matter") {
  
  if (!property %in% names(predictions)) {
    stop(glue("Property '{property}' not found in predictions"))
  }
  
  pred_data <- predictions[[property]]
  
  # Use ensemble predictions if available, otherwise first model
  if ("ensemble" %in% names(pred_data)) {
    y_pred <- pred_data$ensemble
  } else {
    y_pred <- pred_data[[1]]
  }
  
  plot_df <- data.frame(
    predicted = y_pred,
    sample_id = 1:length(y_pred)
  )
  
  if (!is.null(actual_values)) {
    plot_df$actual <- actual_values[[property]]
  }
  
  # Scatter plot if actual values available
  if (!is.null(actual_values)) {
    p <- ggplot(plot_df, aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = glue("Predicted vs Actual {str_to_title(gsub('_', ' ', property))}"),
        x = "Actual Values",
        y = "Predicted Values"
      ) +
      theme_minimal()
  } else {
    # Histogram of predictions
    p <- ggplot(plot_df, aes(x = predicted)) +
      geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue") +
      labs(
        title = glue("Distribution of Predicted {str_to_title(gsub('_', ' ', property))}"),
        x = "Predicted Values",
        y = "Frequency"
      ) +
      theme_minimal()
  }
  
  return(p)
}

# Plot model performance comparison
plot_model_performance <- function(evaluation_results, metric = "R2") {
  
  # Convert evaluation results to long format
  perf_data <- data.frame()
  
  for (property in names(evaluation_results)) {
    prop_results <- evaluation_results[[property]]
    prop_results$model <- rownames(prop_results)
    prop_results$property <- property
    perf_data <- rbind(perf_data, prop_results)
  }
  
  if (!metric %in% colnames(perf_data)) {
    stop(glue("Metric '{metric}' not found in evaluation results"))
  }
  
  p <- ggplot(perf_data, aes(x = property, y = .data[[metric]], fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = glue("Model Performance Comparison ({metric})"),
      x = "Soil Property",
      y = metric,
      fill = "Model"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_viridis_d()
  
  return(p)
}

# Create comprehensive dashboard
create_analysis_dashboard <- function(preprocessed_data, predictions, 
                                    evaluation_results, output_path = NULL) {
  
  # Spectral profiles plot
  p1 <- plot_spectral_profiles(preprocessed_data, n_samples = 5)
  
  # Model performance plot
  p2 <- plot_model_performance(evaluation_results, "R2")
  
  # Soil property prediction plots
  p3 <- plot_soil_predictions(predictions, property = "organic_matter")
  p4 <- plot_soil_predictions(predictions, property = "ph")
  
  # Combine plots
  dashboard <- grid.arrange(
    p1, p2, p3, p4,
    ncol = 2,
    top = "Hyperspectral Soil Analysis Dashboard"
  )
  
  if (!is.null(output_path)) {
    ggsave(output_path, dashboard, width = 16, height = 12, dpi = 300)
    message(glue("Dashboard saved to: {output_path}"))
  }
  
  return(dashboard)
} 