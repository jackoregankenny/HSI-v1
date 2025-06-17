# Soil Quality Modeling Functions
# Author: Auto-generated scaffold
# Purpose: Predict soil properties from hyperspectral data

library(caret)
library(randomForest)
library(e1071)
library(pls)
library(dplyr)
library(yaml)

# Calculate spectral indices for soil properties
calculate_soil_indices <- function(hyperspec_obj) {
  
  message("Calculating soil spectral indices...")
  
  spectra <- hyperspec_obj@data$spc
  wavelengths <- hyperspec_obj@wavelength
  
  # Helper function to find closest wavelength
  find_closest_band <- function(target_wl) {
    which.min(abs(wavelengths - target_wl))
  }
  
  indices <- data.frame(
    # Brightness Index
    BI = rowMeans(spectra[, find_closest_band(c(400, 700))], na.rm = TRUE),
    
    # Redness Index
    RI = spectra[, find_closest_band(670)] / spectra[, find_closest_band(550)],
    
    # Coloration Index
    CI = (spectra[, find_closest_band(650)] - spectra[, find_closest_band(550)]) / 
         (spectra[, find_closest_band(650)] + spectra[, find_closest_band(550)]),
    
    # SWIR ratio
    SWIR32 = spectra[, find_closest_band(1600)] / spectra[, find_closest_band(2200)]
  )
  
  return(indices)
}

# Feature extraction for soil modeling
extract_soil_features <- function(hyperspec_obj, config_params = NULL) {
  
  if (is.null(config_params)) {
    config_params <- yaml::read_yaml(here::here("config", "analysis_params.yml"))$feature_extraction
  }
  
  message("Extracting features for soil modeling...")
  
  # Basic spectral data
  spectra <- hyperspec_obj@data$spc
  wavelengths <- hyperspec_obj@wavelength
  
  features_list <- list()
  
  # 1. Spectral indices
  features_list$indices <- calculate_soil_indices(hyperspec_obj)
  
  # 2. Statistical moments
  features_list$statistics <- data.frame(
    mean_reflectance = rowMeans(spectra, na.rm = TRUE),
    std_reflectance = apply(spectra, 1, sd, na.rm = TRUE),
    skewness = apply(spectra, 1, function(x) {
      x <- x[!is.na(x)]
      if(length(x) < 3) return(NA)
      n <- length(x)
      (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
    }),
    kurtosis = apply(spectra, 1, function(x) {
      x <- x[!is.na(x)]
      if(length(x) < 4) return(NA)
      n <- length(x)
      (sum((x - mean(x))^4) / n) / (sum((x - mean(x))^2) / n)^2 - 3
    })
  )
  
  # 3. PCA features
  if (!is.null(config_params$pca)) {
    pca_result <- prcomp(spectra, center = TRUE, scale. = TRUE)
    n_comp <- min(config_params$pca$n_components, ncol(pca_result$x))
    features_list$pca <- pca_result$x[, 1:n_comp]
  }
  
  # Combine all features
  all_features <- do.call(cbind, features_list)
  
  return(all_features)
}

# Train soil property models
train_soil_models <- function(features, soil_properties, config_params = NULL) {
  
  if (is.null(config_params)) {
    config_params <- yaml::read_yaml(here::here("config", "analysis_params.yml"))$modeling
  }
  
  message("Training soil property prediction models...")
  
  models <- list()
  
  # Set up cross-validation
  cv_control <- trainControl(
    method = config_params$cross_validation$method,
    number = config_params$cross_validation$n_folds,
    repeats = config_params$cross_validation$n_repeats,
    verboseIter = FALSE
  )
  
  # Train models for each soil property
  for (property in names(soil_properties)) {
    message(glue("Training models for {property}..."))
    
    # Prepare data
    y <- soil_properties[[property]]
    valid_samples <- !is.na(y)
    
    if (sum(valid_samples) < 20) {
      warning(glue("Insufficient data for {property} modeling (n = {sum(valid_samples)})"))
      next
    }
    
    X <- features[valid_samples, ]
    y <- y[valid_samples]
    
    property_models <- list()
    
    # Random Forest
    if (config_params$algorithms$random_forest$enabled) {
      rf_grid <- expand.grid(mtry = c(2, 5, 10))
      property_models$random_forest <- train(
        X, y,
        method = "rf",
        trControl = cv_control,
        tuneGrid = rf_grid,
        ntree = config_params$algorithms$random_forest$n_trees
      )
    }
    
    # Partial Least Squares
    if (config_params$algorithms$partial_least_squares$enabled) {
      pls_grid <- expand.grid(ncomp = 1:config_params$algorithms$partial_least_squares$n_components)
      property_models$pls <- train(
        X, y,
        method = "pls",
        trControl = cv_control,
        tuneGrid = pls_grid
      )
    }
    
    # Support Vector Machine
    if (config_params$algorithms$svm$enabled) {
      svm_grid <- expand.grid(
        C = c(0.1, 1, 10),
        sigma = c(0.01, 0.1, 1)
      )
      property_models$svm <- train(
        X, y,
        method = "svmRadial",
        trControl = cv_control,
        tuneGrid = svm_grid
      )
    }
    
    models[[property]] <- property_models
  }
  
  return(models)
}

# Evaluate model performance
evaluate_models <- function(models, features, soil_properties) {
  
  message("Evaluating model performance...")
  
  evaluation_results <- list()
  
  for (property in names(models)) {
    message(glue("Evaluating models for {property}..."))
    
    y_true <- soil_properties[[property]]
    valid_samples <- !is.na(y_true)
    
    if (sum(valid_samples) == 0) next
    
    X <- features[valid_samples, ]
    y_true <- y_true[valid_samples]
    
    property_eval <- list()
    
    for (model_name in names(models[[property]])) {
      model <- models[[property]][[model_name]]
      
      # Get predictions
      y_pred <- predict(model, X)
      
      # Calculate metrics
      rmse <- sqrt(mean((y_true - y_pred)^2))
      mae <- mean(abs(y_true - y_pred))
      r2 <- cor(y_true, y_pred)^2
      
      # Calculate RPD and RPIQ
      rpd <- sd(y_true) / rmse
      rpiq <- (quantile(y_true, 0.75) - quantile(y_true, 0.25)) / rmse
      
      property_eval[[model_name]] <- data.frame(
        RMSE = rmse,
        MAE = mae,
        R2 = r2,
        RPD = rpd,
        RPIQ = rpiq
      )
    }
    
    evaluation_results[[property]] <- do.call(rbind, property_eval)
  }
  
  return(evaluation_results)
}

# Predict soil properties for new data
predict_soil_properties <- function(models, new_features, property_names = NULL) {
  
  if (is.null(property_names)) {
    property_names <- names(models)
  }
  
  predictions <- list()
  
  for (property in property_names) {
    if (!property %in% names(models)) {
      warning(glue("No model available for {property}"))
      next
    }
    
    property_predictions <- list()
    
    for (model_name in names(models[[property]])) {
      model <- models[[property]][[model_name]]
      pred <- predict(model, new_features)
      property_predictions[[model_name]] <- pred
    }
    
    # Ensemble prediction (mean of all models)
    if (length(property_predictions) > 1) {
      property_predictions$ensemble <- rowMeans(do.call(cbind, property_predictions))
    }
    
    predictions[[property]] <- property_predictions
  }
  
  return(predictions)
}

# Create comprehensive soil analysis report
generate_soil_report <- function(models, evaluation_results, predictions = NULL, 
                                output_dir = "output/reports") {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Summary statistics
  summary_stats <- list()
  
  for (property in names(evaluation_results)) {
    best_model <- evaluation_results[[property]][which.max(evaluation_results[[property]]$R2), ]
    summary_stats[[property]] <- list(
      best_model_name = rownames(best_model),
      best_r2 = best_model$R2,
      best_rmse = best_model$RMSE,
      best_rpd = best_model$RPD
    )
  }
  
  # Save detailed results
  report_data <- list(
    analysis_date = Sys.time(),
    model_evaluation = evaluation_results,
    summary_statistics = summary_stats,
    predictions = predictions
  )
  
  saveRDS(report_data, file.path(output_dir, "soil_analysis_report.rds"))
  
  message(glue("Soil analysis report saved to: {output_dir}"))
  
  return(report_data)
} 