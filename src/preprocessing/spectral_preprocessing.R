# Spectral Preprocessing Functions
# Author: Auto-generated scaffold
# Purpose: Clean and preprocess hyperspectral data for analysis

# Load required libraries
library(hyperSpec)  
library(hsdar)
library(prospectr)
library(signal)
library(dplyr)
library(yaml)

# Load analysis parameters
load_analysis_params <- function() {
  params_path <- here::here("config", "analysis_params.yml")
  if (!file.exists(params_path)) {
    stop("Analysis parameters file not found: ", params_path)
  }
  yaml::read_yaml(params_path)
}

# Convert various formats to hyperSpec object
convert_to_hyperspec <- function(data, wavelengths = NULL, metadata = NULL) {
  
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data.frame")
  }
  
  # Convert to matrix if data.frame
  if (is.data.frame(data)) {
    spectral_cols <- sapply(data, is.numeric)
    spectra_matrix <- as.matrix(data[, spectral_cols])
    metadata <- data[, !spectral_cols, drop = FALSE]
  } else {
    spectra_matrix <- data
  }
  
  # Generate wavelengths if not provided
  if (is.null(wavelengths)) {
    wavelengths <- seq(400, 2500, length.out = ncol(spectra_matrix))
    warning("Wavelengths not provided. Generated default wavelengths from 400-2500 nm")
  }
  
  # Create hyperSpec object
  hs_object <- new("hyperSpec", 
                   spc = spectra_matrix,
                   wavelength = wavelengths,
                   data = if(is.null(metadata)) data.frame(row = 1:nrow(spectra_matrix)) else metadata)
  
  return(hs_object)
}

# Spectral smoothing using Savitzky-Golay filter
apply_savgol_smooth <- function(hyperspec_obj, window_size = 5, poly_order = 2) {
  
  message("Applying Savitzky-Golay smoothing...")
  
  # Extract spectra
  spectra <- hyperspec_obj@data$spc
  
  # Apply smoothing to each spectrum
  smoothed_spectra <- t(apply(spectra, 1, function(x) {
    signal::sgolayfilt(x, p = poly_order, n = window_size)
  }))
  
  # Update hyperSpec object
  hyperspec_obj@data$spc <- smoothed_spectra
  
  return(hyperspec_obj)
}

# Standard Normal Variate (SNV) normalization
apply_snv_normalization <- function(hyperspec_obj) {
  
  message("Applying SNV normalization...")
  
  spectra <- hyperspec_obj@data$spc
  
  # Calculate SNV for each spectrum
  snv_spectra <- t(apply(spectra, 1, function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    if (sd_x == 0) return(x)  # Avoid division by zero
    (x - mean_x) / sd_x
  }))
  
  hyperspec_obj@data$spc <- snv_spectra
  
  return(hyperspec_obj)
}

# Multiplicative Scatter Correction (MSC)
apply_msc_normalization <- function(hyperspec_obj, reference = NULL) {
  
  message("Applying MSC normalization...")
  
  spectra <- hyperspec_obj@data$spc
  
  # Use mean spectrum as reference if not provided
  if (is.null(reference)) {
    reference <- colMeans(spectra, na.rm = TRUE)
  }
  
  # Apply MSC to each spectrum
  msc_spectra <- t(apply(spectra, 1, function(x) {
    # Linear regression: x = a + b * reference
    lm_model <- lm(x ~ reference)
    coeffs <- coef(lm_model)
    
    # Correct for multiplicative and additive effects
    (x - coeffs[1]) / coeffs[2]
  }))
  
  hyperspec_obj@data$spc <- msc_spectra
  
  return(hyperspec_obj)
}

# Continuum removal
apply_continuum_removal <- function(hyperspec_obj) {
  
  message("Applying continuum removal...")
  
  spectra <- hyperspec_obj@data$spc
  wavelengths <- hyperspec_obj@wavelength
  
  # Apply continuum removal to each spectrum
  cr_spectra <- t(apply(spectra, 1, function(spectrum) {
    # Find convex hull (continuum)
    hull_indices <- chull(wavelengths, spectrum)
    hull_indices <- hull_indices[order(wavelengths[hull_indices])]
    
    # Interpolate continuum
    continuum <- approx(wavelengths[hull_indices], spectrum[hull_indices], 
                       xout = wavelengths, method = "linear", rule = 2)$y
    
    # Remove continuum
    spectrum / continuum
  }))
  
  hyperspec_obj@data$spc <- cr_spectra
  
  return(hyperspec_obj)
}

# Spectral derivatives
calculate_derivatives <- function(hyperspec_obj, order = 1, gap_size = 5) {
  
  message(glue("Calculating {order} order derivatives..."))
  
  spectra <- hyperspec_obj@data$spc
  
  if (order == 1) {
    # First derivative
    deriv_spectra <- t(apply(spectra, 1, function(x) {
      c(rep(NA, gap_size), diff(x, lag = gap_size), rep(NA, gap_size))
    }))
  } else if (order == 2) {
    # Second derivative
    deriv_spectra <- t(apply(spectra, 1, function(x) {
      first_deriv <- c(rep(NA, gap_size), diff(x, lag = gap_size), rep(NA, gap_size))
      c(rep(NA, gap_size), diff(first_deriv, lag = gap_size, na.rm = TRUE), rep(NA, gap_size))
    }))
  } else {
    stop("Only first and second derivatives are supported")
  }
  
  # Create new hyperSpec object for derivatives
  deriv_obj <- hyperspec_obj
  deriv_obj@data$spc <- deriv_spectra
  
  return(deriv_obj)
}

# Outlier detection using Mahalanobis distance
detect_spectral_outliers <- function(hyperspec_obj, threshold = 0.95) {
  
  message("Detecting spectral outliers...")
  
  spectra <- hyperspec_obj@data$spc
  
  # Remove any rows with all NAs
  complete_rows <- complete.cases(spectra)
  clean_spectra <- spectra[complete_rows, ]
  
  if (nrow(clean_spectra) < 3) {
    warning("Insufficient data for outlier detection")
    return(rep(FALSE, nrow(spectra)))
  }
  
  # Calculate Mahalanobis distances
  center <- colMeans(clean_spectra, na.rm = TRUE)
  cov_matrix <- cov(clean_spectra, use = "complete.obs")
  
  # Handle singular covariance matrix
  if (det(cov_matrix) == 0) {
    message("Singular covariance matrix, using simplified outlier detection")
    distances <- apply(clean_spectra, 1, function(x) {
      sqrt(sum((x - center)^2, na.rm = TRUE))
    })
    outlier_threshold <- quantile(distances, threshold, na.rm = TRUE)
  } else {
    distances <- mahalanobis(clean_spectra, center, cov_matrix)
    outlier_threshold <- qchisq(threshold, df = ncol(clean_spectra))
  }
  
  # Identify outliers
  outliers <- rep(FALSE, nrow(spectra))
  outliers[complete_rows] <- distances > outlier_threshold
  
  message(glue("Detected {sum(outliers)} outliers out of {nrow(spectra)} spectra"))
  
  return(outliers)
}

# Spectral resampling
resample_spectra <- function(hyperspec_obj, target_wavelengths, method = "linear") {
  
  message("Resampling spectra to target wavelengths...")
  
  spectra <- hyperspec_obj@data$spc
  original_wavelengths <- hyperspec_obj@wavelength
  
  # Resample each spectrum
  resampled_spectra <- t(apply(spectra, 1, function(spectrum) {
    approx(original_wavelengths, spectrum, xout = target_wavelengths, 
           method = method, rule = 2)$y
  }))
  
  # Create new hyperSpec object
  resampled_obj <- hyperspec_obj
  resampled_obj@data$spc <- resampled_spectra
  resampled_obj@wavelength <- target_wavelengths
  
  return(resampled_obj)
}

# Quality control checks
perform_quality_checks <- function(hyperspec_obj, params = NULL) {
  
  message("Performing quality control checks...")
  
  if (is.null(params)) {
    params <- load_analysis_params()$quality_control
  }
  
  spectra <- hyperspec_obj@data$spc
  wavelengths <- hyperspec_obj@wavelength
  
  qc_results <- list()
  
  # Check spectral range
  qc_results$spectral_range <- max(wavelengths) - min(wavelengths)
  qc_results$spectral_range_ok <- qc_results$spectral_range >= params$min_spectral_range
  
  # Check for missing data
  missing_prop <- sum(is.na(spectra)) / length(spectra)
  qc_results$missing_data_prop <- missing_prop
  qc_results$missing_data_ok <- missing_prop <= params$max_missing_data
  
  # Check for negative values (after preprocessing)
  qc_results$negative_values <- sum(spectra < 0, na.rm = TRUE)
  qc_results$negative_values_ok <- qc_results$negative_values == 0
  
  # Signal-to-noise ratio estimation (simplified)
  signal_estimate <- apply(spectra, 1, function(x) mean(x, na.rm = TRUE))
  noise_estimate <- apply(spectra, 1, function(x) sd(x, na.rm = TRUE))
  snr <- signal_estimate / noise_estimate
  qc_results$mean_snr <- mean(snr, na.rm = TRUE)
  qc_results$snr_ok <- qc_results$mean_snr >= params$min_snr
  
  # Overall quality assessment
  qc_results$overall_quality <- all(c(
    qc_results$spectral_range_ok,
    qc_results$missing_data_ok,
    qc_results$negative_values_ok,
    qc_results$snr_ok
  ))
  
  message(glue("Quality control summary:"))
  message(glue("  - Spectral range: {round(qc_results$spectral_range)} nm ({ifelse(qc_results$spectral_range_ok, 'PASS', 'FAIL')})"))
  message(glue("  - Missing data: {round(qc_results$missing_data_prop * 100, 2)}% ({ifelse(qc_results$missing_data_ok, 'PASS', 'FAIL')})"))
  message(glue("  - Mean SNR: {round(qc_results$mean_snr, 2)} ({ifelse(qc_results$snr_ok, 'PASS', 'FAIL')})"))
  message(glue("  - Overall quality: {ifelse(qc_results$overall_quality, 'PASS', 'FAIL')}"))
  
  return(qc_results)
}

# Main preprocessing pipeline
preprocess_hyperspectral_data <- function(input_data, wavelengths = NULL, 
                                        config_params = NULL, output_path = NULL) {
  
  # Load configuration if not provided
  if (is.null(config_params)) {
    config_params <- load_analysis_params()$preprocessing
  }
  
  message("Starting hyperspectral data preprocessing pipeline...")
  
  # Convert to hyperSpec object
  hs_data <- convert_to_hyperspec(input_data, wavelengths)
  
  # Initial quality check
  initial_qc <- perform_quality_checks(hs_data)
  
  # Apply preprocessing steps based on configuration
  
  # 1. Smoothing
  if (config_params$smoothing$method == "savitzky_golay") {
    hs_data <- apply_savgol_smooth(
      hs_data, 
      window_size = config_params$smoothing$window_size,
      poly_order = config_params$smoothing$polynomial_order
    )
  }
  
  # 2. Normalization
  if (config_params$normalization$method == "snv") {
    hs_data <- apply_snv_normalization(hs_data)
  } else if (config_params$normalization$method == "msc") {
    hs_data <- apply_msc_normalization(hs_data)
  } else if (config_params$normalization$method == "continuum_removal") {
    hs_data <- apply_continuum_removal(hs_data)
  }
  
  # 3. Outlier detection and removal
  outliers <- detect_spectral_outliers(
    hs_data, 
    threshold = 1 - config_params$outlier_detection$threshold
  )
  
  if (sum(outliers) > 0) {
    message(glue("Removing {sum(outliers)} outlier spectra"))
    hs_data <- hs_data[!outliers, ]
  }
  
  # 4. Spectral resampling if specified
  if (!is.null(config_params$resampling$target_resolution)) {
    current_range <- range(hs_data@wavelength)
    target_wavelengths <- seq(
      current_range[1], 
      current_range[2], 
      by = config_params$resampling$target_resolution
    )
    hs_data <- resample_spectra(
      hs_data, 
      target_wavelengths, 
      method = config_params$resampling$interpolation_method
    )
  }
  
  # Final quality check
  final_qc <- perform_quality_checks(hs_data)
  
  # Prepare output
  result <- list(
    preprocessed_data = hs_data,
    initial_quality = initial_qc,
    final_quality = final_qc,
    processing_steps = names(config_params),
    outliers_removed = sum(outliers),
    preprocessing_time = Sys.time()
  )
  
  # Save results if output path specified
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, output_path)
    message(glue("Preprocessed data saved to: {output_path}"))
  }
  
  message("Preprocessing pipeline completed successfully!")
  
  return(result)
} 