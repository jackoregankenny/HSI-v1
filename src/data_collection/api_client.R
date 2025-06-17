# API Client for Hyperspectral Data Collection
# Author: Auto-generated scaffold
# Purpose: Fetch hyperspectral data from various internet sources

# Load required libraries
library(httr)
library(jsonlite)
library(yaml)
library(dplyr)
library(sf)
library(glue)
library(progress)

# Load configuration
load_config <- function() {
  config_path <- here::here("config", "data_sources.yml")
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }
  yaml::read_yaml(config_path)
}

# Generic API request function with error handling
make_api_request <- function(url, headers = NULL, params = NULL, method = "GET", 
                            timeout = 300, max_retries = 3) {
  
  for (attempt in 1:max_retries) {
    tryCatch({
      response <- httr::VERB(
        method,
        url = url,
        query = params,
        add_headers(.headers = headers),
        timeout(timeout)
      )
      
      if (httr::status_code(response) == 200) {
        return(response)
      } else {
        warning(glue("API request failed with status {status_code(response)} on attempt {attempt}"))
      }
      
    }, error = function(e) {
      warning(glue("API request error on attempt {attempt}: {e$message}"))
      if (attempt < max_retries) {
        Sys.sleep(2^attempt)  # Exponential backoff
      }
    })
  }
  
  stop(glue("Failed to complete API request after {max_retries} attempts"))
}

# Sentinel-2 data collection
fetch_sentinel2_data <- function(bbox, date_range, cloud_cover_max = 10, 
                                username = NULL, password = NULL) {
  
  config <- load_config()
  sentinel_config <- config$satellite_sources$sentinel2
  
  if (is.null(username) || is.null(password)) {
    stop("Sentinel-2 requires authentication. Please provide username and password.")
  }
  
  # Format search parameters
  params <- list(
    q = glue("footprint:\"Intersects(POLYGON(({bbox[1]} {bbox[2]},{bbox[3]} {bbox[2]},{bbox[3]} {bbox[4]},{bbox[1]} {bbox[4]},{bbox[1]} {bbox[2]})))\" AND platformname:Sentinel-2 AND cloudcoverpercentage:[0 TO {cloud_cover_max}] AND beginposition:[{date_range[1]} TO {date_range[2]}]"),
    rows = 100,
    format = "json"
  )
  
  # Make authenticated request
  auth_header <- httr::authenticate(username, password)
  
  message("Searching for Sentinel-2 data...")
  response <- make_api_request(
    url = sentinel_config$api_endpoint,
    headers = auth_header,
    params = params
  )
  
  result <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()
  
  if (result$feed$opensearch$totalResults > 0) {
    message(glue("Found {result$feed$opensearch$totalResults} Sentinel-2 products"))
    return(result$feed$entry)
  } else {
    message("No Sentinel-2 data found for the specified criteria")
    return(NULL)
  }
}

# NEON data collection
fetch_neon_data <- function(site_code, product_code = "DP3.30006.001", 
                           date_range = NULL) {
  
  config <- load_config()
  neon_config <- config$sensor_networks$neon
  
  # NEON API endpoint for hyperspectral data
  base_url <- neon_config$api_endpoint
  
  # Build API URL
  api_url <- glue("{base_url}data/{product_code}/{site_code}")
  
  if (!is.null(date_range)) {
    api_url <- glue("{api_url}/{date_range[1]}/{date_range[2]}")
  }
  
  message("Fetching NEON hyperspectral data...")
  response <- make_api_request(api_url)
  
  result <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()
  
  if (length(result$data$files) > 0) {
    message(glue("Found {length(result$data$files)} NEON data files"))
    return(result$data$files)
  } else {
    message("No NEON data found for the specified criteria")
    return(NULL)
  }
}

# USGS Spectral Library data collection
fetch_usgs_spectral_library <- function(material_type = "soil", 
                                       wavelength_min = 400, 
                                       wavelength_max = 2500) {
  
  config <- load_config()
  usgs_config <- config$spectral_libraries$usgs
  
  # USGS spectral library search parameters
  params <- list(
    material = material_type,
    wave_min = wavelength_min,
    wave_max = wavelength_max,
    format = "json"
  )
  
  message("Searching USGS Spectral Library...")
  response <- make_api_request(
    url = usgs_config$api_endpoint,
    params = params
  )
  
  result <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON()
  
  message(glue("Found {length(result)} spectral library entries"))
  return(result)
}

# Download file with progress bar
download_file_with_progress <- function(url, dest_path, headers = NULL) {
  
  # Create directory if it doesn't exist
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  
  # Get file size for progress bar
  head_response <- httr::HEAD(url, add_headers(.headers = headers))
  file_size <- as.numeric(httr::headers(head_response)$`content-length`)
  
  if (is.na(file_size)) {
    message("Downloading file (size unknown)...")
    httr::GET(url, add_headers(.headers = headers), 
              httr::write_disk(dest_path, overwrite = TRUE))
  } else {
    # Download with progress bar
    message(glue("Downloading {basename(dest_path)} ({round(file_size/1024/1024, 1)} MB)..."))
    
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :rate :eta",
      total = file_size,
      width = 60
    )
    
    httr::GET(url, add_headers(.headers = headers),
              httr::write_disk(dest_path, overwrite = TRUE),
              httr::progress(function(down, up) pb$tick(down - pb$current)))
  }
  
  return(dest_path)
}

# Batch download function
batch_download <- function(file_list, dest_dir, max_concurrent = 3) {
  
  if (length(file_list) == 0) {
    message("No files to download")
    return(character(0))
  }
  
  downloaded_files <- character(length(file_list))
  
  message(glue("Starting batch download of {length(file_list)} files..."))
  
  for (i in seq_along(file_list)) {
    file_info <- file_list[[i]]
    dest_path <- file.path(dest_dir, basename(file_info$url))
    
    tryCatch({
      downloaded_files[i] <- download_file_with_progress(
        url = file_info$url,
        dest_path = dest_path,
        headers = file_info$headers
      )
    }, error = function(e) {
      warning(glue("Failed to download {file_info$url}: {e$message}"))
      downloaded_files[i] <- NA
    })
  }
  
  successful_downloads <- downloaded_files[!is.na(downloaded_files)]
  message(glue("Successfully downloaded {length(successful_downloads)} out of {length(file_list)} files"))
  
  return(successful_downloads)
}

# Main data collection orchestrator
collect_hyperspectral_data <- function(field_boundaries, date_range, 
                                     output_dir = "data/raw", 
                                     sources = c("neon", "sentinel2", "usgs_library")) {
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Convert field boundaries to bounding box
  if (inherits(field_boundaries, "sf")) {
    bbox <- sf::st_bbox(field_boundaries)
  } else if (is.numeric(field_boundaries) && length(field_boundaries) == 4) {
    bbox <- field_boundaries
  } else {
    stop("field_boundaries must be an sf object or numeric bounding box")
  }
  
  collected_data <- list()
  
  # Collect data from each source
  for (source in sources) {
    message(glue("\n--- Collecting data from {source} ---"))
    
    tryCatch({
      if (source == "neon") {
        # Find nearest NEON site
        # This would need site boundary lookup - simplified for scaffold
        site_code <- "HARV"  # Harvard Forest as example
        collected_data[[source]] <- fetch_neon_data(site_code, date_range = date_range)
        
      } else if (source == "sentinel2") {
        # Note: Requires authentication setup
        collected_data[[source]] <- fetch_sentinel2_data(bbox, date_range)
        
      } else if (source == "usgs_library") {
        collected_data[[source]] <- fetch_usgs_spectral_library()
      }
      
    }, error = function(e) {
      warning(glue("Failed to collect data from {source}: {e$message}"))
      collected_data[[source]] <- NULL
    })
  }
  
  # Save collection metadata
  metadata <- list(
    collection_date = Sys.time(),
    field_boundaries = bbox,
    date_range = date_range,
    sources = sources,
    results = sapply(collected_data, function(x) if(is.null(x)) 0 else length(x))
  )
  
  saveRDS(metadata, file.path(output_dir, "collection_metadata.rds"))
  saveRDS(collected_data, file.path(output_dir, "collected_data_index.rds"))
  
  message("\n--- Data Collection Summary ---")
  message(glue("Collection completed at: {Sys.time()}"))
  for (source in names(metadata$results)) {
    message(glue("{source}: {metadata$results[[source]]} items found"))
  }
  
  return(collected_data)
}

# Utility function to validate API credentials
validate_credentials <- function(service, username = NULL, password = NULL, api_key = NULL) {
  
  # Simple credential validation - extend based on service requirements
  if (service == "sentinel2" && (is.null(username) || is.null(password))) {
    return(FALSE)
  }
  
  if (service %in% c("earthdata", "usgs") && is.null(api_key)) {
    return(FALSE)
  }
  
  return(TRUE)
} 