# Getting Started with Hyperspectral Soil Analysis

## Quick Start

1. **Open the R project**: Double-click `HyperspectralSoilAnalysis.Rproj`

2. **Install packages**: Run the setup script
   ```r
   source("src/setup/install_packages.R")
   ```

3. **Run demonstration**: Try the system with sample data
   ```r
   source("main.R")
   demo()
   ```

4. **View results**: Check the `output/` directory for generated reports and plots

## Step-by-Step Workflow

### 1. Configuration

Edit configuration files to match your needs:

- `config/data_sources.yml`: Configure data sources and API endpoints
- `config/analysis_params.yml`: Adjust analysis parameters

### 2. Data Collection

**Option A: Automatic Collection**
```r
source("src/workflows/01_data_collection.R")
```

**Option B: Manual Data Setup**
Place your hyperspectral data files in `data/raw/`

### 3. Analysis Pipeline

Run the complete analysis:
```r
source("src/workflows/02_analysis_pipeline.R")
```

### 4. Custom Analysis

Use individual components:
```r
# Load functions
source("src/preprocessing/spectral_preprocessing.R")
source("src/analysis/soil_quality_modeling.R")
source("src/visualization/plot_functions.R")

# Your custom analysis code here
```

## Data Requirements

### Input Data Formats

- **Hyperspectral Data**: ENVI, GeoTIFF, NetCDF, HDF5, or CSV formats
- **Soil Reference Data**: CSV with soil property measurements
- **Field Boundaries**: Shapefile or coordinate pairs

### Supported Data Sources

- NEON Airborne Observation Platform
- Sentinel-2 satellite imagery (requires ESA account)
- USGS Spectral Library
- Custom uploaded datasets

## API Credentials Setup

For satellite data access, create a `.Renviron` file in your project root:

```
SENTINEL_USERNAME=your_username
SENTINEL_PASSWORD=your_password
EARTHDATA_USERNAME=your_nasa_username  
EARTHDATA_PASSWORD=your_nasa_password
```

## Common Issues

### Package Installation Problems

```r
# If you encounter package installation issues:
install.packages(c("devtools", "remotes"))

# For spatial packages on macOS:
# Install GDAL/PROJ first via Homebrew
```

### Memory Issues with Large Datasets

```r
# Increase memory limit (Windows)
memory.limit(size = 8000)  # 8GB

# Use data chunking for large files
# Edit analysis_params.yml: chunk_size: 500
```

### API Connection Problems

- Check internet connection
- Verify API credentials
- Check API endpoint status
- Review firewall/proxy settings

## Example Use Cases

### 1. Field Soil Assessment
Analyze soil properties across an agricultural field using satellite and ground-truth data.

### 2. Temporal Monitoring  
Track soil quality changes over time using multi-date imagery.

### 3. Regional Mapping
Create soil property maps for larger geographic areas.

## Output Interpretation

### Model Performance Metrics

- **R²**: Coefficient of determination (higher is better, max = 1.0)
- **RMSE**: Root mean square error (lower is better)
- **RPD**: Ratio of performance to deviation (>2.0 = excellent)
- **RPIQ**: Ratio of performance to IQ range (>2.5 = excellent)

### Quality Control Flags

- **PASS**: Data meets quality thresholds
- **FAIL**: Data requires attention or cannot be used reliably

## Next Steps

1. Review the generated reports in `output/reports/`
2. Examine quality control results
3. Adjust parameters if needed
4. Apply models to new data
5. Generate field-specific recommendations

## Getting Help

- Check the project README.md for detailed information
- Review configuration files for parameter explanations  
- Use R's help system: `?function_name`
- Check the `tests/` directory for usage examples 