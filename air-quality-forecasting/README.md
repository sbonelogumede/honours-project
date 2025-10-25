# Air Quality Forecasting Analysis

Time series analysis and forecasting of NO₂ concentrations using air quality and meteorological data from Cape Town, 2019.

## Project Structure

```
project/
├── raw_data/                          # Raw data files (not included in repo)
│   ├── 2019_NO2_CCT.xls
│   ├── 2019_PM10_CCT.xls
│   ├── 2019_SO2_CCT.xls
│   └── Wind_direction_and_speed_2019.xlsx
│
├── R/                                 # Reusable R functions
│   ├── summary_functions.R            # Summary statistics
│   ├── plotting_functions.R           # Visualization functions
│   └── evaluation_metrics.R           # Model evaluation metrics
│
├── data/                              # Processed data outputs
│   ├── extracted_data.RData
│   ├── preprocessed_data.RData
│   └── transformed_data.RData
│
├── output/                            # Analysis outputs
│   ├── figures/                       # Generated plots
│   └── results/                       # Model evaluation results
│
├── 01_extract_data.R                  # Data extraction from Excel files
├── 02_preprocess_explore.R            # Data preprocessing and EDA
├── 03_transform_data.R                # Data transformation for modeling
├── 04_simple_forecasting.R            # Baseline forecasting models
├── 05_sarima_models.R                 # SARIMA model evaluation
├── run_analysis.R                     # Master script to run full pipeline
└── README.md                          # This file
```

## Requirements

### R Version

- R >= 4.0.0

### Required Packages

```r
install.packages(c(
  "dplyr",
  "forecast",
  "GGally",
  "ggplot2",
  "ggcorrplot",
  "lubridate",
  "parallel",
  "readxl",
  "tidyr"
))
```

## Usage

### Run Complete Analysis

To run the entire analysis pipeline:

```r
source("run_analysis.R")
```

### Run Individual Steps

You can also run scripts individually:

```r
# Step 1: Extract data
source("01_extract_data.R")

# Step 2: Preprocess and explore
source("02_preprocess_explore.R")

# Step 3: Transform for modeling
source("03_transform_data.R")

# Step 4: Simple forecasting models
source("04_simple_forecasting.R")

# Step 5: SARIMA models
source("05_sarima_models.R")
```

## Analysis Pipeline

### 1. Data Extraction (`01_extract_data.R`)

- Reads raw Excel files containing air quality measurements
- Combines NO₂, PM₁₀, SO₂, and wind speed data
- Handles missing values and data quality flags
- Creates both original and scaled versions of the dataset

### 2. Data Preprocessing (`02_preprocess_explore.R`)

- Adds weekday/weekend indicators
- Generates exploratory visualizations
- Computes summary statistics
- Creates time series plots and correlation matrices

### 3. Data Transformation (`03_transform_data.R`)

- Removes incomplete observations
- Creates time series objects with hourly frequency
- Splits data into training (7 days) and test (1 day) sets
- Prepares covariates for modeling

### 4. Simple Forecasting Models (`04_simple_forecasting.R`)

Evaluates baseline methods using 7-fold rolling origin cross-validation:

- Mean forecast
- Naive forecast
- Seasonal naive forecast
- Drift forecast

### 5. SARIMA Models (`05_sarima_models.R`)

Evaluates seasonal ARIMA models:

- SARIMA(1,0,0)(1,0,0)[24]
- SARIMA(1,0,0)(0,0,1)[24]
- SARIMA(2,0,0)(1,0,0)[24]
- SARIMA(1,0,1)(1,0,0)[24]

## Evaluation Metrics

All models are evaluated using:

- **RMSE**: Root Mean Square Error
- **MAE**: Mean Absolute Error
- **SMAPE**: Symmetric Mean Absolute Percentage Error

## Outputs

### Figures

- Time series plots for each pollutant
- Weekday vs weekend comparison boxplots
- ACF and PACF plots
- Pairs plots showing correlations

### Results

- Cross-validated error metrics (CSV format)
- Model comparison tables
- Forecast visualizations

## Data

The analysis uses hourly air quality data from Cape Town, 2019:

- **NO₂**: Nitrogen dioxide concentration (μg/m³)
- **PM₁₀**: Particulate matter concentration (μg/m³)
- **SO₂**: Sulfur dioxide concentration (μg/m³)
- **Wind Speed**: Meteorological wind speed (m/s)

## Notes

- Missing values are handled using `na.omit()` for modeling
- Time series have hourly frequency (24 observations per day)
- Cross-validation uses a rolling origin approach with 1-day forecast horizon
- All functions are documented with roxygen-style comments

## Author

[Sbonelo Gumede]
