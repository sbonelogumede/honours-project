# Air Quality Forecasting Analysis

Time series analysis and forecasting of NO₂ concentrations using air quality and meteorological data from Cape Town, 2019.

## Project Structure

```
project/
├── raw_data/                          # Raw data files
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
├── hpc/                               # HPC job scripts
│   └── install_packages.sh            # Package installation job script
│
├── install_packages.R                 # Package installation script
├── 01_extract_data.R                  # Data extraction from Excel files
├── 02_preprocess_explore.R            # Data preprocessing and EDA
├── 03_transform_data.R                # Data transformation for modeling
├── 04_simple_forecasting.R            # Baseline forecasting models
├── 05_sarima_models.R                 # SARIMA model evaluation
├── 06_comprehensive_diagnostics.R     # Model diagnostics
├── 07_extract_components.R            # Component extraction
├── 08_GP_MLR_CV.R                     # GP & MLR models (run separately on HPC)
├── run_analysis.R                     # Master script (runs steps 1-5)
└── README.md                          # This file
```

## Requirements

### R Version

- R >= 4.0.0

### Required Packages

All required packages are installed automatically using `install_packages.R`.

## Setup

### Step 0: Install Packages

**First time setup:**

```bash
Rscript install_packages.R
```

This will install:
- Data manipulation: dplyr, tidyr, lubridate, readxl
- Time series: forecast
- Visualization: ggplot2, GGally, ggcorrplot, qqconf
- Parallel computing: parallel
- Bayesian modeling: cmdstanr (via remotes)

**For HPC users (optional):**

```bash
sbatch hpc/install_packages.sh
```

Check installation status:
```bash
cat install_packages.out
cat install_packages.err
```

## Usage

### Run Complete Analysis

To run the entire analysis pipeline:

```r
source("run_analysis.R")
```

This runs steps 1-5 (data extraction through SARIMA models).

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

# Step 6: Model diagnostics
source("06_comprehensive_diagnostics.R")

# Step 7: Extract components
source("07_extract_components.R")

# Step 8: GP & MLR models (run separately, requires HPC)
source("08_GP_MLR_CV.R")
```

## Analysis Pipeline

### 0. Package Installation (`install_packages.R`)

- Configures user library path
- Installs all required dependencies
- Sets up CmdStanR for Bayesian modeling

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

### 6. Comprehensive Diagnostics (`06_comprehensive_diagnostics.R`)

- Residual analysis (ACF, PACF, Ljung-Box tests)
- Normality tests
- Heteroscedasticity checks
- Diagnostic plots

### 7. Component Extraction (`07_extract_components.R`)

- Decomposes time series into trend, seasonal, and irregular components
- Analyzes component characteristics
- Visualizes decomposition

### 8. GP & MLR with Cross-Validation (`08_GP_MLR_CV.R`)

**Note: Run separately on HPC due to computational requirements**

- Fits Gaussian Process regression models
- Multiple Linear Regression with cross-validation
- Model comparison and validation

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
- Model diagnostic plots
- Component decomposition visualizations

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

## HPC Usage

The Gaussian Process models (step 8) are computationally intensive and should be run on HPC:

```bash
# Submit GP modeling job
sbatch hpc/run_gp_models.sh
```

All other steps can be run locally.

## Notes

- Missing values are handled using `na.omit()` for modeling
- Time series have hourly frequency (24 observations per day)
- Cross-validation uses a rolling origin approach with 1-day forecast horizon
- All functions are documented with roxygen-style comments
- Package installation must be completed before running any analysis scripts
- `run_analysis.R` executes steps 1-5 only; steps 6-8 are run separately

## Troubleshooting

If scripts report missing packages:

```bash
Rscript install_packages.R
```

For CmdStan installation issues:

```r
library(cmdstanr)
install_cmdstan()
cmdstan_version()
```

## Author

Sbonelo Gumede
