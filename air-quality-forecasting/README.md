# Air Quality Forecasting

Time series analysis and forecasting of NO₂ concentrations using hourly air quality and meteorological data from Cape Town, 2019.

## Overview

This project forecasts hourly NO₂ pollution levels using historical air quality data and meteorological covariates. The analysis compares baseline methods, SARIMA models, and advanced Gaussian Process regression to identify optimal forecasting strategies for urban air quality monitoring.

**Key approach:** 7 days training data → 1 day ahead forecasts, evaluated via 7-fold rolling origin cross-validation.

## Directory Structure

```
project/
├── raw_data/                    # Raw Excel files
├── data/                        # Processed RData files
├── output/
│   ├── figures/                 # Generated plots
│   └── results/                 # Model evaluation results
├── logs/                        # HPC job logs
├── R/                           # Reusable functions
│   ├── summary_functions.R
│   ├── plotting_functions.R
│   └── evaluation_metrics.R
├── *.stan                       # GP kernel definitions
├── *.R                          # Analysis scripts
├── *.sh                         # HPC job scripts
└── README.md
```

## Setup

```bash
Rscript install_packages.R
```

## Quick Start

**Steps must run in order (1→5 or use `run_analysis.R`). Steps 6-9 are optional.**

Run the complete analysis pipeline:

```r
source("run_analysis.R")  # Runs steps 1-5 (~5 min)
```

Or run individual steps:

```r
source("01_extract_data.R")          # Load raw Excel files, combine datasets
source("02_preprocess_explore.R")    # Add features, generate EDA plots
source("03_transform_data.R")        # Create time series objects, train/test split
source("04_simple_forecasting.R")    # Evaluate baseline models (~2 min)
source("05_sarima_models.R")         # Fit SARIMA models (~5 min)
source("06_comprehensive_diagnostics.R")  # Residual analysis, model checks
source("07_extract_components.R")    # Decompose time series (trend/seasonal/irregular)
source("08_GP_MLR_CV.R")            # Bayesian GP regression (HPC: 24-48 hrs)
source("09_plot_gp_predictions.R")   # Visualize GP forecasts (HPC: 12-24 hrs)
```

## Models

**Baseline:** Mean, Naive, Seasonal Naive, Drift

**SARIMA:** (1,0,0)(1,0,0)[24], (1,0,0)(0,0,1)[24], (2,0,0)(1,0,0)[24], (1,0,1)(1,0,0)[24]

**Advanced:** Gaussian Process regression, Multiple Linear Regression

### GP Kernels

Three covariance functions model different temporal patterns:

- **SE:** Squared Exponential - captures smooth trends (GP_MLR_SE.stan)
- **PER:** Periodic - captures daily/weekly cycles (GP_MLR_PER.stan)
- **SExPER:** SE × Periodic - captures evolving periodic patterns (GP_MLR_SExPER.stan)

## Data

Hourly measurements from Cape Town, 2019:
- **NO₂** (μg/m³) - target variable
- **PM₁₀** (μg/m³) - particulate matter covariate
- **SO₂** (μg/m³) - sulfur dioxide covariate
- **Wind Speed** (m/s) - meteorological covariate

**Size:** ~8,760 hourly observations (365 days × 24 hours)

## Evaluation

Models evaluated using **7-fold rolling origin cross-validation** (1-day forecast horizon):
- **RMSE:** Root Mean Square Error (penalizes large errors)
- **MAE:** Mean Absolute Error (average prediction error)
- **SMAPE:** Symmetric MAPE (scale-independent percentage error)

## Outputs

- **data/**: Processed RData files for each step
- **output/figures/**: Time series plots, ACF/PACF, diagnostics, forecasts
- **output/results/**: CSV files with cross-validated metrics for all models
- **logs/**: HPC job stdout/stderr files

## HPC Usage (Optional)

For running on HPC:

```bash
# Install packages
sbatch hpc_install_packages.sh

# Run main analysis (steps 1-5)
sbatch run_analysis.sh

# Run diagnostics (step 6)
sbatch 06_comprehensive_diagnostics.sh

# Extract components (step 7)
sbatch 07_extract_components.sh

# GP & MLR models (step 8)
sbatch 08_GP_MLR_CV.sh

# Plot GP predictions (step 9)
sbatch 09_plot_gp_predictions.sh

# Check logs
cat logs/*.out
cat logs/*.err
```

## Note

**Why HPC?** Steps 8-9 use Bayesian MCMC sampling for GP models (via Stan), requiring 24-48 hours with 40+ cores. Steps 1-7 run locally in ~10 minutes total.

**Dependencies:** Stan models require CmdStanR. Run `install_packages.R` first.
