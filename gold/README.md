# Gaussian Process Forecasting for Gold Prices

This repository contains the full implementation and supporting documentation for the financial application component of the *Honours Thesis on Gaussian Process Regression and Uncertainty Quantification*.  
The study focuses on forecasting **daily Gold ETF prices** using various **Gaussian Process (GP)** models and comparing their performance to traditional time-series benchmarks.

---

## 🧠 Project Overview

The goal of this project is to evaluate and enhance the predictive capability of Gaussian Process regression in a financial context.  
Three GP-based approaches were developed and compared against standard benchmark forecasting models:

1. **GP-Single:** Baseline model trained on the full dataset.  
2. **Bootstrap Ensemble GP:** Ensemble of GPs trained on bootstrapped samples to improve robustness and uncertainty calibration.  
3. **Cross-Validation (CV) Ensemble GP:** Aggregation of GPs trained on cross-validation folds.

Each model was assessed on **daily gold price data (2011–2019)** using multiple error metrics (RMSE, MAE, MAPE) and visual diagnostics.  
Benchmark comparisons include Mean, Naïve, Seasonal Naïve, Drift, and AR(1) models.

---

## 📂 Repository Structure

gold/
│
├── code/
│ ├── EDA/
│ │ ├── EDA_1.qmd
│ │ └── EDA_2.qmd
│ │
│ ├── GP_Approaches/
│ │ ├── Approach1_GP_single.qmd
│ │ ├── Approach2_GP_Boot.qmd
│ │ └── Approach3_GP_CV.qmd
│ │
│ ├── Kernels/
│ │ ├── Kernels.qmd
│ │ └── kernel_optimization_methodology.qmd
│ │
│ ├── Simulations/
│ │ ├── generic_simulation.qmd
│ │ └── Sine_Simulation.qmd
│ │
│ ├── benchmark_models.qmd
│ ├── Configuration_optimisation.qmd
│ ├── .Rhistory
│ ├── .RData
│ ├── code.Rproj
│ └── output/
│ ├── ensemble_forecast.png
│ ├── residual_histogram.png
│ ├── residuals_vs_fitted.png
│ ├── pacf_plot.png
│ ├── individual_models_grid.png
│ ├── errors_over_time.png
│ ├── individual_predictions.csv
│ └── ...
│
├── data/
│ └── FINAL_USO.csv
│
└── docs/
└── ABOUT_DATA_(gold).docx