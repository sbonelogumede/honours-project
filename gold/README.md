# Gaussian Processes for Time Series Modelling

## Institution
The University of Cape Town

## Project
Honours Minor Dissertation

## Topic
Gaussian Processes for Time Series Modelling

## Authors
Raphaela Azar and Sbonelo Gumede

## Supervisor
Professor Birgit Erni

---

## Abstract
This repository contains the code, data, and documentation supporting the Honours Minor Dissertation titled *"Gaussian Processes for Time Series Modelling"*.  
The project explores the application of Gaussian Process (GP) regression methods to financial time series, specifically for forecasting gold price movements.  
Several GP-based models are implemented and compared — including single-fit, cross-validation ensemble, and bootstrap ensemble approaches — against benchmark statistical models.  
The repository provides reproducible analysis pipelines written in **R and Quarto (QMD)**, along with supporting datasets, kernel optimization scripts, and simulation experiments.

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

---
##  Figures and Outputs

All generated figures (diagnostics, forecasts, kernel behavior, etc.) are saved in the `/code/output` directory, including:
- Residual plots and Q–Q analysis  
- Forecast comparisons and uncertainty bands  
- Individual ensemble model predictions  
- Volatility and ACF/PACF analyses

---

##  Data

**Source:** Gold ETF prices and macroeconomic variables (2011–2019).  
Data were collected from Yahoo Finance and various macroeconomic data repositories.  
For detailed feature descriptions, refer to:  
📄 `docs/ABOUT DATA (gold).docx`

---
## ⚙️ Requirements
- R (≥ 4.2.0)
- Quarto (≥ 1.3.0)
- Key R packages:  
  `tidyverse`, `kernlab`, `GPfit`, `ggplot2`, `dplyr`, `forecast`, `caret`, `readr`
---

##  Running the Project

1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/honours-project.git
   
   
   
   
Results Summary
The Bootstrap Ensemble GP model achieved the best predictive performance across metrics (RMSE, MAE, MAPE), outperforming both benchmark and single GP approaches.
Residual diagnostics indicated homoskedasticity and well-calibrated uncertainty, supporting the robustness of the ensemble GP framework for financial forecasting.



Citation
If you use this repository, please cite:

Azah, R., & Gumede, S. (2025). Gaussian Processes for Time Series Modelling. Honours Minor Dissertation, University of Cape Town.
