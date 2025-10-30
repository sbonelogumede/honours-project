# Gaussian Processes for Time Series Modelling

## Institution

The University of Cape Town

## Project

Honours Minor Dissertation

## Topic

Gaussian Processes for Time Series Modelling

## Authors

Raphaela Azah and Sbonelo Gumede

## Supervisor

Professor Birgit Erni

## Abstract

There are many ways to model the autocorrelation structure in time series data. Capturing the correlation structure through a Gaussian Process is a non-parametric approach which is very flexible and a good approach for capturing uncertainties. For this project we use Gaussian processes for modelling time series data, simulate from Gaussian processes, reproduce existing model fits to data, and then model a local data set using Gaussian processes.

We apply GPs on air pollution forecasting and gold price modelling.

## Project Structure

```
honours-project/
├── air-quality-forecasting/    # Air pollution forecasting using GP models
│   ├── README.md               # Project-specific documentation
│   └── ...                     # Project files
│
├── gold/                       # Gold price modelling using GP models
│   ├── README.md               # Project-specific documentation
│   └── ...                     # Project files
│
├── .gitignore                  # Git ignore rules for repository
└── README.md                   # This file
```

## Getting Started

Each project directory contains its own README with specific instructions for:

- Data requirements
- Package installation
- Running the analysis
- Interpreting results

Navigate to the respective project directory for detailed documentation:

- [Air Quality Forecasting](air-quality-forecasting/README.md)
- [Gold Price Modelling](gold/README.md)

## Requirements

- R >= 4.0.0
- Required packages are listed in each project's README
- HPC access recommended for computationally intensive GP models

## License

University of Cape Town - Academic Use
