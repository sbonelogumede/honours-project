# ==============================================================================
# Simple Forecasting Models - Cross Validation
# Purpose: Evaluate baseline forecasting methods using rolling origin CV
# ==============================================================================

# Load required packages -------------------------------------------------------
library(forecast)
library(ggplot2)

# Source utility functions -----------------------------------------------------
source("R/evaluation_metrics.R")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
output_dir <- "output"
results_dir <- file.path(output_dir, "results")
fig_dir <- file.path(output_dir, "figures")

# Create directories if needed
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Load transformed data --------------------------------------------------------
load(file.path(data_dir, "transformed_data.RData"))

message("Evaluating simple forecasting models with cross-validation...")

# Cross-validation setup -------------------------------------------------------
n_cv <- 10  # Number of cross-validation iterations

# Storage for results
R_list <- list()

# Result matrix
R_mat <- matrix(
  data = NA,
  nrow = 3,
  ncol = 4,
  dimnames = list(
    c("IRMSE", "IMAE", "ISMAPE"),
    c("Mean", "Naive", "Seasonal Naive", "Drift")
  )
)

# Cross-validation loop --------------------------------------------------------
message(sprintf("Running %d-fold rolling origin cross-validation...", n_cv))

for (i in 1:n_cv) {
  message(sprintf("  Fold %d/%d", i, n_cv))
  
  # Fit simple forecasting models
  mean_fit <- meanf(y = Y1, h = h[1] * 24)
  naive_fit <- naive(y = Y1, h = h[1] * 24)
  snaive_fit <- snaive(y = Y1, h = h[1] * 24)
  drift_fit <- rwf(y = Y1, h = h[1] * 24, drift = TRUE)
  
  # Calculate RMSE
  R_mat[1, 1] <- rmse(yhat = mean_fit$mean, yobs = Y2)
  R_mat[1, 2] <- rmse(yhat = naive_fit$mean, yobs = Y2)
  R_mat[1, 3] <- rmse(yhat = snaive_fit$mean, yobs = Y2)
  R_mat[1, 4] <- rmse(yhat = drift_fit$mean, yobs = Y2)
  
  # Calculate MAE
  R_mat[2, 1] <- mae(yhat = mean_fit$mean, yobs = Y2)
  R_mat[2, 2] <- mae(yhat = naive_fit$mean, yobs = Y2)
  R_mat[2, 3] <- mae(yhat = snaive_fit$mean, yobs = Y2)
  R_mat[2, 4] <- mae(yhat = drift_fit$mean, yobs = Y2)
  
  # Calculate SMAPE
  R_mat[3, 1] <- smape(yhat = mean_fit$mean, yobs = Y2)
  R_mat[3, 2] <- smape(yhat = naive_fit$mean, yobs = Y2)
  R_mat[3, 3] <- smape(yhat = snaive_fit$mean, yobs = Y2)
  R_mat[3, 4] <- smape(yhat = drift_fit$mean, yobs = Y2)
  
  # Store results for this fold
  R_list[[i]] <- R_mat
  
  # Shift train and test sets for next iteration
  X1 <- window(
    x = X_mat,
    start = start(X1) + c(1, 0),
    end = end(X1) + c(1, 0)
  )
  Y1 <- window(
    x = Y_vec,
    start = start(Y1) + c(1, 0),
    end = end(Y1) + c(1, 0)
  )
  n1 <- nrow(X1)
  
  X2 <- window(
    x = X_mat,
    start = start(X2) + c(1, 0),
    end = end(X2) + c(1, 0)
  )
  Y2 <- window(
    x = Y_vec,
    start = start(Y2) + c(1, 0),
    end = end(Y2) + c(1, 0)
  )
}

# Integrate error estimates across folds ---------------------------------------
R_mat_integrated <- apply(
  X = simplify2array(R_list),
  MARGIN = c(1, 2),
  FUN = mean
)

# Display results --------------------------------------------------------------
message("\nCross-validated results for simple forecasting models:")
print(round(t(R_mat_integrated), digits = 3))

# Save results -----------------------------------------------------------------
save(
  R_mat_integrated,
  R_list,
  file = file.path(results_dir, "simple_forecasting_results.RData")
)

message("\nResults saved successfully!")
message(sprintf("  Location: %s", results_dir))
