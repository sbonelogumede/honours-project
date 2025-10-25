# ==============================================================================
# SARIMA Models - Cross Validation
# Purpose: Evaluate SARIMA forecasting models using rolling origin CV
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

# Load transformed data --------------------------------------------------------
load(file.path(data_dir, "transformed_data.RData"))

message("Evaluating SARIMA models with cross-validation...")

# Create ACF and PACF plots ----------------------------------------------------
message("Creating ACF and PACF plots...")

ggAcf_plot <- ggAcf(x = Y_vec) +
  ggtitle(label = NULL) +
  theme_minimal()

ggPacf_plot <- ggPacf(x = Y_vec) +
  ggtitle(label = NULL) +
  theme_minimal()

# Display plots
print(p1)
print(ggAcf_plot)
print(ggPacf_plot)

# Save plots
ggsave(
  filename = file.path(fig_dir, "train_ts.pdf"),
  plot = p1,
  width = 6.5,
  height = 4,
  dpi = 600
)

ggsave(
  filename = file.path(fig_dir, "acf_plot.pdf"),
  plot = ggAcf_plot,
  width = 6.5,
  height = 4,
  dpi = 600
)

ggsave(
  filename = file.path(fig_dir, "pacf_plot.pdf"),
  plot = ggPacf_plot,
  width = 6.5,
  height = 4,
  dpi = 600
)

# Cross-validation setup -------------------------------------------------------
n_cv <- 7  # Number of cross-validation iterations

# Storage for results
R_list <- list()

# Result matrix
R_mat <- matrix(
  data = NA,
  nrow = 3,
  ncol = 4,
  dimnames = list(
    c("IRMSE", "IMAE", "ISMAPE"),
    c(
      "SARIMA(1,0,0)(1,0,0)[24]",
      "SARIMA(1,0,0)(0,0,1)[24]",
      "SARIMA(2,0,0)(1,0,0)[24]",
      "SARIMA(1,0,1)(1,0,0)[24]"
    )
  )
)

# Cross-validation loop --------------------------------------------------------
message(sprintf("Running %d-fold rolling origin cross-validation...", n_cv))

for (i in 1:n_cv) {
  message(sprintf("  Fold %d/%d", i, n_cv))
  
  # Fit SARIMA models
  
  # SARIMA(1,0,0)(1,0,0)[24]
  sarima1_obj <- Arima(
    Y1,
    order = c(1, 0, 0),
    seasonal = list(order = c(1, 0, 0), period = 24)
  )
  sarima1_fit <- forecast(object = sarima1_obj, h = h[1] * 24)
  
  # SARIMA(1,0,0)(0,0,1)[24]
  sarima2_obj <- Arima(
    Y1,
    order = c(1, 0, 0),
    seasonal = list(order = c(0, 0, 1), period = 24)
  )
  sarima2_fit <- forecast(object = sarima2_obj, h = h[1] * 24)
  
  # SARIMA(2,0,0)(1,0,0)[24]
  sarima3_obj <- Arima(
    Y1,
    order = c(2, 0, 0),
    seasonal = list(order = c(1, 0, 0), period = 24)
  )
  sarima3_fit <- forecast(object = sarima3_obj, h = h[1] * 24)
  
  # SARIMA(1,0,1)(1,0,0)[24]
  sarima4_obj <- Arima(
    Y1,
    order = c(1, 0, 1),
    seasonal = list(order = c(1, 0, 0), period = 24)
  )
  sarima4_fit <- forecast(object = sarima4_obj, h = h[1] * 24)
  
  # Calculate RMSE
  R_mat[1, 1] <- rmse(yhat = sarima1_fit$mean, yobs = Y2)
  R_mat[1, 2] <- rmse(yhat = sarima2_fit$mean, yobs = Y2)
  R_mat[1, 3] <- rmse(yhat = sarima3_fit$mean, yobs = Y2)
  R_mat[1, 4] <- rmse(yhat = sarima4_fit$mean, yobs = Y2)
  
  # Calculate MAE
  R_mat[2, 1] <- mae(yhat = sarima1_fit$mean, yobs = Y2)
  R_mat[2, 2] <- mae(yhat = sarima2_fit$mean, yobs = Y2)
  R_mat[2, 3] <- mae(yhat = sarima3_fit$mean, yobs = Y2)
  R_mat[2, 4] <- mae(yhat = sarima4_fit$mean, yobs = Y2)
  
  # Calculate SMAPE
  R_mat[3, 1] <- smape(yhat = sarima1_fit$mean, yobs = Y2)
  R_mat[3, 2] <- smape(yhat = sarima2_fit$mean, yobs = Y2)
  R_mat[3, 3] <- smape(yhat = sarima3_fit$mean, yobs = Y2)
  R_mat[3, 4] <- smape(yhat = sarima4_fit$mean, yobs = Y2)
  
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
message("\nCross-validated results for SARIMA models:")
print(round(t(R_mat_integrated), digits = 3))

# Save results -----------------------------------------------------------------
save(
  R_mat_integrated,
  R_list,
  file = file.path(results_dir, "sarima_results.RData")
)

message("\nResults saved successfully!")
message(sprintf("  Location: %s", results_dir))
