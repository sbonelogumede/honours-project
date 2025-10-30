# ==============================================================================
# Plot GP Model Forecasts and Fitted Values with Proper Intervals
# Purpose: 
#   - Fitted values: 95% CREDIBLE intervals for latent function f1
#   - Forecasts: 95% PREDICTION intervals for y2
# ==============================================================================

# Load required packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
figures_dir <- "output/figures"

# Ensure output directories exist
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

cat("Loading transformed data...\n")
load(file.path(data_dir, "transformed_data.RData"))

cat("Loading GP model results...\n")
load(file.path(results_dir, "GP_MLR_SE_IR.RData"))
load(file.path(results_dir, "GP_MLR_PER_IR.RData"))
load(file.path(results_dir, "GP_MLR_SExPER_IR.RData"))

n_cv <- length(SE_draws)

# -----------------------------------------------------------------------------
# Kernel Functions
# -----------------------------------------------------------------------------

# Compute SE kernel
compute_se_kernel <- function(t1, t2, alpha, rho) {
  n1 <- length(t1)
  n2 <- length(t2)
  K <- matrix(0, n1, n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      diff <- t1[i] - t2[j]
      K[i, j] <- alpha^2 * exp(-0.5 * diff^2 / rho^2)
    }
  }
  return(K)
}

# Compute periodic kernel
compute_periodic_kernel <- function(t1, t2, alpha, rho, T = 24) {
  n1 <- length(t1)
  n2 <- length(t2)
  K <- matrix(0, n1, n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      diff <- t1[i] - t2[j]
      sin_val <- sin(pi * diff / T)
      K[i, j] <- alpha^2 * exp(-2 * sin_val^2 / rho^2)
    }
  }
  return(K)
}

# Compute SE × Periodic product kernel
compute_sexper_kernel <- function(t1, t2, alpha_se, rho_se, alpha_per, rho_per, T = 24) {
  K_se <- compute_se_kernel(t1, t2, alpha_se, rho_se)
  K_per <- compute_periodic_kernel(t1, t2, alpha_per, rho_per, T)
  return(K_se * K_per)
}

# -----------------------------------------------------------------------------
# Extract Latent Function Credible Intervals (for fitted values)
# -----------------------------------------------------------------------------

extract_latent_credible_intervals <- function(draws, fold_idx, Y_vec, X_mat, 
                                              kernel_type = c("SE", "PER", "SExPER")) {
  kernel_type <- match.arg(kernel_type)
  
  cat(sprintf("  Processing fold %d (%s kernel)...\n", fold_idx, kernel_type))
  
  # Get fold-specific training window
  Y1_fold <- window(
    x = Y_vec,
    start = start(Y1) + c(fold_idx - 1, 0),
    end = end(Y1) + c(fold_idx - 1, 0)
  )
  
  X1_fold <- window(
    x = X_mat,
    start = start(Y1) + c(fold_idx - 1, 0),
    end = end(Y1) + c(fold_idx - 1, 0)
  )
  
  # Training time (normalized)
  n_obs <- length(Y1_fold)
  X1_time <- seq(0, 1, length.out = n_obs)
  y1_obs <- as.numeric(Y1_fold)
  
  # Extract hyperparameter draws
  sigma_draws <- draws[, "sigma"]
  beta_cols <- grep("^beta\\[", colnames(draws), value = TRUE)
  beta_draws <- draws[, beta_cols, drop = FALSE]
  
  n_draws <- nrow(draws)
  f1_draws <- matrix(NA, nrow = n_draws, ncol = n_obs)
  
  # For each posterior draw, compute posterior mean of f1|y1, theta
  for (iter in 1:n_draws) {
    # Extract hyperparameters for this draw
    sigma <- sigma_draws[iter]
    beta <- beta_draws[iter, ]
    
    # Compute mean function
    mu1 <- as.numeric(X1_fold %*% beta)
    
    # Compute kernel matrices based on model type
    if (kernel_type == "SE") {
      alpha <- draws[iter, "alpha"]
      rho <- draws[iter, "rho"]
      K11_latent <- compute_se_kernel(X1_time, X1_time, alpha, rho)
      
    } else if (kernel_type == "PER") {
      alpha <- draws[iter, "alpha"]
      rho <- draws[iter, "rho"]
      K11_latent <- compute_periodic_kernel(X1_time, X1_time, alpha, rho, T = 24)
      
    } else if (kernel_type == "SExPER") {
      alpha_se <- draws[iter, "alpha_se"]
      rho_se <- draws[iter, "rho_se"]
      alpha_per <- draws[iter, "alpha_per"]
      rho_per <- draws[iter, "rho_per"]
      K11_latent <- compute_sexper_kernel(X1_time, X1_time, alpha_se, rho_se, 
                                          alpha_per, rho_per, T = 24)
    }
    
    # K11 with observation noise
    K11_obs <- K11_latent + diag(sigma^2, n_obs)
    
    # Posterior mean of f1|y1, theta using GP conditioning
    # f1_mean = mu1 + K11_latent * K11_obs^{-1} * (y1 - mu1)
    y1_centered <- y1_obs - mu1
    
    # Solve K11_obs * x = y1_centered using Cholesky for numerical stability
    L <- tryCatch({
      chol(K11_obs)
    }, error = function(e) {
      # Add small jitter if Cholesky fails
      chol(K11_obs + diag(1e-6, n_obs))
    })
    
    v <- backsolve(L, forwardsolve(t(L), y1_centered))
    
    # Posterior mean
    f1_draws[iter, ] <- mu1 + K11_latent %*% v
  }
  
  # Compute credible intervals
  f1_mean <- colMeans(f1_draws)
  f1_lower <- apply(f1_draws, 2, quantile, probs = 0.025)
  f1_upper <- apply(f1_draws, 2, quantile, probs = 0.975)
  
  data.frame(
    time = X1_time,
    observed = y1_obs,
    mean = f1_mean,
    lower = f1_lower,
    upper = f1_upper
  )
}

# -----------------------------------------------------------------------------
# Extract Forecasts with Prediction Intervals (existing function)
# -----------------------------------------------------------------------------

extract_forecasts <- function(draws, fold_idx, Y_vec, X_mat) {
  # Get fold-specific test window
  Y2_fold <- window(
    x = Y_vec,
    start = start(Y2) + c(fold_idx - 1, 0),
    end = end(Y2) + c(fold_idx - 1, 0)
  )

  # Extract predictions from draws
  var_cols <- grep("^y2\\[", colnames(draws), value = TRUE)

  if (length(var_cols) == 0) {
    stop("No columns matching pattern '^y2\\[' found in draws")
  }

  var_draws <- draws[, var_cols]

  # Calculate mean and 95% prediction intervals
  var_mean <- colMeans(var_draws)
  var_lower <- apply(var_draws, 2, quantile, probs = 0.025)
  var_upper <- apply(var_draws, 2, quantile, probs = 0.975)

  # Create time index (normalized to [0, 1])
  n_obs <- length(var_mean)
  time_norm <- seq(0, 1, length.out = n_obs)

  # Return data frame
  data.frame(
    time = time_norm,
    observed = as.numeric(Y2_fold),
    mean = var_mean,
    lower = var_lower,
    upper = var_upper
  )
}

# -----------------------------------------------------------------------------
# Prepare Fitted Values Data (Credible Intervals for Latent Function)
# -----------------------------------------------------------------------------

cat("\nExtracting latent function credible intervals for fitted values...\n")

fitted_data_list <- list()

for (fold_idx in 1:n_cv) {
  cat(sprintf("Fold %d:\n", fold_idx))
  
  # SE Kernel
  se_data <- extract_latent_credible_intervals(
    SE_draws[[fold_idx]], 
    fold_idx, 
    Y_vec, 
    X_mat,
    kernel_type = "SE"
  ) %>%
    mutate(model = "GP_MLR_SE", fold = fold_idx)
  
  # Periodic Kernel
  per_data <- extract_latent_credible_intervals(
    PER_draws[[fold_idx]], 
    fold_idx, 
    Y_vec, 
    X_mat,
    kernel_type = "PER"
  ) %>%
    mutate(model = "GP_MLR_PER", fold = fold_idx)
  
  # SE × Periodic Product Kernel
  sexper_data <- extract_latent_credible_intervals(
    SExPER_draws[[fold_idx]], 
    fold_idx, 
    Y_vec, 
    X_mat,
    kernel_type = "SExPER"
  ) %>%
    mutate(model = "GP_MLR_SExPER", fold = fold_idx)
  
  fitted_data_list[[fold_idx]] <- bind_rows(se_data, per_data, sexper_data)
}

# Combine all folds
fitted_data <- bind_rows(fitted_data_list)

# Create factor for models with clean labels
fitted_data$model_label <- factor(
  fitted_data$model,
  levels = c("GP_MLR_SE", "GP_MLR_PER", "GP_MLR_SExPER"),
  labels = c("GP SE Kernel", "GP Periodic Kernel", "GP SE × Periodic Kernel")
)

# -----------------------------------------------------------------------------
# Prepare Forecast Data (Prediction Intervals)
# -----------------------------------------------------------------------------

cat("\nPreparing forecast plotting data with prediction intervals...\n")

forecast_data_list <- list()

for (fold_idx in 1:n_cv) {
  # Extract forecasts for each model
  se_data <- extract_forecasts(SE_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_SE", fold = fold_idx)

  per_data <- extract_forecasts(PER_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_PER", fold = fold_idx)

  sexper_data <- extract_forecasts(SExPER_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_SExPER", fold = fold_idx)

  forecast_data_list[[fold_idx]] <- bind_rows(se_data, per_data, sexper_data)
}

# Combine all folds
forecast_data <- bind_rows(forecast_data_list)

# Create factor for models with clean labels
forecast_data$model_label <- factor(
  forecast_data$model,
  levels = c("GP_MLR_SE", "GP_MLR_PER", "GP_MLR_SExPER"),
  labels = c("GP SE Kernel", "GP Periodic Kernel", "GP SE × Periodic Kernel")
)

# -----------------------------------------------------------------------------
# Create Plot Function
# -----------------------------------------------------------------------------

create_gp_plot <- function(plot_data, plot_title_suffix = "", interval_label = "95% Interval") {
  # Define colors for models
  model_colors <- c(
    "GP SE Kernel" = "#0072B2",
    "GP Periodic Kernel" = "#D55E00",
    "GP SE × Periodic Kernel" = "#009E73"
  )

  # Define colors for intervals (lighter versions)
  interval_colors <- c(
    "GP SE Kernel" = "#56B4E9",
    "GP Periodic Kernel" = "#E69F00",
    "GP SE × Periodic Kernel" = "#00D9A3"
  )

  # Create the plot
  p <- ggplot(plot_data, aes(x = time)) +
    # Intervals (credible or prediction)
    geom_ribbon(
      aes(ymin = lower, ymax = upper, fill = model_label),
      alpha = 0.3
    ) +
    # Mean lines
    geom_line(
      aes(y = mean, color = model_label),
      linewidth = 0.8
    ) +
    # Observed values
    geom_line(
      aes(y = observed, linetype = "Observed"),
      color = "black",
      linewidth = 0.6
    ) +
    # Facet by fold
    facet_wrap(~ fold, ncol = 2, labeller = label_both) +
    # Scales
    scale_color_manual(
      name = plot_title_suffix,
      values = model_colors
    ) +
    scale_fill_manual(
      name = interval_label,
      values = interval_colors
    ) +
    scale_linetype_manual(
      name = "",
      values = c("Observed" = "dashed"),
      labels = c("Observed")
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 0.25),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
    ) +
    # Labels
    labs(
      x = "Time",
      y = expression(NO[2]~levels)
    ) +
    # Theme
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_blank(),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "top",
      legend.box = "vertical",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 11, face = "bold")
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      fill = guide_legend(order = 2, nrow = 1),
      linetype = guide_legend(order = 3, nrow = 1)
    )

  return(p)
}

# -----------------------------------------------------------------------------
# Create and Save Fitted Values Plot (Credible Intervals)
# -----------------------------------------------------------------------------

cat("\nCreating fitted values plot with 95% credible intervals...\n")

p_fitted <- create_gp_plot(
  fitted_data, 
  "Fitted Values", 
  "95% Credible Interval"
)

cat("Saving fitted values plot...\n")

ggsave(
  filename = file.path(figures_dir, "gp_fitted_values_all_folds.pdf"),
  plot = p_fitted,
  width = 10,
  height = 14,
  units = "in",
  dpi = 300
)

cat("\n", rep("=", 70), "\n", sep = "")
cat("FITTED VALUES PLOT SAVED SUCCESSFULLY\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("\nPDF: %s\n", file.path(figures_dir, "gp_fitted_values_all_folds.pdf")))

# -----------------------------------------------------------------------------
# Create and Save Forecast Plot (Prediction Intervals)
# -----------------------------------------------------------------------------

cat("\nCreating forecast plot with 95% prediction intervals...\n")

p_forecast <- create_gp_plot(
  forecast_data, 
  "Forecasts", 
  "95% Prediction Interval"
)

cat("Saving forecast plot...\n")

ggsave(
  filename = file.path(figures_dir, "gp_forecasts_all_folds.pdf"),
  plot = p_forecast,
  width = 10,
  height = 14,
  units = "in",
  dpi = 300
)

cat("\n", rep("=", 70), "\n", sep = "")
cat("FORECAST PLOT SAVED SUCCESSFULLY\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("\nPDF: %s\n", file.path(figures_dir, "gp_forecasts_all_folds.pdf")))

# -----------------------------------------------------------------------------
# Display Summary Statistics - FITTED VALUES (Credible Intervals)
# -----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("FITTED VALUES SUMMARY STATISTICS (Credible Intervals)\n")
cat(rep("=", 70), "\n", sep = "")

# Calculate coverage of credible intervals
fitted_coverage_stats <- fitted_data %>%
  group_by(model_label, fold) %>%
  summarize(
    coverage = mean(observed >= lower & observed <= upper) * 100,
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_coverage = mean(coverage),
    sd_coverage = sd(coverage),
    .groups = "drop"
  )

cat("\n95% Credible Interval Coverage (%):\n")
print(fitted_coverage_stats, n = Inf)

# Calculate average credible interval width
fitted_interval_width <- fitted_data %>%
  group_by(model_label, fold) %>%
  summarize(
    avg_width = mean(upper - lower),
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_width = mean(avg_width),
    sd_width = sd(avg_width),
    .groups = "drop"
  )

cat("\nAverage Credible Interval Width:\n")
print(fitted_interval_width, n = Inf)

# -----------------------------------------------------------------------------
# Display Summary Statistics - FORECASTS (Prediction Intervals)
# -----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("FORECAST SUMMARY STATISTICS (Prediction Intervals)\n")
cat(rep("=", 70), "\n", sep = "")

# Calculate coverage of prediction intervals
forecast_coverage_stats <- forecast_data %>%
  group_by(model_label, fold) %>%
  summarize(
    coverage = mean(observed >= lower & observed <= upper) * 100,
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_coverage = mean(coverage),
    sd_coverage = sd(coverage),
    .groups = "drop"
  )

cat("\n95% Prediction Interval Coverage (%):\n")
print(forecast_coverage_stats, n = Inf)

# Calculate average prediction interval width
forecast_interval_width <- forecast_data %>%
  group_by(model_label, fold) %>%
  summarize(
    avg_width = mean(upper - lower),
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_width = mean(avg_width),
    sd_width = sd(avg_width),
    .groups = "drop"
  )

cat("\nAverage Prediction Interval Width:\n")
print(forecast_interval_width, n = Inf)

cat("\n", rep("=", 70), "\n", sep = "")
cat("ALL PLOTS AND STATISTICS COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")
