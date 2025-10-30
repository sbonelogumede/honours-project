# ==============================================================================
# Gaussian Process Cross-Validation
# Purpose: Fit GP models (SE, PER, SExPER) with cross-validation
# ==============================================================================

# Load required packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(cmdstanr)
  library(forecast)
  library(parallel)
  library(future)
  library(future.apply)
})

# Source evaluation metrics ----------------------------------------------------
source("R/evaluation_metrics.R")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
stan_dir <- "stan"  # Directory containing .stan files

# Ensure output directories exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# HPC Configuration
# -----------------------------------------------------------------------------

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
if (n_cores == 1) {
  n_cores <- detectCores() - 1
}

cat(sprintf("Using %d cores\n", n_cores))
plan(multisession, workers = n_cores)
options(mc.cores = min(4, n_cores))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

cat("Loading transformed data...\n")
load(file.path(data_dir, "transformed_data.RData"))
n_cv <- 10

# -----------------------------------------------------------------------------
# Prepare Data: Separate Time from Covariates
# -----------------------------------------------------------------------------

cat("Preparing data...\n")

X_mean <- X_mat  # Use as-is
TIME_COLUMN_INDEX <- 2  # Time is in the second column

# Extract time column for kernel
X_time <- ts(
  data = X_mean[, TIME_COLUMN_INDEX],
  start = start(X_mean),
  frequency = frequency(X_mean)
)

# Update dimensions
p_mean <- ncol(X_mean)  # Number of predictors including intercept

cat(sprintf("Data dimensions:\n"))
cat(sprintf("  - X_mean: %d observations × %d predictors\n", 
            nrow(X_mean), p_mean))
cat(sprintf("  - X_time: %d observations × 1 (time only)\n", length(X_time)))
cat(sprintf("  - Response: %d observations\n", length(Y_vec)))
cat(sprintf("Covariate names: %s\n", paste(colnames(X_mean), collapse=", ")))

cat("Data preparation complete\n\n")

# -----------------------------------------------------------------------------
# Compile Stan Models
# -----------------------------------------------------------------------------

cat("Compiling Stan models...\n")

GP_MLR_SE_Model <- cmdstan_model(
  stan_file = file.path(stan_dir, "GP_MLR_SE.stan"),
  cpp_options = list(stan_threads = TRUE)
)

GP_MLR_PER_Model <- cmdstan_model(
  stan_file = file.path(stan_dir, "GP_MLR_PER.stan"), 
  cpp_options = list(stan_threads = TRUE)
)

GP_MLR_SExPER_Model <- cmdstan_model(
  stan_file = file.path(stan_dir, "GP_MLR_SExPER.stan"),
  cpp_options = list(stan_threads = TRUE)
)

cat("Models compiled successfully\n")

# -----------------------------------------------------------------------------
# Define CV Function for Each Fold
# -----------------------------------------------------------------------------

run_cv_fold <- function(i, model_obj, model_name, use_period = FALSE, 
                        is_product = FALSE) {
  
  # Calculate fold-specific train/test windows
  # Training data with intercept
  X1_mean_fold <- window(
    x = X_mean, 
    start = start(X1) + c(i-1, 0), 
    end = end(X1) + c(i-1, 0)
  )
  
  # Training time only
  X1_time_fold <- window(
    x = X_time, 
    start = start(X1) + c(i-1, 0), 
    end = end(X1) + c(i-1, 0)
  )
  
  # Training response
  Y1_fold <- window(
    x = Y_vec, 
    start = start(Y1) + c(i-1, 0), 
    end = end(Y1) + c(i-1, 0)
  )
  n1_fold <- length(Y1_fold)
  
  # Test data with intercept
  X2_mean_fold <- window(
    x = X_mean, 
    start = start(X2) + c(i-1, 0), 
    end = end(X2) + c(i-1, 0)
  )
  
  # Test time only
  X2_time_fold <- window(
    x = X_time, 
    start = start(X2) + c(i-1, 0), 
    end = end(X2) + c(i-1, 0)
  )
  
  # Test response
  Y2_fold <- window(
    x = Y_vec, 
    start = start(Y2) + c(i-1, 0), 
    end = end(Y2) + c(i-1, 0)
  )
  n2_fold <- length(Y2_fold)
  
  # Prepare Stan data with separated inputs
  if (use_period || is_product) {
    stan_data <- list(
      X1_mean = X1_mean_fold, 
      X1_time = as.vector(X1_time_fold),
      y1 = Y1_fold, 
      n1 = n1_fold, 
      p_mean = p_mean,
      X2_mean = X2_mean_fold,
      X2_time = as.vector(X2_time_fold),
      n2 = n2_fold, 
      T = 24  # Period for hourly data with daily cycle
    )
  } else {
    stan_data <- list(
      X1_mean = X1_mean_fold, 
      X1_time = as.vector(X1_time_fold),
      y1 = Y1_fold, 
      n1 = n1_fold, 
      p_mean = p_mean,
      X2_mean = X2_mean_fold,
      X2_time = as.vector(X2_time_fold),
      n2 = n2_fold
    )
  }
  
  # Fit model
  fit <- model_obj$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 1,
    iter_warmup = 500,
    iter_sampling = 500,
    seed = 2025 + i,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    adapt_delta = 0.95
  )
  
  # Extract ALL draws
  all_draws <- fit$draws(format = "draws_matrix")
  
  # Extract predictions for error calculation
  y2_draws <- fit$draws("y2", format = "matrix")
  y2_pred <- ts(
    data = colMeans(y2_draws), 
    start = start(Y2_fold), 
    frequency = frequency(Y2_fold)
  )
  
  # Calculate errors
  errors <- c(
    RMSE = rmse(yhat = y2_pred, yobs = Y2_fold),
    MAE = mae(yhat = y2_pred, yobs = Y2_fold),
    SMAPE = smape(yhat = y2_pred, yobs = Y2_fold)
  )
  
  # Check convergence
  diagnostics <- fit$diagnostic_summary()
  max_rhat <- max(fit$summary("lp__")$rhat, na.rm = TRUE)
  
  return(list(
    fold = i,
    model = model_name,
    errors = errors,
    max_rhat = max_rhat,
    divergences = sum(diagnostics$num_divergent),
    max_treedepth = sum(diagnostics$num_max_treedepth),
    draws = all_draws
  ))
}

# -----------------------------------------------------------------------------
# Run Cross-Validation in Parallel
# -----------------------------------------------------------------------------

cat("\n=== Running GP with SE Kernel ===\n")
SE_results <- future_lapply(
  X = 1:n_cv,
  FUN = run_cv_fold,
  model_obj = GP_MLR_SE_Model,
  model_name = "GP_MLR_SE",
  use_period = FALSE,
  is_product = FALSE,
  future.seed = TRUE
)

cat("\n=== Running GP with Periodic Kernel ===\n")
PER_results <- future_lapply(
  X = 1:n_cv,
  FUN = run_cv_fold,
  model_obj = GP_MLR_PER_Model,
  model_name = "GP_MLR_PER",
  use_period = TRUE,
  is_product = FALSE,
  future.seed = TRUE
)

cat("\n=== Running GP with SE × Periodic Product Kernel ===\n")
SExPER_results <- future_lapply(
  X = 1:n_cv,
  FUN = run_cv_fold,
  model_obj = GP_MLR_SExPER_Model,
  model_name = "GP_MLR_SExPER",
  use_period = FALSE,
  is_product = TRUE,
  future.seed = TRUE
)

# -----------------------------------------------------------------------------
# Process Results
# -----------------------------------------------------------------------------

# Extract error matrices
SE_errors <- do.call(rbind, lapply(SE_results, function(x) x$errors))
PER_errors <- do.call(rbind, lapply(PER_results, function(x) x$errors))
SExPER_errors <- do.call(rbind, lapply(SExPER_results, function(x) x$errors))

# Calculate integrated errors
SE_IR <- colMeans(SE_errors)
PER_IR <- colMeans(PER_errors)
SExPER_IR <- colMeans(SExPER_errors)

# Extract draws for each model
SE_draws <- lapply(SE_results, function(x) x$draws)
PER_draws <- lapply(PER_results, function(x) x$draws)
SExPER_draws <- lapply(SExPER_results, function(x) x$draws)

# Convergence diagnostics
SE_convergence <- data.frame(
  fold = sapply(SE_results, function(x) x$fold),
  max_rhat = sapply(SE_results, function(x) x$max_rhat),
  divergences = sapply(SE_results, function(x) x$divergences),
  max_treedepth = sapply(SE_results, function(x) x$max_treedepth)
)

PER_convergence <- data.frame(
  fold = sapply(PER_results, function(x) x$fold),
  max_rhat = sapply(PER_results, function(x) x$max_rhat),
  divergences = sapply(PER_results, function(x) x$divergences),
  max_treedepth = sapply(PER_results, function(x) x$max_treedepth)
)

SExPER_convergence <- data.frame(
  fold = sapply(SExPER_results, function(x) x$fold),
  max_rhat = sapply(SExPER_results, function(x) x$max_rhat),
  divergences = sapply(SExPER_results, function(x) x$divergences),
  max_treedepth = sapply(SExPER_results, function(x) x$max_treedepth)
)

# -----------------------------------------------------------------------------
# Display Results
# -----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("RESULTS SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nGP with SE Kernel - Integrated Errors:\n")
print(round(SE_IR, 3))

cat("\nGP with Periodic Kernel - Integrated Errors:\n")
print(round(PER_IR, 3))

cat("\nGP with SE × Periodic Product Kernel - Integrated Errors:\n")
print(round(SExPER_IR, 3))

cat("\n", rep("-", 70), "\n", sep = "")
cat("CONVERGENCE DIAGNOSTICS\n")
cat(rep("-", 70), "\n", sep = "")

cat("\nSE Kernel:\n")
print(SE_convergence)

cat("\nPeriodic Kernel:\n")
print(PER_convergence)

cat("\nSE × Periodic Product Kernel:\n")
print(SExPER_convergence)

# Flag potential issues
if (any(SE_convergence$max_rhat > 1.01)) {
  cat("\nWARNING: SE model has Rhat > 1.01 in some folds!\n")
}
if (any(PER_convergence$max_rhat > 1.01)) {
  cat("\nWARNING: Periodic model has Rhat > 1.01 in some folds!\n")
}
if (any(SExPER_convergence$max_rhat > 1.01)) {
  cat("\nWARNING: Product kernel model has Rhat > 1.01 in some folds!\n")
}

# -----------------------------------------------------------------------------
# Save Results with Draws
# -----------------------------------------------------------------------------

cat("\nSaving results...\n")

# SE Kernel
save(
  SE_IR, SE_errors, SE_convergence, SE_draws,
  file = file.path(results_dir, "GP_MLR_SE_IR.RData")
)
cat(sprintf("Saved: %s\n", file.path(results_dir, "GP_MLR_SE_IR.RData")))

# Periodic Kernel
save(
  PER_IR, PER_errors, PER_convergence, PER_draws,
  file = file.path(results_dir, "GP_MLR_PER_IR.RData")
)
cat(sprintf("Saved: %s\n", file.path(results_dir, "GP_MLR_PER_IR.RData")))

# Product Kernel
save(
  SExPER_IR, SExPER_errors, SExPER_convergence, SExPER_draws,
  file = file.path(results_dir, "GP_MLR_SExPER_IR.RData")
)
cat(sprintf("Saved: %s\n", file.path(results_dir, "GP_MLR_SExPER_IR.RData")))

cat("\n", rep("=", 70), "\n", sep = "")
cat("ALL RESULTS SAVED SUCCESSFULLY\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("\nResults saved to: %s\n", results_dir))
